#include <cassert>
#include <algorithm>
#include <utility>
#include <map>
#include <string>
#include "grammar_symbols.h"
#include "parse.tab.h"
#include "node.h"
#include "ast.h"
#include "exceptions.h"
#include "semantic_analysis.h"
#include "type.h"
#include "symtab.h"

SemanticAnalysis::SemanticAnalysis()
  : m_global_symtab(new SymbolTable(nullptr)) {
  m_cur_symtab = m_global_symtab;
  m_cur_function = nullptr;
}

SemanticAnalysis::~SemanticAnalysis() {
  while (m_cur_symtab != nullptr) {
    SymbolTable *next = m_cur_symtab->get_parent();
    delete m_cur_symtab;
    m_cur_symtab = next;
  }
}

// Enter new scope, with new symbol table
// Set old symbol table as parent
void SemanticAnalysis::enter_scope() {
  SymbolTable *scope = new SymbolTable(m_cur_symtab);
  m_cur_symtab = scope;
}

// Leave current symbol table scope
// Transfer to parent symbol table
void SemanticAnalysis::leave_scope() {
  SymbolTable *old = m_cur_symtab;
  m_cur_symtab = m_cur_symtab->get_parent();
  delete old;
  assert(m_cur_symtab != nullptr);
}

// Promote node type to new type (and signedness)
// Must be integral type
// Adds new implicit conversion node
Node *SemanticAnalysis::promote_type(Node *n, BasicTypeKind new_type, bool is_signed) {
  assert(n->get_type()->is_integral());
  std::shared_ptr<Type> type(new BasicType(new_type, is_signed));
  return implicit_conversion(n, type);
}

// Adds new implicit conversion node with previous node as child
// Used for type promotion
Node *SemanticAnalysis::implicit_conversion(Node *n, const std::shared_ptr<Type> &type) {
  std::unique_ptr<Node> conversion(new Node(AST_IMPLICIT_CONVERSION, {n}));
  conversion->set_type(type);
  return conversion.release();
}

/**
 * Visits a struct type node. Annotate node with processed type.
 **/
void SemanticAnalysis::visit_struct_type(Node *n) {
  bool is_const = false;
  bool is_volatile = false;
  bool type_set = false;
  std::shared_ptr<Type> struct_type;

  // Process type
  for (auto i = n->cbegin(); i != n->cend(); i++) {
    Node *type_child = *i;
    int tag = type_child->get_tag();
    switch (tag) {
      case TOK_CONST:
        if (is_const) SemanticError::raise(n->get_loc(), "Malformed struct type");
        is_const = true;
        break;
      case TOK_VOLATILE:
        if (is_volatile) SemanticError::raise(n->get_loc(), "Malformed struct type");
        is_volatile = true;
        break;
      case TOK_IDENT:
        {
          if (type_set) SemanticError::raise(n->get_loc(), "Malformed struct type");
          std::shared_ptr<Symbol> struct_sym = m_cur_symtab->lookup_recursive("struct " + type_child->get_str());
          if (struct_sym == nullptr) SemanticError::raise(n->get_loc(), "Unknown struct type");
          struct_type = struct_sym->get_type();
          type_set = true;
        }
        break;
      default:
        SemanticError::raise(n->get_loc(), "Malformed struct type");
    }
  }
  assert(type_set);
  // Create QualifiedType if necessary
  if (is_const) {
    struct_type = std::shared_ptr<Type>(new QualifiedType(struct_type, TypeQualifier::CONST));
  } else if (is_volatile) {
    struct_type = std::shared_ptr<Type>(new QualifiedType(struct_type, TypeQualifier::VOLATILE));
  }
  // Annotate node with type
  n->set_type(struct_type);
}

/**
 * Visit union type. Currently unsupported.
 **/
void SemanticAnalysis::visit_union_type(Node *n) {
  RuntimeError::raise("union types aren't supported");
}

/**
 * Recursively processes a (possibly chained) declarator for a variable declaration.
 * Annotates var ident node with name of var and type.
 * Adds annotated node to vector of newly declared variables.
 **/
void SemanticAnalysis::process_declarator(std::vector<Node *> &vars, Node *declarator, const std::shared_ptr<Type> &base_type) {
  std::shared_ptr<Type> new_base_type;
  int tag = declarator->get_tag();
  switch (tag) {
    case AST_ARRAY_DECLARATOR:
      {
        int length = stoi(declarator->get_kid(1)->get_str());
        new_base_type = std::shared_ptr<Type>(new ArrayType(base_type, length));
        process_declarator(vars, declarator->get_kid(0), new_base_type);
      }
      break;
    case AST_POINTER_DECLARATOR:
      new_base_type = std::shared_ptr<Type>(new PointerType(base_type));
      process_declarator(vars, declarator->get_kid(0), new_base_type);
      break;
    case AST_NAMED_DECLARATOR:
      { 
        declarator->get_kid(0)->set_type(base_type);
        vars.push_back(declarator);
      }
      break;
    default:
      SemanticError::raise(declarator->get_loc(), "Unrecognized declarator");
      break;
  }
}

/**
 * Visits variable declaration node.
 * Processes base type and declarators.
 **/
void SemanticAnalysis::visit_variable_declaration(Node *n) {
  // child 0 is storage (TODO?)

  // Visit base type
  visit(n->get_kid(1));
  std::shared_ptr<Type> base_type = n->get_kid(1)->get_type();

  std::vector<Node *> vars;
  Node *decl_list = n->get_kid(2);

  // Process declarators
  for (auto i = decl_list->cbegin(); i != decl_list->cend(); i++) {
    Node *declarator = *i;
    process_declarator(vars, declarator, base_type);
  }
  // Add vars to symbol table
  add_vars_to_sym_table(vars);
}

/**
 * Visits basic type node, processing the full type (and qualifiers if present).
 **/
void SemanticAnalysis::visit_basic_type(Node *n) {
  bool type_set = false;
  bool is_signed = true;
  bool sign_set = false;
  bool is_const = false; 
  bool is_volatile = false;
  BasicTypeKind type;

  // Analyze basic type and type qualifer keywords
  for (auto i = n->cbegin(); i != n->cend(); i++) {
    Node *type_child = *i;
    int tag = type_child->get_tag();
    switch (tag) {
      case TOK_CONST:
        if (is_const || (type_set && type == BasicTypeKind::VOID)) SemanticError::raise(n->get_loc(), "Malformed basic type");
        is_const = true;
        break;
      case TOK_VOLATILE:
        if (is_volatile || (type_set && type == BasicTypeKind::VOID)) SemanticError::raise(n->get_loc(), "Malformed basic type");
        is_volatile = true;
        break;
      case TOK_UNSIGNED:
        if (sign_set || (type_set && type == BasicTypeKind::VOID)) SemanticError::raise(n->get_loc(), "Malformed basic type");
        is_signed = false;
        sign_set = true;
        break;
      case TOK_SIGNED:
        if (sign_set || (type_set && type == BasicTypeKind::VOID)) SemanticError::raise(n->get_loc(), "Malformed basic type");
        is_signed = true;
        sign_set = true;
        break;
      case TOK_VOID:
        if (is_volatile || is_const || sign_set || type_set) SemanticError::raise(n->get_loc(), "Malformed basic type");
        type = BasicTypeKind::VOID;
        type_set = true;
        break;
      case TOK_INT:
        if (type_set && (type == BasicTypeKind::SHORT || type == BasicTypeKind::LONG)) break;
        if (type_set) SemanticError::raise(n->get_loc(), "Malformed basic type");
        type = BasicTypeKind::INT;
        type_set = true;
        break;
      case TOK_CHAR:
        if (type_set) SemanticError::raise(n->get_loc(), "Malformed basic type");
        type = BasicTypeKind::CHAR;
        type_set = true;
        break;
      case TOK_LONG: 
        if (type_set && type == BasicTypeKind::INT) {
          type = BasicTypeKind::LONG;
        } else if (type_set) {
          SemanticError::raise(n->get_loc(), "Malformed basic type");
        } else {
          type = BasicTypeKind::LONG;
          type_set = true;
        }
        break;
      case TOK_SHORT: 
        if (type_set && type == BasicTypeKind::INT) {
          type = BasicTypeKind::SHORT;
        } else if (type_set) {
          SemanticError::raise(n->get_loc(), "Malformed basic type");
        } else {
          type = BasicTypeKind::SHORT;
          type_set = true;
        }
        break;
      default:
        SemanticError::raise(n->get_loc(), "Malformed basic type");
    }
  }
  // Int is default type
  if (!type_set) type = BasicTypeKind::INT;
  // Create BasicType
  std::shared_ptr<Type> basic_type = std::shared_ptr<Type>(new BasicType(type, is_signed));
  // Create QualifiedType if necessary
  if (is_const) {
    basic_type = std::shared_ptr<Type>(new QualifiedType(basic_type, TypeQualifier::CONST));
  } else if (is_volatile) {
    basic_type = std::shared_ptr<Type>(new QualifiedType(basic_type, TypeQualifier::VOLATILE));
  }
  // Annotate node with type
  n->set_type(basic_type);
}

/** 
 * Adds vector of annotated var nodes to current symbol table.
 **/
void SemanticAnalysis::add_vars_to_sym_table(std::vector<Node *> &vars) {
  for (auto i = vars.cbegin(); i != vars.cend(); i++) {
    Node *var_parent = *i;
    Node *var = var_parent->get_kid(0);
    if (m_cur_symtab->has_symbol_local(var->get_str())) SemanticError::raise(var->get_loc(), "Name already defined");
    std::shared_ptr<Symbol> sym = m_cur_symtab->define(SymbolKind::VARIABLE, var->get_str(), var->get_type());
    var_parent->set_symbol(sym);
  }
}

/**
 * Processes a functions parameters, processing each type, 
 * and adding them as members to the function.
 **/
void SemanticAnalysis::process_function_parameters(Node *parameter_list, std::vector<Node *> &declared_parameters, std::shared_ptr<Type> &fn_type) {
  // Process function parameter types
  for (auto i = parameter_list->cbegin(); i != parameter_list->cend(); i++) {
    Node *parameter = *i;
    // Visit base type
    visit(parameter->get_kid(0));
    std::shared_ptr<Type> base_type = parameter->get_kid(0)->get_type();
    // Process declarators
    process_declarator(declared_parameters, parameter->get_kid(1), base_type);
  }

  // Add parameters as members to function type
  for (auto i = declared_parameters.cbegin(); i != declared_parameters.cend(); i++) {
    Node *parameter = *i;
    fn_type->add_member(Member(parameter->get_kid(0)->get_str(), parameter->get_kid(0)->get_type()));
  }
}

/**
 * Visits return expression, checking that the type is compatible 
 * with its function return type.
 **/
void SemanticAnalysis::visit_return_expression_statement(Node *n) {
  const Location &loc = n->get_loc();
  std::shared_ptr<Type> fn_return_type = m_cur_function->get_base_type();
  
  // Check for void function
  if (n->get_num_kids() == 0 && m_cur_function->get_base_type()->get_basic_type_kind() == BasicTypeKind::VOID) {
    return;
  } else if (n->get_num_kids() == 0) {
    SemanticError::raise(loc, "Mismatch in function return type");
  }

  // Visit return expression
  visit(n->get_kid(0));
  // Get expression type
  std::shared_ptr<Type> return_type = n->get_kid(0)->get_type();
  // Check if return type can be casted/assigned to function return type
  check_assignment(fn_return_type, return_type, loc);

  // Promote type if needed
  if (return_type->is_integral() && fn_return_type->is_integral() && !return_type->is_same(fn_return_type.get())) {
    n->set_kid(0, promote_type(n->get_kid(0), fn_return_type->get_basic_type_kind(), fn_return_type->is_signed()));
  }
}

/**
 * Visits function definition, processing parameters, defining function, 
 * defining parameters, and visiting function body.
 **/ 
void SemanticAnalysis::visit_function_definition(Node *n) {
  // Visit return type
  visit(n->get_kid(0));
  // Function name
  const std::string &fn_name = n->get_kid(1)->get_str();
  // Create function type
  std::shared_ptr<Type> fn_type(new FunctionType(n->get_kid(0)->get_type()));

  std::vector<Node *> declared_parameters;
  // Process parameters
  process_function_parameters(n->get_kid(2), declared_parameters, fn_type);  
  // Define function
  if (m_cur_symtab->has_symbol_local(fn_name)) SemanticError::raise(n->get_loc(), "Function name already in use");
  std::shared_ptr<Symbol> sym = m_cur_symtab->define(SymbolKind::FUNCTION, fn_name, fn_type);
  n->set_symbol(sym);

  // Define parameters (since this is a function definition, not declaration)
  enter_scope();
  add_vars_to_sym_table(declared_parameters);

  // Track function to check return type
  m_cur_function = fn_type;
  // Visit function body
  visit(n->get_kid(3));
  m_cur_function = nullptr;
  
  leave_scope();
}

/**
 * Visits function declaration, processing parameters, declaring function.
 **/ 
void SemanticAnalysis::visit_function_declaration(Node *n) {
  // Visit return type
  visit(n->get_kid(0));
  // Function name
  const std::string &fn_name = n->get_kid(1)->get_str();
  // Create function type
  std::shared_ptr<Type> fn_type(new FunctionType(n->get_kid(0)->get_type()));

  std::vector<Node *> declared_parameters;
  // Process parameters
  process_function_parameters(n->get_kid(2), declared_parameters, fn_type);  
  // Define function
  if (m_cur_symtab->has_symbol_local(fn_name)) SemanticError::raise(n->get_loc(), "Function name already in use");
  m_cur_symtab->declare(SymbolKind::FUNCTION, fn_name, fn_type);
}

// NOTE: this is currently unused due to process_function_parameters bypassing it
void SemanticAnalysis::visit_function_parameter(Node *n) { }

/**
 * Enter new scope and visit each child in a statement list.
 **/
void SemanticAnalysis::visit_statement_list(Node *n) {
  enter_scope();
  for (auto i = n->cbegin(); i != n->cend(); i++) {
    Node *child = *i;
    visit(child);
  }
  leave_scope();
}

/**
 * Visits struct type definition. Processes all struct fields.
 **/
void SemanticAnalysis::visit_struct_type_definition(Node *n) {
  // Create and define struct type
  const std::string &struct_name = n->get_kid(0)->get_str();
  std::shared_ptr<Type> struct_type(new StructType(struct_name));
  if (m_cur_symtab->has_symbol_local("struct " + struct_name)) SemanticError::raise(n->get_loc(), "Struct name already defined");
  std::shared_ptr<Symbol> sym = m_cur_symtab->define(SymbolKind::TYPE, "struct " + struct_name, struct_type);
  n->set_symbol(sym);

  Node *field_list = n->get_kid(1);
  std::vector<Node *> declared_fields;

  // Process fields
  for (auto i = field_list->cbegin(); i != field_list->cend(); i++) {
    Node *field = *i;  
    // Visit base type
    visit(field->get_kid(1));
    std::shared_ptr<Type> base_type = field->get_kid(1)->get_type();
    
    // Process declarators
    Node *decl_list = field->get_kid(2);
    for (auto i = decl_list->cbegin(); i != decl_list->cend(); i++) {
      Node *declarator = *i;
      process_declarator(declared_fields, declarator, base_type);
    }
  }

  // Add declared fields to symbol table
  enter_scope();
  add_vars_to_sym_table(declared_fields);
  leave_scope();

  // Add fields as members
  unsigned offset = 0;
  int index = 0;
  for (auto i = declared_fields.cbegin(); i != declared_fields.cend(); i++) {
    Node *field = *i;
    struct_type->add_member(Member(field->get_kid(0)->get_str(), field->get_type(), offset));
    index++;
    offset += field->get_type()->get_storage_size();
  }
}

// Checks if an operator is relational or logical
bool SemanticAnalysis::is_relational_or_logical_op(int tag) {
  switch (tag) {
    case TOK_LT:
    case TOK_LTE:
    case TOK_GT:
    case TOK_GTE:
    case TOK_EQUALITY:
    case TOK_INEQUALITY:
    case TOK_LOGICAL_OR:
    case TOK_LOGICAL_AND:
      return true;
    default:
      return false;
  }
}

// Checks if a node is a pointer dereference
bool SemanticAnalysis::is_pointer_dereference(Node *n) {
  return n->get_kid(0)->get_tag() == TOK_ASTERISK;
}

/**
 * Checks if a node is an l-value, which is one of the following:
 * a reference to a variable
 * an array subscript reference
 * a pointer dereference
 * a reference to a struct instance
 * a reference to a field of a struct instance
 **/
bool SemanticAnalysis::is_lvalue(Node *n) {
  int tag = n->get_tag();
  switch (tag) {
    case AST_VARIABLE_REF:
      return (!n->get_type()->is_array());
    case AST_ARRAY_ELEMENT_REF_EXPRESSION:
    case AST_FIELD_REF_EXPRESSION:
    case AST_INDIRECT_FIELD_REF_EXPRESSION:
      return true;
    case AST_UNARY_EXPRESSION:
      return is_pointer_dereference(n);
    default:
      return false;
  }
}

/**
 * Checks if the right type can be legally assigned to the left type.
 **/
void SemanticAnalysis::check_assignment(const std::shared_ptr<Type> &left, const std::shared_ptr<Type> &right, const Location &loc) {
  // Check for assignment to const lvalue
  if (left->is_const() && !left->is_pointer() && !left->is_array()) SemanticError::raise(loc, "Assignment to const l-value");

  // Integer assignment
  if (left->is_integral() && right->is_integral()) {
    return;
  } 
  // Pointer assignment
  else if ((left->is_pointer() || left->is_array()) && (right->is_pointer() || right->is_array())) {
    std::shared_ptr<Type> left_base = left->get_base_type();
    std::shared_ptr<Type> right_base = right->get_base_type();

    if (!left_base->get_unqualified_type()->is_same(right_base->get_unqualified_type())) { 
      SemanticError::raise(loc, "Mismatch in pointer types");
    }
    if ((right_base->is_const() && !left_base->is_const()) || (right_base->is_volatile() && !left_base->is_volatile())) SemanticError::raise(loc, "Mismatch in qualifers");
  } 
  // Struct assignment
  else if (left->is_struct() && right->is_struct()) {
    if (!left->is_same(right.get())) SemanticError::raise(loc, "Mismatch in struct types");
  } 
  // Malformed assignment
  else {
    SemanticError::raise(loc, "Malformed assignment types");
  }
}

/**
 * Processes an assignment binary expression.
 * Promotes right side if necessary.
 **/
void SemanticAnalysis::process_assignment(Node *n) {
  std::shared_ptr<Type> left = n->get_kid(1)->get_type();
  std::shared_ptr<Type> right = n->get_kid(2)->get_type();

  // Check that left type is an lvalue
  if (!is_lvalue(n->get_kid(1))) SemanticError::raise(n->get_loc(), "Assignment to non l-value");

  // Check for legal assignment
  const Location &loc = n->get_loc();
  check_assignment(left, right, loc);
  // Check for promotion
  if (left->is_integral() && right->is_integral() && !left->is_same(right.get())) {
    n->set_kid(2, promote_type(n->get_kid(2), left->get_basic_type_kind(), left->is_signed()));
  } 
  // Annotate node
  n->set_type(left);
}

/**
 * Processes a non-assignment binary expression.
 * Promotes types when necessary.
 **/
void SemanticAnalysis::process_non_assignment(Node *n) {
  std::shared_ptr<Type> left = n->get_kid(1)->get_type();
  std::shared_ptr<Type> right = n->get_kid(2)->get_type();

  if (left->is_pointer() && right->is_pointer()) SemanticError::raise(n->get_loc(), "Arithmetic on two pointers is illegal");

  // Check for pointer arithmetic
  if (left->is_pointer() || right->is_pointer()) {
    if (left->is_pointer() && right->is_integral()) {
      n->set_type(left);
      return;
    } else if (right->is_pointer() && left->is_integral()) {
      n->set_type(right);
      return;
    } else {
      SemanticError::raise(n->get_loc(), "Malformed pointer arithmetic");
    }
  }

  // Check for integral types
  if (!left->is_integral() || !right->is_integral()) SemanticError::raise(n->get_loc(), "Illegal binary expression");

  bool left_promoted_is_signed = (left->is_signed() != right->is_signed()) && left->is_signed() ? false : left->is_signed();
  bool right_promoted_is_signed = (left->is_signed() != right->is_signed()) && right->is_signed() ? false : right->is_signed();

  // Promote left and right to int (and possibly signs)
  if (left->get_basic_type_kind() < BasicTypeKind::INT && right->get_basic_type_kind() < BasicTypeKind::INT) {
    n->set_kid(1, promote_type(n->get_kid(1), BasicTypeKind::INT, left_promoted_is_signed));
    n->set_kid(2, promote_type(n->get_kid(2), BasicTypeKind::INT, right_promoted_is_signed));
  } 
  // Promote left to right
  else if (left->get_basic_type_kind() < BasicTypeKind::INT || left->get_basic_type_kind() < right->get_basic_type_kind()) {
    BasicTypeKind promotion_type = right->get_basic_type_kind();
    n->set_kid(1, promote_type(n->get_kid(1), promotion_type, left_promoted_is_signed));
    if (right_promoted_is_signed != right->is_signed()) n->set_kid(2, promote_type(n->get_kid(2), promotion_type, right_promoted_is_signed));
  } 
  // Promote right to left
  else if (right->get_basic_type_kind() < BasicTypeKind::INT || right->get_basic_type_kind() < left->get_basic_type_kind()) {
    BasicTypeKind promotion_type = left->get_basic_type_kind();
    n->set_kid(2, promote_type(n->get_kid(2), promotion_type, right_promoted_is_signed));
    if (left_promoted_is_signed != left->is_signed()) n->set_kid(1, promote_type(n->get_kid(1), promotion_type, left_promoted_is_signed));
  } 
  // Promote only signs
  else if (left->is_signed() != left_promoted_is_signed || right->is_signed() != right_promoted_is_signed) {
    if (left_promoted_is_signed != left->is_signed()) n->set_kid(1, promote_type(n->get_kid(1), left->get_basic_type_kind(), left_promoted_is_signed));
    if (right_promoted_is_signed != right->is_signed()) n->set_kid(2, promote_type(n->get_kid(2), right->get_basic_type_kind(), right_promoted_is_signed));
  }

  // Annotate node with return type
  int operator_tag = n->get_kid(0)->get_tag();
  std::shared_ptr<Type> return_type;
  // Relational or logical operators return an int (signed??? TODO)
  if (is_relational_or_logical_op(operator_tag)) {
    return_type = std::shared_ptr<Type>(new BasicType(BasicTypeKind::INT, true));
  } else {
    // Left and right type are equivalent, doesn't matter which we get 
    return_type = n->get_kid(1)->get_type();
  }
  n->set_type(return_type);
}

/**
 * Visits and processes a binary expression.
 **/
void SemanticAnalysis::visit_binary_expression(Node *n) {
  int operator_tag = n->get_kid(0)->get_tag();
  // Visit left child 
  visit(n->get_kid(1));
  // Visit right child
  visit(n->get_kid(2));

  if (operator_tag == TOK_ASSIGN) {
    process_assignment(n);
  } else {
    process_non_assignment(n);
  }
}

/**
 * Visits and processes a unary expression.
 **/
void SemanticAnalysis::visit_unary_expression(Node *n) {
  // Visit operand
  visit(n->get_kid(1));
  // Get operand type
  std::shared_ptr<Type> operand_type = n->get_kid(1)->get_type();
  // Check operator
  int tag = n->get_kid(0)->get_tag();
  switch (tag) {
    case TOK_ASTERISK:
      if (!is_lvalue(n->get_kid(1))) SemanticError::raise(n->get_loc(), "Cannot dereference non-lvalue");
      if (!operand_type->is_pointer()) SemanticError::raise(n->get_loc(), "Cannot dereference non-pointer");
      n->set_type(operand_type->get_base_type());
      return;
    case TOK_AMPERSAND:
      {
        if (!is_lvalue(n->get_kid(1))) SemanticError::raise(n->get_loc(), "Cannot get address of non-lvalue");
        // Note that var had address taken
        // TODO:ADDED
        if (n->get_kid(1)->get_tag() == AST_VARIABLE_REF) {
          n->get_kid(1)->get_symbol()->set_address_taken();
        }
        std::shared_ptr<Type> new_type(new PointerType(operand_type));
        n->set_type(new_type);
      }  
      return;
    case TOK_MINUS:
      if (!operand_type->is_integral()) SemanticError::raise(n->get_loc(), "Cannot apply minus to non-integral type");
      if (operand_type->get_basic_type_kind() < BasicTypeKind::INT) {
        n->set_kid(1, promote_type(n->get_kid(1), BasicTypeKind::INT, true));
      } else if (!operand_type->is_signed()) {
        n->set_kid(1, promote_type(n->get_kid(1), operand_type->get_basic_type_kind(), true));
      }
      n->set_type(n->get_kid(1)->get_type());
      return;
    case TOK_NOT:
      if (!operand_type->is_integral()) SemanticError::raise(n->get_loc(), "Cannot apply logical-not to non-integral type");
      if (operand_type->get_basic_type_kind() < BasicTypeKind::INT) {
        n->set_kid(1, promote_type(n->get_kid(1), BasicTypeKind::INT, operand_type->is_signed()));
      } 
      n->set_type(std::shared_ptr<Type>(new BasicType(BasicTypeKind::INT, true)));
      return;
    default:
      SemanticError::raise(n->get_loc(), "Unrecognized unary expression");
  }
}

/* UNUSED. */
void SemanticAnalysis::visit_postfix_expression(Node *n) {}

/* UNUSED. */
void SemanticAnalysis::visit_conditional_expression(Node *n) {}

/* UNUSED. */
void SemanticAnalysis::visit_cast_expression(Node *n) {}

/**
 * Visits and processes a function call.
 * Checks for a valid function name, matching number of parameters,
 * and that types of parameters/arguments are legal.
 **/
void SemanticAnalysis::visit_function_call_expression(Node *n) {
  const std::string &fn_name = n->get_kid(0)->get_kid(0)->get_str();
  Node *arg_list = n->get_kid(1);
  std::shared_ptr<Symbol> fn_sym = m_cur_symtab->lookup_recursive(fn_name);
  std::shared_ptr<Type> fn_type = fn_sym->get_type();

  // Check for matching number of parameters
  if (fn_type->get_num_members() != arg_list->get_num_kids()) SemanticError::raise(n->get_loc(), "Bad number of parameters for function call");

  // Check each parameter
  for (unsigned i = 0; i != arg_list->get_num_kids(); i++) {
    Node *parameter = arg_list->get_kid(i);
    visit(parameter);
    std::shared_ptr<Type> right_type = parameter->get_type();
    std::shared_ptr<Type> left_type = fn_type->get_member(i).get_type();
    const Location &loc = parameter->get_loc();
    check_assignment(left_type, right_type, loc);

    // Check for promotion
    if (left_type->is_integral() && right_type->is_integral() && !left_type->is_same(right_type.get())) {
      arg_list->set_kid(i, promote_type(parameter, left_type->get_basic_type_kind(), left_type->is_signed()));
    } 
    // Parameter will already be annotated
  }
  // Annotate function call
  n->set_type(fn_type->get_base_type());
  n->set_fn_type(fn_type);
}

/**
 * Visits struct field reference.
 **/
void SemanticAnalysis::visit_field_ref_expression(Node *n) {
  // Visit variable
  visit(n->get_kid(0));
  // Struct field reference
  const std::string &field = n->get_kid(1)->get_str();
  std::shared_ptr<Type> struct_type = n->get_kid(0)->get_type();
  // Check if type is actually a struct
  if (!struct_type->is_struct()) SemanticError::raise(n->get_loc(), "Cannot reference non-struct with .");

  // Check for matching field name
  for (unsigned i = 0; i < struct_type->get_num_members(); i++) {
    const Member &mem = struct_type->get_member(i);
    if (mem.get_name() == field) {
      n->set_type(mem.get_type());
      return;
    }
  }
  SemanticError::raise(n->get_loc(), "Field does not exist in struct");
}

/**
 * Visits indirect struct field reference (e.g. use of ->).
 **/
void SemanticAnalysis::visit_indirect_field_ref_expression(Node *n) {
  // Visit variable
  visit(n->get_kid(0));
  // Struct field reference
  const std::string &field = n->get_kid(1)->get_str();
  std::shared_ptr<Type> struct_pointer_type = n->get_kid(0)->get_type();
  std::shared_ptr<Type> struct_type = struct_pointer_type->get_base_type();
  // Check if type is actually a pointer to a struct
  // TODO: allow arrays as well?
  if (!(struct_pointer_type->is_pointer() && struct_type->is_struct())) SemanticError::raise(n->get_loc(), "Cannot reference non-struct pointer with ->");

  // Check for matching field name
  for (unsigned i = 0; i < struct_type->get_num_members(); i++) {
    const Member &mem = struct_type->get_member(i);
    if (mem.get_name() == field) {
      n->set_type(mem.get_type());
      return;
    }
  }
  SemanticError::raise(n->get_loc(), "Field does not exist in struct");
}

/**
 * Visits array element reference.
 **/
void SemanticAnalysis::visit_array_element_ref_expression(Node *n) {
  // Visit variable
  visit(n->get_kid(0));
  // Visit index
  visit(n->get_kid(1));

  std::shared_ptr<Type> var_type = n->get_kid(0)->get_type();

  if (!var_type->is_array() && !var_type->is_pointer()) SemanticError::raise(n->get_loc(), "Cannot reference non-array with []");
  if (!n->get_kid(1)->get_type()->is_integral()) SemanticError::raise(n->get_loc(), "Cannot reference array with non-integral index");
  // TODO: check for array index out of bounds?

  // Annotate node
  n->set_type(n->get_kid(0)->get_type()->get_base_type());
}

/**
 * Annotates var reference with pointer to Symbol representing symbol table entry.
 **/
void SemanticAnalysis::visit_variable_ref(Node *n) {
  const std::string &var_name = n->get_kid(0)->get_str();
  std::shared_ptr<Symbol> sym = m_cur_symtab->lookup_recursive(var_name);
  if (sym == nullptr) SemanticError::raise(n->get_loc(), "Undeclared variable reference");

  assert (!n->has_symbol());
  n->set_symbol(sym);
}

/**
 * Visits literal value. Annotates node with processed type.
 **/
void SemanticAnalysis::visit_literal_value(Node *n) {
  int tag = n->get_kid(0)->get_tag();
  const std::string &lexeme = n->get_kid(0)->get_str();
  const Location &loc = n->get_kid(0)->get_loc();
  std::shared_ptr<Type> lit_type;
  LiteralValue lit;

  switch (tag) {
    case TOK_INT_LIT:
      {
        lit = LiteralValue::from_int_literal(lexeme, loc);
        BasicTypeKind lit_kind = lit.is_long() ? BasicTypeKind::LONG : BasicTypeKind::INT;
        lit_type = std::shared_ptr<Type>(new BasicType(lit_kind, !lit.is_unsigned()));
      }
      break;
    case TOK_CHAR_LIT:
      lit = LiteralValue::from_char_literal(lexeme, loc);
      lit_type = std::shared_ptr<Type>(new BasicType(BasicTypeKind::INT, true));
      break;
    case TOK_STR_LIT:
      lit = LiteralValue::from_str_literal(lexeme, loc);
      lit_type = std::shared_ptr<Type>(new BasicType(BasicTypeKind::CHAR, true));
      lit_type = std::shared_ptr<Type>(new QualifiedType(lit_type, TypeQualifier::CONST));
      lit_type = std::shared_ptr<Type>(new PointerType(lit_type));
      break;
    default:
      SemanticError::raise(loc, "Unrecognized literal");
  }
  // Annotate node with literal type
  n->set_type(lit_type);
}
