#include <cassert>
#include "node.h"
#include "instruction.h"
#include "highlevel.h"
#include "ast.h"
#include "parse.tab.h"
#include "grammar_symbols.h"
#include "exceptions.h"
#include "highlevel_codegen.h"
#include "local_storage_allocation.h"

namespace {

// Adjust an opcode for a basic type
HighLevelOpcode get_opcode(HighLevelOpcode base_opcode, const std::shared_ptr<Type> &type) {
  if (type->is_basic())
    return static_cast<HighLevelOpcode>(int(base_opcode) + int(type->get_basic_type_kind()));
  else if (type->is_pointer() || type->is_array())
    return static_cast<HighLevelOpcode>(int(base_opcode) + int(BasicTypeKind::LONG));
  else
    RuntimeError::raise("attempt to use type '%s' as data in opcode selection", type->as_str().c_str());
}

}

HighLevelCodegen::HighLevelCodegen(int next_label_num)
  : m_next_label_num(next_label_num)
  , m_hl_iseq(new InstructionSequence()) {
}

HighLevelCodegen::~HighLevelCodegen() {
}

/**
 * Process function parameter, generating code to move it from 
 * argument register to parameter's vreg.
 **/
void HighLevelCodegen::process_parameter(Node *declarator, int register_index) {
  int tag = declarator->get_tag();
  switch (tag) {
    case AST_ARRAY_DECLARATOR:
      process_parameter(declarator->get_kid(0), register_index);
      return;
    case AST_POINTER_DECLARATOR:
      process_parameter(declarator->get_kid(0), register_index);
      return;
    case AST_NAMED_DECLARATOR:
      Node *var = declarator->get_kid(0);
      visit_variable_ref(declarator);
      Operand var_op = declarator->get_operand();
      Operand arg_register(Operand::VREG, register_index);
      HighLevelOpcode mov_opcode = get_opcode(HINS_mov_b, var->get_type());
      m_hl_iseq->append(new Instruction(mov_opcode, var_op, arg_register));
      return;
  }
}

/**
 * Get next temp vreg.
 **/
int HighLevelCodegen::next_temp_vreg() {
  int temp_vreg = m_next_temp_vreg;
  
  if (temp_vreg > m_max_temp_vreg)
    m_max_temp_vreg = temp_vreg;

  m_next_temp_vreg++;
  return temp_vreg;
}

/**
 * Generate code for a function.
 **/
void HighLevelCodegen::visit_function_definition(Node *n) {
  // generate the name of the label that return instructions should target
  std::string fn_name = n->get_kid(1)->get_str();
  m_return_label_name = ".L" + fn_name + "_return";

  std::shared_ptr<Symbol> fn_sym = n->get_symbol();

  // Function storage
  unsigned total_local_storage = fn_sym->get_offset();
  
  // Starting temp vreg #
  m_next_temp_vreg = fn_sym->get_vreg();
  // Starting max temp vreg # used 
  m_max_temp_vreg = m_next_temp_vreg - 1;

  m_hl_iseq->append(new Instruction(HINS_enter, Operand(Operand::IMM_IVAL, total_local_storage)));

  // Visit function parameters
  Node *parameters = n->get_kid(2);
  int register_index = 1;
  for (auto i = parameters->cbegin(); i != parameters->cend(); i++) {
    Node *declarator = (*i)->get_kid(1);
    process_parameter(declarator, register_index);
    register_index++;
  }

  // visit body
  visit(n->get_kid(3));

  m_hl_iseq->define_label(m_return_label_name);
  m_hl_iseq->append(new Instruction(HINS_leave, Operand(Operand::IMM_IVAL, total_local_storage)));
  m_hl_iseq->append(new Instruction(HINS_ret));

  // Set max temp vreg info for function node
  n->set_max_temp_vreg(m_max_temp_vreg);
}

/**
 * Generate code for expression. Update temp vreg numbers.
 **/
void HighLevelCodegen::visit_expression_statement(Node *n) {
  int save = m_next_temp_vreg;

  visit(n->get_kid(0));
  n->set_operand(n->get_kid(0)->get_operand());

  // Reset temp vregs
  m_next_temp_vreg = save;
}

// Visit return statement, generating code to jump to return label.
void HighLevelCodegen::visit_return_statement(Node *n) {
  // jump to the return label
  m_hl_iseq->append(new Instruction(HINS_jmp, Operand(Operand::LABEL, m_return_label_name)));
}

// Generate code to evaluate return expression.
void HighLevelCodegen::visit_return_expression_statement(Node *n) {
  Node *expr = n->get_kid(0);

  // generate code to evaluate the expression
  visit(expr);

  // move the computed value to the return value vreg
  HighLevelOpcode mov_opcode = get_opcode(HINS_mov_b, expr->get_type());
  m_hl_iseq->append(new Instruction(mov_opcode, Operand(Operand::VREG, LocalStorageAllocation::VREG_RETVAL), expr->get_operand()));

  // jump to the return label
  visit_return_statement(n);
}

/**
 * Generate code for while statement.
 **/
void HighLevelCodegen::visit_while_statement(Node *n) {
  std::string body_label = next_label();
  std::string cond_label = next_label();

  Node *condition = n->get_kid(0);
  Node *body = n->get_kid(1);

  m_hl_iseq->append(new Instruction(HINS_jmp, Operand(Operand::LABEL, cond_label)));
  m_hl_iseq->define_label(body_label);
  visit(body);
  m_hl_iseq->define_label(cond_label);
  visit(condition);
  m_hl_iseq->append(new Instruction(HINS_cjmp_t, condition->get_operand(), Operand(Operand::LABEL, body_label)));
}

/**
 * Generate code for do-while statement.
 **/
void HighLevelCodegen::visit_do_while_statement(Node *n) {
  std::string loop_label = next_label();

  Node *body = n->get_kid(0);
  Node *condition = n->get_kid(1);

  m_hl_iseq->define_label(loop_label);
  visit(body);
  visit(condition);
  m_hl_iseq->append(new Instruction(HINS_cjmp_t, condition->get_operand(), Operand(Operand::LABEL, loop_label)));
}

/**
 * Generate code for "for" statement.
 **/
void HighLevelCodegen::visit_for_statement(Node *n) {
  Node *loop_var_initialize = n->get_kid(0);
  Node *loop_cond = n->get_kid(1);
  Node *loop_update = n->get_kid(2);
  Node *loop_body = n->get_kid(3);

  std::string body_label = next_label();
  std::string cond_label = next_label();

  visit(loop_var_initialize);
  m_hl_iseq->append(new Instruction(HINS_jmp, Operand(Operand::LABEL, cond_label)));
  m_hl_iseq->define_label(body_label);
  visit(loop_body);
  visit(loop_update);
  m_hl_iseq->define_label(cond_label);
  visit(loop_cond);
  m_hl_iseq->append(new Instruction(HINS_cjmp_t, loop_cond->get_operand(), Operand(Operand::LABEL, body_label)));
}

/**
 * Generate code for if statement.
 **/
void HighLevelCodegen::visit_if_statement(Node *n) {
  std::string after_if_label = next_label();

  Node *condition = n->get_kid(0);
  Node *if_body = n->get_kid(1);

  visit(condition);
  m_hl_iseq->append(new Instruction(HINS_cjmp_f, condition->get_operand(), Operand(Operand::LABEL, after_if_label)));
  visit(if_body);
  m_hl_iseq->define_label(after_if_label);
}

/**
 * Generate code for if-else statement.
 **/
void HighLevelCodegen::visit_if_else_statement(Node *n) {
  std::string post_label = next_label();
  std::string else_label = next_label();

  Node *condition = n->get_kid(0);
  Node *if_body = n->get_kid(1);
  Node *else_body = n->get_kid(2);

  visit(condition);
  m_hl_iseq->append(new Instruction(HINS_cjmp_f, condition->get_operand(), Operand(Operand::LABEL, else_label)));
  visit(if_body);
  m_hl_iseq->append(new Instruction(HINS_jmp, Operand(Operand::LABEL, post_label)));
  m_hl_iseq->define_label(else_label);
  visit(else_body);
  m_hl_iseq->define_label(post_label);
}

/**
 * Generate code for assignment operation.
 **/
void HighLevelCodegen::generate_assignment(Node *n) {
  Operand left = n->get_kid(1)->get_operand();
  Operand right = n->get_kid(2)->get_operand();

  HighLevelOpcode mov_opcode = get_opcode(HINS_mov_b, n->get_kid(1)->get_type());

  std::shared_ptr<Type> left_type = n->get_kid(1)->get_type();

  // Do not dereference is left side is ptr/array
  if ((left_type->is_pointer() || left_type->is_array()) && right.is_memref())
    right = Operand(Operand::VREG, right.get_base_reg());

  m_hl_iseq->append(new Instruction(mov_opcode, left, right));
  n->set_operand(left);
}

/**
 * Generate code for non-assignment binary operation.
 **/
void HighLevelCodegen::generate_non_assignment(Node *n, int binary_op) {
  Operand left = n->get_kid(1)->get_operand();
  Operand right = n->get_kid(2)->get_operand();
  // left & right should have same types (since promotions already occurred)

  int vreg = next_temp_vreg();
  Operand dest(Operand::VREG, vreg);

  HighLevelOpcode opcode;
  switch (binary_op) {
    case TOK_PLUS:
      opcode = HINS_add_b;
      break;
    case TOK_MINUS:
      opcode = HINS_sub_b;
      break;
    case TOK_ASTERISK:
      opcode = HINS_mul_b;
      break;
    case TOK_DIVIDE:
      opcode = HINS_div_b;
      break;
    case TOK_MOD:
      opcode = HINS_mod_b;
      break;
    case TOK_LT:
      opcode = HINS_cmplt_b;
      break;
    case TOK_LTE:
      opcode = HINS_cmplte_b;
      break;
    case TOK_GT:
      opcode = HINS_cmpgt_b;
      break;
    case TOK_GTE:
      opcode = HINS_cmpgte_b;
      break;
    case TOK_EQUALITY:
      opcode = HINS_cmpeq_b;
      break;
    case TOK_INEQUALITY:
      opcode = HINS_cmpneq_b;
      break;
    case TOK_LOGICAL_OR:
      opcode = HINS_or_b;
      break;
    case TOK_LOGICAL_AND:
      opcode = HINS_and_b;
      break;
    default:
      // Unreachable
      break;
  }

  opcode = get_opcode(opcode, n->get_kid(1)->get_type());

  // Help resolve mem-references
  if (left.is_memref() && right.is_memref()) {
    HighLevelOpcode mov_opcode = get_opcode(HINS_mov_b, n->get_kid(1)->get_type());
    
    vreg = next_temp_vreg();
    Operand temp_left_op(Operand::VREG, vreg);

    m_hl_iseq->append(new Instruction(mov_opcode, temp_left_op, left));

    vreg = next_temp_vreg();
    Operand temp_right_op(Operand::VREG, vreg);

    m_hl_iseq->append(new Instruction(mov_opcode, temp_right_op, right));
    m_hl_iseq->append(new Instruction(opcode, dest, temp_left_op, temp_right_op));
  } else {
    m_hl_iseq->append(new Instruction(opcode, dest, left, right));
  }

  n->set_operand(dest);  
}

/**
 * Generates HL code for binary expression. Annotates node.
 **/
void HighLevelCodegen::visit_binary_expression(Node *n) {
  int binary_op = n->get_kid(0)->get_tag();
  // Visit left child
  visit(n->get_kid(1));
  // Visit right child
  visit(n->get_kid(2));

  if (binary_op == TOK_ASSIGN) {
    generate_assignment(n);
  } else {
    generate_non_assignment(n, binary_op);
  }
}

/**
 * Places fn arguments in argument registers and calls function.
 **/
void HighLevelCodegen::visit_function_call_expression(Node *n) {
  std::shared_ptr<Type> fn_type = n->get_fn_type();

  // Place arguments in argument registers
  Node *arg_list = n->get_kid(1);
  int arg_reg_index = 1;
  for (auto i = arg_list->cbegin(); i != arg_list->cend(); i++) {
    Node *arg = *i;
    visit(arg);
    Operand arg_op = arg->get_operand();

    // Do not dereference is member is ptr/array
    std::shared_ptr<Type> parameter_type = fn_type->get_member(arg_reg_index - 1).get_type();
    if (parameter_type->is_pointer() || parameter_type->is_array())
      arg_op = Operand(Operand::VREG, arg_op.get_base_reg());

    Operand arg_reg = Operand(Operand::VREG, arg_reg_index);
    HighLevelOpcode mov_opcode = get_opcode(HINS_mov_b, arg->get_type());
    m_hl_iseq->append(new Instruction(mov_opcode, arg_reg, arg_op));
    arg_reg_index++;
  }

  // Call function
  std::string fn_name = n->get_kid(0)->get_kid(0)->get_str();
  m_hl_iseq->append(new Instruction(HINS_call, Operand(Operand::LABEL, fn_name)));

  // Annotate node with return value in vr0
  if (fn_type->get_base_type()->get_basic_type_kind() == BasicTypeKind::VOID) {
    return;
  }

  int vreg = next_temp_vreg();
  Operand return_val(Operand::VREG, vreg);
  HighLevelOpcode mov_opcode = get_opcode(HINS_mov_b, n->get_type());
  m_hl_iseq->append(new Instruction(mov_opcode, return_val, Operand(Operand::VREG, LocalStorageAllocation::VREG_RETVAL)));
  n->set_operand(return_val);
}

/**
 * References array index. Converts index to quad if needed. Multiplies raw
 * index by size of data of array and accessed memory there.
 **/
void HighLevelCodegen::visit_array_element_ref_expression(Node *n) {
  Node *arr = n->get_kid(0);
  Node *index = n->get_kid(1);

  visit(arr);
  visit(index);

  int vreg;
  Operand raw_index = index->get_operand();
  Operand quad_index;

  // Check if need to convert index to quad word (64 bit pointers)
  BasicTypeKind basic_type = index->get_type()->get_basic_type_kind();
  if (basic_type != BasicTypeKind::LONG) {
    vreg = next_temp_vreg();
    quad_index = Operand(Operand::VREG, vreg);
    HighLevelOpcode convert_opcode;
    if (basic_type == BasicTypeKind::CHAR) {
      convert_opcode = HINS_sconv_bq;
    } else if (basic_type == BasicTypeKind::SHORT) {
      convert_opcode = HINS_sconv_wq;
    } else if (basic_type == BasicTypeKind::INT) {
      convert_opcode = HINS_sconv_lq;
    } else {
      RuntimeError::raise("Attempt to index array/pointer with %s", index->get_type()->as_str().c_str());
    }
    // Upgrade index to LONG
    m_hl_iseq->append(new Instruction(convert_opcode, quad_index, raw_index));
  } else {
    quad_index = raw_index;
  }

  // Mult index by size of type of array
  unsigned size = arr->get_type()->get_base_type()->get_storage_size();
  vreg = next_temp_vreg();
  Operand scaled_index = Operand(Operand::VREG, vreg);
  m_hl_iseq->append(new Instruction(HINS_mul_q, scaled_index, quad_index, Operand(Operand::IMM_IVAL, size)));

  // Add scaled index to pointer
  Operand arr_base = arr->get_address_of_operand();

  // Make sure array base is not dereferenced
  if (arr_base.is_memref()) 
    arr_base = Operand(Operand::VREG, arr_base.get_base_reg());

  vreg = next_temp_vreg();
  Operand arr_shifted = Operand(Operand::VREG, vreg);
  m_hl_iseq->append(new Instruction(HINS_add_q, arr_shifted, arr_base, scaled_index));

  // Annotate node with mem reference to shifted arr 
  n->set_operand(arr_shifted.to_memref());
}

/**
 * Annotates variable reference with operand.
 **/
void HighLevelCodegen::visit_variable_ref(Node *n) {
  int vreg;
  std::shared_ptr<Symbol> sym = n->get_symbol();
  if (sym->requires_storage()) {
    vreg = next_temp_vreg();
    Operand op(Operand::VREG, vreg);
    unsigned offset = sym->get_offset();
    m_hl_iseq->append(new Instruction(HINS_localaddr, op, Operand(Operand::IMM_IVAL, offset)));
    n->set_operand(op);
  } else {
    vreg = sym->get_vreg();
    Operand op(Operand::VREG, vreg);
    n->set_operand(op);
  }
}

/**
 * Generates code for a literal value.
 **/
void HighLevelCodegen::visit_literal_value(Node *n) {
  // A partial implementation (note that this won't work correctly
  // for string constants!):

  // String literal, already dealt with in module collector
  if (n->get_kid(0)->get_tag() == TOK_STR_LIT) {
    int vreg = next_temp_vreg();
    Operand dest(Operand::VREG, vreg);
    m_hl_iseq->append(new Instruction(HINS_mov_q, dest, n->get_kid(0)->get_operand()));
    n->set_operand(dest);
    return;
  }

  const std::string &lexeme = n->get_kid(0)->get_str();
  const Location &loc = n->get_kid(0)->get_loc();
  LiteralValue val;
  long ival;

  if (n->get_kid(0)->get_tag() == TOK_CHAR_LIT) {
    val = LiteralValue::from_char_literal(lexeme, loc);
    ival = val.get_char_value();
  } else {
    val = LiteralValue::from_int_literal(lexeme, loc);
    ival = val.get_int_value();
  }

  int vreg = next_temp_vreg();
  Operand dest(Operand::VREG, vreg);
  HighLevelOpcode mov_opcode = get_opcode(HINS_mov_b, n->get_type());
  m_hl_iseq->append(new Instruction(mov_opcode, dest, Operand(Operand::IMM_IVAL, ival)));
  n->set_operand(dest);
}

/**
 * Generates code for a unary expression.
 **/
void HighLevelCodegen::visit_unary_expression(Node *n) {
  // Visit expression argument
  Node *arg = n->get_kid(1);
  visit(arg);

  int vreg;
  Operand arg_op = arg->get_operand();
  int binary_op = n->get_kid(0)->get_tag();

  switch (binary_op) {
    case TOK_ASTERISK:
      if (arg_op.is_memref()) {
        vreg = next_temp_vreg();
        Operand op(Operand::VREG, vreg);
        HighLevelOpcode mov_opcode = get_opcode(HINS_mov_b, arg->get_type());
        m_hl_iseq->append(new Instruction(mov_opcode, op, arg_op));
        n->set_operand(op.to_memref());
      } else {
        Operand mem_access = arg_op.to_memref();
        n->set_operand(mem_access);
      }
      break;
    case TOK_AMPERSAND:
      {
      Operand mem_loc = arg->get_address_of_operand();
      n->set_operand(mem_loc);
      }
      break;
    case TOK_MINUS:
      {
      vreg = next_temp_vreg();
      Operand dest(Operand::VREG, vreg);
      HighLevelOpcode neg_opcode = get_opcode(HINS_neg_b, arg->get_type());
      m_hl_iseq->append(new Instruction(neg_opcode, dest, arg_op));
      n->set_operand(dest);
      }
      break;
    case TOK_NOT:
      {
      vreg = next_temp_vreg();
      Operand dest(Operand::VREG, vreg);
      HighLevelOpcode not_opcode = get_opcode(HINS_not_b, arg->get_type());
      m_hl_iseq->append(new Instruction(not_opcode, dest, arg_op));
      n->set_operand(dest);
      }
      break;
    default:
      // Unreachable
      break;
  }
}

/**
 * Generates operand for a struct's offset given a field name.
 * Returns temp vreg with offset stored.
 **/
Operand HighLevelCodegen::get_struct_offset(Node *struct_node, const std::string &field_name) {
  // Find field offset
  unsigned offset;
  std::shared_ptr<Type> struct_type = struct_node->get_type();
  if (struct_type->is_array() || struct_type->is_pointer()) {
    struct_type = struct_type->get_base_type();
  }
  for (unsigned i = 0; i < struct_type->get_num_members(); i++) {
    const Member &mem = struct_type->get_member(i);
    if (mem.get_name() == field_name) {
      offset = mem.get_offset();
      break;
    }
  }
  // Move offset into temp vreg
  int vreg = next_temp_vreg();
  Operand offset_op(Operand::VREG, vreg);
  m_hl_iseq->append(new Instruction(HINS_mov_q, offset_op, Operand(Operand::IMM_IVAL, offset)));
  return offset_op;
}

/**
 * Generate code to reference a field in a struct.
 **/
void HighLevelCodegen::visit_field_ref_expression(Node *n) {
  Node *struct_node = n->get_kid(0);
  Node *field_node = n->get_kid(1);
  const std::string &field_name = field_node->get_str();
  // Visit struct
  visit(struct_node);

  // Get struct offset 
  Operand offset_op = get_struct_offset(struct_node, field_name);
  // Add offset to struct register
  Operand struct_op = struct_node->get_address_of_operand();

  // Make sure struct base is not dereferenced
  if (struct_op.is_memref()) 
    struct_op = Operand(Operand::VREG, struct_op.get_base_reg());

  int vreg = next_temp_vreg();
  Operand shifted_field(Operand::VREG, vreg);
  m_hl_iseq->append(new Instruction(HINS_add_q, shifted_field, struct_op, offset_op));
  // Annotate node
  n->set_operand(shifted_field.to_memref());
}

/**
 * Generate code to indirectly reference a field in a struct.
 * This is identical to directly referencing a field since
 * structs always have an offset so we deal with them with mem 
 * references anyways.
 **/
void HighLevelCodegen::visit_indirect_field_ref_expression(Node *n) {
  visit_field_ref_expression(n);
}

void HighLevelCodegen::visit_implicit_conversion(Node *n) {
  // Visit child
  Node *prev = n->get_kid(0);
  visit(prev);

  BasicTypeKind prev_type = prev->get_type()->get_basic_type_kind();
  BasicTypeKind new_type = n->get_type()->get_basic_type_kind();

  // Case: truncation
  if (prev_type >= new_type) {
    // Just annotate with same vreg
    n->set_operand(prev->get_operand());
    return;
  }

  int vreg = next_temp_vreg();
  Operand converted(Operand::VREG, vreg);
  HighLevelOpcode conv_opcode;

  // Get conversion opcode
  if (n->get_type()->is_signed()) {
    if (prev->get_type()->get_basic_type_kind() == BasicTypeKind::CHAR && n->get_type()->get_basic_type_kind() == BasicTypeKind::SHORT) {
      conv_opcode = HINS_sconv_bw;
    } else if (prev->get_type()->get_basic_type_kind() == BasicTypeKind::CHAR && n->get_type()->get_basic_type_kind() == BasicTypeKind::INT) {
      conv_opcode = HINS_sconv_bl;
    } else if (prev->get_type()->get_basic_type_kind() == BasicTypeKind::CHAR && n->get_type()->get_basic_type_kind() == BasicTypeKind::LONG) {
      conv_opcode = HINS_sconv_bq;
    } else if (prev->get_type()->get_basic_type_kind() == BasicTypeKind::SHORT && n->get_type()->get_basic_type_kind() == BasicTypeKind::INT) {
      conv_opcode = HINS_sconv_wl;
    } else if (prev->get_type()->get_basic_type_kind() == BasicTypeKind::SHORT && n->get_type()->get_basic_type_kind() == BasicTypeKind::LONG) {
      conv_opcode = HINS_sconv_wq;
    } else if (prev->get_type()->get_basic_type_kind() == BasicTypeKind::INT && n->get_type()->get_basic_type_kind() == BasicTypeKind::LONG) {
      conv_opcode = HINS_sconv_lq;
    } 
  } else {
    if (prev->get_type()->get_basic_type_kind() == BasicTypeKind::CHAR && n->get_type()->get_basic_type_kind() == BasicTypeKind::SHORT) {
      conv_opcode = HINS_uconv_bw;
    } else if (prev->get_type()->get_basic_type_kind() == BasicTypeKind::CHAR && n->get_type()->get_basic_type_kind() == BasicTypeKind::INT) {
      conv_opcode = HINS_uconv_bl;
    } else if (prev->get_type()->get_basic_type_kind() == BasicTypeKind::CHAR && n->get_type()->get_basic_type_kind() == BasicTypeKind::LONG) {
      conv_opcode = HINS_uconv_bq;
    } else if (prev->get_type()->get_basic_type_kind() == BasicTypeKind::SHORT && n->get_type()->get_basic_type_kind() == BasicTypeKind::INT) {
      conv_opcode = HINS_uconv_wl;
    } else if (prev->get_type()->get_basic_type_kind() == BasicTypeKind::SHORT && n->get_type()->get_basic_type_kind() == BasicTypeKind::LONG) {
      conv_opcode = HINS_uconv_wq;
    } else if (prev->get_type()->get_basic_type_kind() == BasicTypeKind::INT && n->get_type()->get_basic_type_kind() == BasicTypeKind::LONG) {
      conv_opcode = HINS_uconv_lq;
    } 
  }

  m_hl_iseq->append(new Instruction(conv_opcode, converted, prev->get_operand()));
  n->set_operand(converted);
}

/**
 * Generates next label in format .L{next_label_num}.
 **/
std::string HighLevelCodegen::next_label() {
  std::string label = ".L" + std::to_string(m_next_label_num++);
  return label;
}
