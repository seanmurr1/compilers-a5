#include <cassert>
#include "node.h"
#include "symtab.h"
#include "ast.h"
#include "local_storage_allocation.h"

LocalStorageAllocation::LocalStorageAllocation()
  : m_storage_calc()
  , m_total_local_storage(0U)
  , m_next_vreg(VREG_FIRST_LOCAL) {
}

LocalStorageAllocation::~LocalStorageAllocation() {
}

/**
 * Get next virtual register. Update for future calls.
 **/
int LocalStorageAllocation::get_next_vreg() {
  int next_vreg = m_next_vreg;
  m_next_vreg++;
  return next_vreg;
}

/**
 * Process declarator. Sets vreg or storage offset for variable.
 **/
void LocalStorageAllocation::process_declarator(Node *declarator) {
  int tag = declarator->get_tag();
  switch (tag) {
    case AST_ARRAY_DECLARATOR:
      process_declarator(declarator->get_kid(0));
      return;
    case AST_POINTER_DECLARATOR:
      process_declarator(declarator->get_kid(0));
      return;
    case AST_NAMED_DECLARATOR:
      Node *var = declarator->get_kid(0);
      std::string var_name = var->get_str();
      std::shared_ptr<Symbol> sym = declarator->get_symbol();
      std::shared_ptr<Type> type = var->get_type();
      if ((type->is_integral() || type->is_pointer()) && !sym->requires_storage()) {
        // Set vreg if var is integral or a pointer, and its address is never taken
        int vreg = get_next_vreg();
        sym->set_vreg(vreg);
        printf("/* variable \'%s\' allocated vreg %d */\n", var_name.c_str(), vreg);
      } else {
        // Allocate offset in function's local storage area
        unsigned offset = m_storage_calc.add_field(type);
        sym->set_offset(offset);
        printf("/* variable \'%s\' allocated %u bytes of storage at offset %u */\n", var_name.c_str(), type->get_storage_size(), offset);
      }
      return;
  }
}

/**
 * Visits list of declarators.
 **/
void LocalStorageAllocation::visit_declarator_list(Node *n) {
  // Process all declarators
  for (auto i = n->cbegin(); i != n->cend(); i++) {
    Node *declarator = *i;
    process_declarator(declarator);
  }
}

/**
 * Visits function definition, setting up vregisters and storage offsets 
 * for vars and parameters.
 **/
void LocalStorageAllocation::visit_function_definition(Node *n) {
  // Reset variables
  m_total_local_storage = 0;
  m_next_vreg = VREG_FIRST_LOCAL;
  m_storage_calc = StorageCalculator();

  // Function info
  const std::string &fn_name = n->get_kid(1)->get_str();
  std::shared_ptr<Symbol> fn_sym = n->get_symbol();

  // Visit function parameters
  Node *fn_parameters = n->get_kid(2);
  for (auto i = fn_parameters->cbegin(); i != fn_parameters->cend(); i++) {
    Node *parameter = *i;
    visit_function_parameter(parameter);
  }

  // Visit function body
  visit(n->get_kid(3));

  m_storage_calc.finish();
  m_total_local_storage = m_storage_calc.get_size();
  int next_temp_vreg = get_next_vreg();
  // Just use offset field of symbol to store storage
  // We never use it for a function def node anyways...
  fn_sym->set_offset(m_total_local_storage);
  // Use vreg field to store next temp vreg for function to use
  fn_sym->set_vreg(next_temp_vreg);

  printf("/* Function \'%s\' uses %u bytes of memory and %d virtual registers */\n", fn_name.c_str(), m_total_local_storage, next_temp_vreg);
}

// Don't allocate for struct type defs.
void LocalStorageAllocation::visit_struct_type_definition(Node *n) {
  return;
}

// Don't allocate for function declarations.
void LocalStorageAllocation::visit_function_declaration(Node *n) {
  return;
}

/**
 * Visit function parameter.
 **/
void LocalStorageAllocation::visit_function_parameter(Node *n) {
  Node *declarator = n->get_kid(1);
  process_declarator(declarator);
}

/**
 * Visit list of statments to process.
 **/
void LocalStorageAllocation::visit_statement_list(Node *n) {
  // Enter nested scope

  // Issue with optimization...
  // StorageCalculator save = m_storage_calc;

  // Visit statements in list
  for (auto i = n->cbegin(); i != n->cend(); i++) {
    Node *child = *i;
    visit(child);
  }

  // Leave nested scope
  // m_storage_calc = save;  
}
