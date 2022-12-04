#ifndef SEMANTIC_ANALYSIS_H
#define SEMANTIC_ANALYSIS_H

#include <cstdint>
#include <memory>
#include <utility>
#include "type.h"
#include "symtab.h"
#include "ast_visitor.h"
#include "location.h"

class SemanticAnalysis : public ASTVisitor {
private:
  SymbolTable *m_global_symtab, *m_cur_symtab;
  std::shared_ptr<Type> m_cur_function;

public:
  SemanticAnalysis();
  virtual ~SemanticAnalysis();

  SymbolTable *get_global_symtab() { return m_global_symtab; }

  virtual void visit_struct_type(Node *n);
  virtual void visit_union_type(Node *n);
  virtual void visit_variable_declaration(Node *n);
  virtual void visit_basic_type(Node *n);
  virtual void visit_function_definition(Node *n);
  virtual void visit_function_declaration(Node *n);
  virtual void visit_function_parameter(Node *n);
  virtual void visit_statement_list(Node *n);
  virtual void visit_struct_type_definition(Node *n);
  virtual void visit_binary_expression(Node *n);
  virtual void visit_unary_expression(Node *n);
  virtual void visit_postfix_expression(Node *n);
  virtual void visit_conditional_expression(Node *n);
  virtual void visit_cast_expression(Node *n);
  virtual void visit_function_call_expression(Node *n);
  virtual void visit_field_ref_expression(Node *n);
  virtual void visit_indirect_field_ref_expression(Node *n);
  virtual void visit_array_element_ref_expression(Node *n);
  virtual void visit_variable_ref(Node *n);
  virtual void visit_literal_value(Node *n);
  virtual void visit_return_expression_statement(Node *n);

private:
  void process_declarator(std::vector<Node *> &vars, Node *declarator, const std::shared_ptr<Type> &base_type);
  void enter_scope();
  void leave_scope();
  void process_assignment(Node *n);
  void process_non_assignment(Node *n);
  void check_assignment(const std::shared_ptr<Type> &left, const std::shared_ptr<Type> &right, const Location &loc);
  void process_function_parameters(Node *parameter_list, std::vector<Node *> &declared_parameters, std::shared_ptr<Type> &fn_type);
  void add_vars_to_sym_table(std::vector<Node *> &vars);
  void visit_arithmetic_expression(Node *n);

  bool is_relational_or_logical_op(int tag);
  bool is_pointer_dereference(Node *n);
  bool is_lvalue(Node *n);

  Node *promote_type(Node *n, BasicTypeKind new_type, bool is_signed);
  Node *implicit_conversion(Node *n, const std::shared_ptr<Type> &type);
};

#endif // SEMANTIC_ANALYSIS_H
