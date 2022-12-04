// Copyright (c) 2021, David H. Hovemeyer <david.hovemeyer@gmail.com>
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the "Software"),
// to deal in the Software without restriction, including without limitation
// the rights to use, copy, modify, merge, publish, distribute, sublicense,
// and/or sell copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
// THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
// OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
// ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
// OTHER DEALINGS IN THE SOFTWARE.

#ifndef NODE_BASE_H
#define NODE_BASE_H

#include <memory>
#include "type.h"
#include "symtab.h"
#include "literal_value.h"
#include "instruction.h"

// The Node class will inherit from this type, so you can use it
// to define any attributes and methods that Node objects should have
// (constant value, results of semantic analysis, code generation info,
// etc.)
class NodeBase {
private:
  // fields (pointer to Type, pointer to Symbol, etc.)
  std::shared_ptr<Symbol> m_symbol;
  std::shared_ptr<Type> m_type;

  // Operand associated with node
  Operand m_op;

  // Function type (only used for function call nodes)
  std::shared_ptr<Type> m_fn_type;

  int m_max_temp_vreg;

  // copy ctor and assignment operator not supported
  NodeBase(const NodeBase &);
  NodeBase &operator=(const NodeBase &);

public:
  NodeBase();
  virtual ~NodeBase();

  void set_symbol(const std::shared_ptr<Symbol> &symbol);
  void set_type(const std::shared_ptr<Type> &type);
  bool has_symbol() const;
  std::shared_ptr<Symbol> get_symbol() const;
  std::shared_ptr<Type> get_type() const;

  void set_operand(Operand op);
  Operand get_operand() const;
  Operand get_address_of_operand() const;

  void set_max_temp_vreg(int vreg) { m_max_temp_vreg = vreg; }
  int get_max_temp_vreg() const { return m_max_temp_vreg; }

  void set_fn_type(const std::shared_ptr<Type> &type) { m_fn_type = type; }
  std::shared_ptr<Type> get_fn_type() const { return m_fn_type; }

};

#endif // NODE_BASE_H
