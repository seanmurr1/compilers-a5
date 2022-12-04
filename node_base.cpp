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

#include <cassert>
#include "node_base.h"

NodeBase::NodeBase() {
  m_symbol = nullptr;
  m_type = nullptr;
}

NodeBase::~NodeBase() {
}

/**
 * Set node's symbol.
 * Must not have a type or symbol already.
 **/
void NodeBase::set_symbol(const std::shared_ptr<Symbol> &symbol) {
  assert(!has_symbol());
  assert(m_type == nullptr);
  m_symbol = symbol;
}

/** 
 * Set node's type.
 * Must not have a type or symbol already.
 **/
void NodeBase::set_type(const std::shared_ptr<Type> &type) {
  assert(!has_symbol());
  assert(!m_type);
  m_type = type;
}

// Does node have a symbol?
bool NodeBase::has_symbol() const {
  return m_symbol != nullptr;
}

// Get node's symbol.
std::shared_ptr<Symbol> NodeBase::get_symbol() const {
  return m_symbol;
}

/**
 * Gets node's type. If it has a symbol, returns
 * symbol's type.
 **/
std::shared_ptr<Type> NodeBase::get_type() const {
  // this shouldn't be called unless there is actually a type
  // associated with this node

  if (has_symbol()) 
    return m_symbol->get_type(); // Symbol will definitely have a valid Type
  else {
    assert(m_type); // make sure a Type object actually exists
    return m_type;
  }
}

// Sets operand for given node
void NodeBase::set_operand(Operand op) {
  m_op = op;
}

// Get operand associated with node
Operand NodeBase::get_operand() const {
  if (has_symbol() && m_symbol->requires_storage()) {
    return m_op.to_memref();
  } else {
    return m_op;
  }
}

// Get address of operand associated with node
// This just means do not dereference if it 
// requires storage.
Operand NodeBase::get_address_of_operand() const {
  return m_op;
}


