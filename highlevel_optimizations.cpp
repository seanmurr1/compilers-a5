#include "highlevel_optimizations.h"
#include "highlevel.h"
#include "node.h"
#include "cfg.h"

/*************** HL Optimizer ****************/
HighLevelOptimizer::HighLevelOptimizer() { }

HighLevelOptimizer::~HighLevelOptimizer() { }

std::shared_ptr<InstructionSequence> HighLevelOptimizer::optimize(std::shared_ptr<InstructionSequence> &cur_hl_iseq) {
  Node *funcdef_ast = cur_hl_iseq->get_funcdef_ast();

  // Global callee-saved register assignment
  GlobalCalleeSavedRegAssignment global_assigner;
  cur_hl_iseq = global_assigner.optimize(cur_hl_iseq);

  // Build CFG
  HighLevelControlFlowGraphBuilder hl_cfg_builder(cur_hl_iseq);
  std::shared_ptr<ControlFlowGraph> cfg = hl_cfg_builder.build();

  int num_iterations = 2;
  for (int i = 0; i < num_iterations; i++) {
    // Constant propagation
    ConstantPropagation constant_prop(cfg);
    cfg = constant_prop.transform_cfg();
    // LVN
    LocalValueNumbering lvn(cfg);
    cfg = lvn.transform_cfg();
    // Copy propagation
    CopyPropagation copy_prop(cfg);
    cfg = copy_prop.transform_cfg();
    // Dead store elimination
    DeadStoreElimination dead_elim(cfg);
    cfg = dead_elim.transform_cfg();
  }

  // Local register allocation
  LocalRegisterAllocation local_assigner(cfg);
  cfg = local_assigner.transform_cfg();
  int num_reg_spilled = local_assigner.get_num_reg_spilled();
  funcdef_ast->set_max_temp_vreg(num_reg_spilled + 9);

  // Convert transformed high-level CFG back into iseq
  cur_hl_iseq = cfg->create_instruction_sequence();
  // Function def AST might have info needed for low-level code generation
  cur_hl_iseq->set_funcdef_ast(funcdef_ast);
  // Return transformed HL sequence
  return cur_hl_iseq;
}

/*************** DEAD STORE ELIMINATION ****************/
DeadStoreElimination::DeadStoreElimination(const std::shared_ptr<ControlFlowGraph> &cfg)
  : ControlFlowGraphTransform(cfg)
  , m_live_vregs(cfg) {
    m_live_vregs.execute();
}

DeadStoreElimination::~DeadStoreElimination() { }

/**
 * Perform dead store elimination on single block.
 **/
std::shared_ptr<InstructionSequence> DeadStoreElimination::transform_basic_block(const InstructionSequence *orig_bb) {
  // LiveVregs needs a pointer to a BasicBlock object to get a dataflow fact for that basic block
  const BasicBlock *orig_bb_as_basic_block = static_cast<const BasicBlock *>(orig_bb);

  std::shared_ptr<InstructionSequence> result_iseq(new InstructionSequence());

  for (auto i = orig_bb->cbegin(); i != orig_bb->cend(); i++) {
    Instruction *orig_ins = *i;
    bool preserve_instruction = true;

    if (HighLevel::is_def(orig_ins)) {
      Operand dest = orig_ins->get_operand(0);

      LiveVregs::FactType live_after = m_live_vregs.get_fact_after_instruction(orig_bb_as_basic_block, orig_ins);

      if (!live_after.test(dest.get_base_reg()) && dest.get_base_reg() > 6)
        // Dest register is immediately dead after this instruction
        // so it can be eliminated 
       preserve_instruction = false; 
    }

    if (preserve_instruction)
      result_iseq->append(orig_ins->duplicate());
  }
  return result_iseq;
}

/*************** LOCAL VALUE NUMBERING ****************/
LocalValueNumbering::LocalValueNumbering(const std::shared_ptr<ControlFlowGraph> &cfg)
  : ControlFlowGraphTransform(cfg)
  { }

LocalValueNumbering::~LocalValueNumbering() { }

/**
 * Check for matching HL opcode.
 **/ 
bool match_hl(int base, int hl_opcode) {
  return hl_opcode >= base && hl_opcode < (base + 4);
}

/**
 * Gets shift from 'b' variant of a HL opcode.
 * Convert to mov opcode.
 **/
HighLevelOpcode get_mov_opcode(HighLevelOpcode opcode) {
  int mov_shift = 0;
  if (match_hl(HINS_add_b, opcode)) 
    mov_shift = opcode - HINS_add_b;
  else if (match_hl(HINS_sub_b, opcode)) 
    mov_shift = opcode - HINS_sub_b;
  else if (match_hl(HINS_mul_b, opcode)) 
    mov_shift = opcode - HINS_mul_b;
  else if (match_hl(HINS_div_b, opcode)) 
    mov_shift = opcode - HINS_div_b;
  else if (match_hl(HINS_mod_b, opcode)) 
    mov_shift = opcode - HINS_mod_b;
  else if (match_hl(HINS_cmplt_b, opcode)) 
    mov_shift = opcode - HINS_cmplt_b;
  else if (match_hl(HINS_cmplte_b, opcode)) 
    mov_shift = opcode - HINS_cmplte_b;
  else if (match_hl(HINS_cmpgt_b, opcode)) 
    mov_shift = opcode - HINS_cmpgt_b;
  else if (match_hl(HINS_cmpgte_b, opcode)) 
    mov_shift = opcode - HINS_cmpgte_b;
  else if (match_hl(HINS_cmpeq_b, opcode)) 
    mov_shift = opcode - HINS_cmpeq_b;
  else if (match_hl(HINS_cmpneq_b, opcode)) 
    mov_shift = opcode - HINS_cmpneq_b;
  else if (match_hl(HINS_mov_b, opcode))
    mov_shift = opcode - HINS_mov_b;
  else {
    mov_shift = 3;
  }

  return (HighLevelOpcode) ((int) HINS_mov_b + mov_shift);
}

/**
 * Gets operator type from a HL opcode.
 **/
Operator get_operator(HighLevelOpcode opcode) {

  if (match_hl(HINS_add_b, opcode)) 
    return Operator::ADD;
  else if (match_hl(HINS_sub_b, opcode)) 
    return Operator::SUB;
  else if (match_hl(HINS_mul_b, opcode)) 
    return Operator::MUL;
  else if (match_hl(HINS_div_b, opcode)) 
    return Operator::DIV;
  else if (match_hl(HINS_mod_b, opcode)) 
    return Operator::MOD;
  else if (match_hl(HINS_cmplt_b, opcode)) 
    return Operator::LT;
  else if (match_hl(HINS_cmplte_b, opcode)) 
    return Operator::LTE;
  else if (match_hl(HINS_cmpgt_b, opcode)) 
    return Operator::GT;
  else if (match_hl(HINS_cmpgte_b, opcode)) 
    return Operator::GTE;
  else if (match_hl(HINS_cmpeq_b, opcode)) 
    return Operator::EQ;
  else if (match_hl(HINS_cmpneq_b, opcode)) 
    return Operator::NEQ;
  else 
    assert(false);

}

/**
 * Applies constant folding to a binary expression involving two constants.
 **/
void LocalValueNumbering::constant_fold(std::shared_ptr<InstructionSequence> &result_iseq, Instruction *orig_ins) {
  HighLevelOpcode opcode = (HighLevelOpcode) orig_ins->get_opcode();
  Operand dest = orig_ins->get_operand(0);
  long left = orig_ins->get_operand(1).get_imm_ival();
  long right = orig_ins->get_operand(2).get_imm_ival();
  Operator op = get_operator(opcode);
  long result;

  switch (op) {
    case Operator::ADD:
      result = left + right;
      break;
    case Operator::SUB:
      result = left - right;
      break;
    case Operator::MUL:
      result = left * right;
      break;
    case Operator::DIV:
      result = left / right;
      break;
    case Operator::MOD:
      result = left % right;
      break;
    case Operator::GT:
      result = left > right;
      break;
    case Operator::GTE:
      result = left >= right;
      break;
    case Operator::LT:
      result = left < right;
      break;
    case Operator::LTE:
      result = left <= right;
      break;
    case Operator::EQ:
      result = left == right;
      break;
    case Operator::NEQ:
      result = left != right;
      break;
    default:
      break;
  }

  HighLevelOpcode mov_opcode = get_mov_opcode(opcode);
  result_iseq->append(new Instruction(mov_opcode, dest, Operand(Operand::IMM_IVAL, result)));
}

/**
 * Checks if a instruction utilizes an algebraic identity.
 * If so, simplify instruction and return true.
 * Return false otherwise.
 **/
bool LocalValueNumbering::check_algebraic_identities(std::shared_ptr<InstructionSequence> &result_iseq, Instruction *orig_ins) {
  HighLevelOpcode opcode = (HighLevelOpcode) orig_ins->get_opcode();
  Operand dest = orig_ins->get_operand(0);
  Operand left = orig_ins->get_operand(1);
  Operand right = orig_ins->get_operand(2);

  Operator op = get_operator(opcode);
  HighLevelOpcode mov_opcode = get_mov_opcode(opcode);

  if (op == Operator::ADD) {
    // Identity: add zero
    if (left.has_imm_ival() && left.get_imm_ival() == 0) {
      result_iseq->append(new Instruction(mov_opcode, dest, right));
      return true;
    } else if (right.has_imm_ival() && right.get_imm_ival() == 0) {
      result_iseq->append(new Instruction(mov_opcode, dest, left));
      return true;
    }
  } else if (op == Operator::SUB) {
    // Identity: sub zero
    if (right.has_imm_ival() && right.get_imm_ival() == 0) {
      result_iseq->append(new Instruction(mov_opcode, dest, left));
      return true;
    } 
    // Identity: x - x = 0
    else if (left == right) {
      result_iseq->append(new Instruction(mov_opcode, dest, Operand(Operand::IMM_IVAL, 0)));
      return true;
    }
    // TODO: 0 - R => but we would need to emit negate instruction...?
  } else if (op == Operator::MUL) {
    // Identity: multiply by one
    if (left.has_imm_ival() && left.get_imm_ival() == 1) {
      result_iseq->append(new Instruction(mov_opcode, dest, right));
      return true;
    } else if (right.has_imm_ival() && right.get_imm_ival() == 1) {
      result_iseq->append(new Instruction(mov_opcode, dest, left));
      return true;
    }
  } else if (op == Operator::DIV) {
    // Identity: divide by one
    if (right.has_imm_ival() && right.get_imm_ival() == 1) {
      result_iseq->append(new Instruction(mov_opcode, dest, left));
      return true;
    }
    // Identity: x / x = 1
    else if (left == right) {
      result_iseq->append(new Instruction(mov_opcode, dest, Operand(Operand::IMM_IVAL, 1)));
      return true;
    }
  } else if (op == Operator::MOD) {
    // Identity: mod by one 
    if (right.has_imm_ival() && right.get_imm_ival() == 1) {
      result_iseq->append(new Instruction(mov_opcode, dest, Operand(Operand::IMM_IVAL, 0)));
      return true;
    }
  }

  // Case: no identity
  return false;
}

// Checks if an opcode represents a commutative operation.
bool is_commutative(HighLevelOpcode opcode) {
  return match_hl(HINS_add_b, opcode)
      || match_hl(HINS_mul_b, opcode)
      || match_hl(HINS_cmpeq_b, opcode)
      || match_hl(HINS_cmpneq_b, opcode);
}

void LocalValueNumbering::invalidate_mappings(Operand op) {
  if (reverse_map.count(op) == 1) {
    // Invalidate previous bindings
    std::set<LVN_key> &reverse_mappings = reverse_map[op];
    for (auto key : reverse_mappings) {
      if (lvn_map.count(key) == 0)
        continue;
      if (!key.contains(op) && !(lvn_map[key] == op))
        continue;
      lvn_map.erase(key);
    }
    reverse_map.erase(op);
  }
}

void LocalValueNumbering::fix_commutativity(std::vector<Operand> &right_side) {
  bool swap = false;
  if (right_side[0].has_base_reg() && right_side[1].has_base_reg()) {
    if (right_side[1] < right_side[0]) 
      swap = true;
  } else if (right_side[1].has_base_reg()) {
    swap = true;
  }
  if (swap) {
    Operand temp = right_side[1];
    right_side[1] = right_side[0];
    right_side[0] = temp;
  }
}

void LocalValueNumbering::process_definition(Instruction *orig_ins, std::shared_ptr<InstructionSequence> &result_iseq) {
  // For now assume num_operands = 3 (maybe deal with unary expressions later)
  unsigned num_operands = orig_ins->get_num_operands();
  Operand dest = orig_ins->get_operand(0);
  // need to invalidate for dest regardless
  invalidate_mappings(dest);

  if (num_operands < 3) {
    result_iseq->append(orig_ins->duplicate());
    return;
  }

  Operand left = orig_ins->get_operand(1);
  Operand right = orig_ins->get_operand(2);

  // Constant folding
  if (left.is_imm_ival() && right.is_imm_ival()) {
    constant_fold(result_iseq, orig_ins);
    return;
  }
  // Algebraic identities
  if (check_algebraic_identities(result_iseq, orig_ins))
    return;   

  HighLevelOpcode opcode = (HighLevelOpcode) orig_ins->get_opcode();   
  std::vector<Operand> right_side(2);
  right_side[0] = left;
  right_side[1] = right;

  // Fix commutativity
  if (is_commutative(opcode)) 
    fix_commutativity(right_side);

  // Local Value Numbering
  left = right_side[0];
  right = right_side[1];

  Operator op = get_operator(opcode); // TODO: maybe just use HighLevelOpcode to hash
                                      // bc of sizing and all
  // Create hash key
  LVN_key key(left, right, op);

  if (lvn_map.count(key) == 0) {
    // Key not present: append original instruction
    result_iseq->append(orig_ins->duplicate());
  } else {
    // Key present: replace instruction with copy
    Operand to_copy = lvn_map[key];
    HighLevelOpcode mov_opcode = get_mov_opcode(opcode);
    if (!(dest == to_copy))
      result_iseq->append(new Instruction(mov_opcode, dest, to_copy));
  }

  // Update maps
  lvn_map[key] = dest;
  reverse_map[dest].insert(key); 
  if (left.has_base_reg())
    reverse_map[left].insert(key);
  if (right.has_base_reg())
    reverse_map[right].insert(key);     
}

/**
 * Apply LVN to a single block.
 **/
std::shared_ptr<InstructionSequence> LocalValueNumbering::transform_basic_block(const InstructionSequence *orig_bb) {
  // Clear map first
  lvn_map.clear();
  reverse_map.clear();

  std::shared_ptr<InstructionSequence> result_iseq(new InstructionSequence());

  for (auto i = orig_bb->cbegin(); i != orig_bb->cend(); i++) {
    Instruction *orig_ins = *i;

    if (HighLevel::is_def(orig_ins)) {
      process_definition(orig_ins, result_iseq);
    } else {
      result_iseq->append(orig_ins->duplicate());
    }
  }
  return result_iseq;


  // TODO: issue: need to invalidate reverse mappings
  // when we add a new mapping, need to invalidate all mappings
  // that involve any of the registers (not constants) invovled in the 
  // new mapping

  // or maybe with localaddr????
}

/**
 * Adds a new instruction to result instruction sequence,
 * where the number of passed operands is variable.
 **/
void add_variable_length_ins(Instruction *orig_ins, std::shared_ptr<InstructionSequence> &result_iseq, std::vector<Operand> &new_ops) {
  HighLevelOpcode opcode = (HighLevelOpcode) orig_ins->get_opcode();
  int num_operands = orig_ins->get_num_operands();
  switch (num_operands) {
    case 0:
      result_iseq->append(new Instruction(opcode));
      break;
    case 1: 
      result_iseq->append(new Instruction(opcode, new_ops[0]));
      break;
    case 2:
      result_iseq->append(new Instruction(opcode, new_ops[0], new_ops[1]));
      break;
    case 3:
      result_iseq->append(new Instruction(opcode, new_ops[0], new_ops[1], new_ops[2]));
      break;
    default:
      break;
  }
}

/*************** CONSTANT PROPAGATION ****************/
ConstantPropagation::ConstantPropagation(const std::shared_ptr<ControlFlowGraph> &cfg)
  : ControlFlowGraphTransform(cfg)
  { }

ConstantPropagation::~ConstantPropagation() { }

/**
 * Process a defining instruction and perform constant propagation if necessary.
 **/
void ConstantPropagation::process_definition(Instruction *orig_ins, std::shared_ptr<InstructionSequence> &result_iseq) {
  HighLevelOpcode opcode = (HighLevelOpcode) orig_ins->get_opcode();
  unsigned num_operands = orig_ins->get_num_operands();
  Operand dest = orig_ins->get_operand(0);
  int reg = dest.get_base_reg();

  if (match_hl(HINS_localaddr, opcode)) {
    constants_map.erase(dest);
    constants_map.erase(dest.to_memref());
    result_iseq->append(orig_ins->duplicate());
  } else if (match_hl(HINS_mov_b, opcode)) {
    Operand src = orig_ins->get_operand(1);
    if (src.is_imm_ival()) {
      // Dest now tracks a constant: add to map
      constants_map[dest] = src.get_imm_ival();
      //printf("vr%d now tracks constant: %d\n", reg, orig_ins->get_operand(1).get_imm_ival());
    } else {
      // Dest no longer tracks a constant: remove from map
      constants_map.erase(dest);
      constants_map.erase(dest.to_memref());
      //printf("vr%d no longer tracks\n", reg);
      // Check second value
      if (constants_map.count(src) == 1) 
        src = Operand(Operand::IMM_IVAL, constants_map[src]);
    }
    result_iseq->append(new Instruction(opcode, dest, src));
  } else {
    // Dest no longer tracks a constant: remove from map
    constants_map.erase(dest);
    constants_map.erase(dest.to_memref());
    std::vector<Operand> new_ops(num_operands);
    new_ops[0] = dest;
    for (int i = 1; i < num_operands; i++) {
      Operand op = orig_ins->get_operand(i);
      if (op.has_base_reg() && constants_map.count(op) == 1) {
        // We have a copy stored
        op = Operand(Operand::IMM_IVAL, constants_map[op]);
        //printf("Using %d in place of vr%d\n", constants_map[op], op.get_base_reg());
      }        
      new_ops[i] = op;
    }
    add_variable_length_ins(orig_ins, result_iseq, new_ops);
  } 
}

/**
 * Perform constant propagation on a single block.
 **/
std::shared_ptr<InstructionSequence> ConstantPropagation::transform_basic_block(const InstructionSequence *orig_bb) {
  // Clear map first
  constants_map.clear();

  std::shared_ptr<InstructionSequence> result_iseq(new InstructionSequence());

  for (auto i = orig_bb->cbegin(); i != orig_bb->cend(); i++) {
    Instruction *orig_ins = *i;

    // Do not check call, return, etc. instructions (those with less than 2 operands)
    if (orig_ins->get_num_operands() < 2) 
      result_iseq->append(orig_ins->duplicate());
    else if (HighLevel::is_def(orig_ins)) 
      process_definition(orig_ins, result_iseq);
    else 
      result_iseq->append(orig_ins->duplicate());
  }
  return result_iseq;
}

/*************** COPY PROPAGATION ****************/
CopyPropagation::CopyPropagation(const std::shared_ptr<ControlFlowGraph> &cfg)
  : ControlFlowGraphTransform(cfg)
  { }

CopyPropagation::~CopyPropagation() { }

/**
 * Process a defining instruction and perform copy propagation if necessary.
 **/
void CopyPropagation::process_definition(Instruction *orig_ins, std::shared_ptr<InstructionSequence> &result_iseq) {
  HighLevelOpcode opcode = (HighLevelOpcode) orig_ins->get_opcode();
  unsigned num_operands = orig_ins->get_num_operands();
  Operand dest = orig_ins->get_operand(0);
  int reg = dest.get_base_reg();
  
  // Remove reverse mappings
  if (reverse_map.count(dest) == 1) {
    std::set<Operand> &reverse_mappings = reverse_map[dest];
    for (auto i : reverse_mappings) 
      if (copy_map.count(i) == 1 && (copy_map[i] == dest || copy_map[i] == dest.to_memref())) {
        copy_map.erase(i);
        copy_map.erase(i.to_memref());
      }
    reverse_mappings.clear();
    reverse_map.erase(dest);
  }
  
  if (match_hl(HINS_localaddr, opcode)) {
    copy_map.erase(dest);
    copy_map.erase(dest.to_memref());
    result_iseq->append(orig_ins->duplicate());
  } else if (match_hl(HINS_mov_b, opcode)) {
    if (orig_ins->get_operand(1).is_imm_ival()) {
      // Dest no longer tracks a vreg to copy: remove from map
      copy_map.erase(dest);
      copy_map.erase(dest.to_memref());
    } else {
      // Dest tracks vreg to copy into: add to map
      Operand copy = orig_ins->get_operand(1);  
      if (!copy.is_memref()) {
        copy_map[dest] = copy;
        reverse_map[copy].insert(dest);
      }      
    }
    // Duplicate instruction
    result_iseq->append(orig_ins->duplicate());
  } else {
    std::vector<Operand> new_ops(num_operands);
    new_ops[0] = dest;
    for (int i = 1; i < num_operands; i++) {
      Operand op = orig_ins->get_operand(i);
      if (op.has_base_reg() && copy_map.count(op) == 1)
        // We have a copy stored
        op = copy_map[op];
      new_ops[i] = op;
    }
    add_variable_length_ins(orig_ins, result_iseq, new_ops);
  }
}

/**
 * Perform copy propagation on a single block.
 **/
std::shared_ptr<InstructionSequence> CopyPropagation::transform_basic_block(const InstructionSequence *orig_bb) {
  // Clear map first
  copy_map.clear();
  reverse_map.clear();

  std::shared_ptr<InstructionSequence> result_iseq(new InstructionSequence());

  for (auto i = orig_bb->cbegin(); i != orig_bb->cend(); i++) {
    Instruction *orig_ins = *i;

    // Do not check call, return, etc. instructions (those with less than 2 operands)
    if (orig_ins->get_num_operands() < 2) 
      result_iseq->append(orig_ins->duplicate());
    else if (HighLevel::is_def(orig_ins)) 
      process_definition(orig_ins, result_iseq);
    else 
      result_iseq->append(orig_ins->duplicate());
  }
  return result_iseq;
}

/*************** LOCAL REGISTER ALLOCATION ****************/
LocalRegisterAllocation::LocalRegisterAllocation(const std::shared_ptr<ControlFlowGraph> &cfg)
  : ControlFlowGraphTransform(cfg)
  , m_live_vregs(cfg)
  , max_reg_spilled(0)
  , max_reg_to_not_use(-1) {
    m_live_vregs.execute();
}

LocalRegisterAllocation::~LocalRegisterAllocation() { }

/**
 * Determine which argument/caller-saved registers are used in this block,
 * along with which VREG #s not to map to local registers.
 * 
 * Returns VREG # of last arg register used.
 **/
int LocalRegisterAllocation::process_registers(const InstructionSequence *orig_bb) {
  // LiveVregs needs a pointer to a BasicBlock object to get a dataflow fact for that basic block
  const BasicBlock *orig_bb_as_basic_block = static_cast<const BasicBlock *>(orig_bb);
  LiveVregs::FactType live_after = m_live_vregs.get_fact_at_end_of_block(orig_bb_as_basic_block);
  
  int last_arg_reg_used = 0;
  for (auto i = orig_bb->cbegin(); i != orig_bb->cend(); i++) {
    Instruction *orig_ins = *i;
    HighLevelOpcode opcode = (HighLevelOpcode) orig_ins->get_opcode();
    if (match_hl(HINS_div_b, opcode) || match_hl(HINS_mod_b, opcode))
      last_arg_reg_used = 3;

    int num_operands = orig_ins->get_num_operands();
    for (int i = 0; i < num_operands; i++) {
      Operand op = orig_ins->get_operand(i);
      if (!op.has_base_reg()) continue;

      int reg = op.get_base_reg();
      if (live_after.test(reg) || op.get_mreg() >= 0) {
        if (op.get_mreg() < 0 && reg > max_reg_to_not_use)
          max_reg_to_not_use = reg;        
        
        do_not_map.insert(reg);
      }

      if (reg >= 1 && reg <= 6) 
        last_arg_reg_used++;
    }    
  }
  return last_arg_reg_used;
}

/**
 * Perform local register allocation, spilling as needed.
 **/
void LocalRegisterAllocation::local_allocation(const InstructionSequence *orig_bb, std::shared_ptr<InstructionSequence> &result_iseq) {
 for (auto i = orig_bb->cbegin(); i != orig_bb->cend(); i++) {
    Instruction *orig_ins = *i;
    int num_operands = orig_ins->get_num_operands();
    std::vector<Operand> new_ops(num_operands);

    // Operands mapped in this given instruction
    currently_mapped.clear();
    int ops_mapped = 0;

    for (int i = 0; i < num_operands; i++) {
      Operand op = orig_ins->get_operand(i);
      if (!op.has_base_reg() || ops_mapped >= num_local_regs) {
        new_ops[i] = op;
        continue;
      }

      int reg = op.get_base_reg();
      if (do_not_map.count(reg) == 1 || reg <= 6) {
        new_ops[i] = op;
        continue;
      }

      // Allocate local reg if not assigned or currently spilled
      if (local_reg_map.count(reg) == 0) {
        HighLevelOpcode mov_opcode = get_mov_opcode((HighLevelOpcode) orig_ins->get_opcode());
        allocate_and_assign_register(result_iseq, op, i == 0, mov_opcode);
      }
      
      new_ops[i] = Operand(op.get_kind(), local_reg_map[reg]); // TODO: should this just be Operand::VREG? for kind...

      currently_mapped.insert(reg);
      ops_mapped++;
    }  
    add_variable_length_ins(orig_ins, result_iseq, new_ops);
  }
}

std::shared_ptr<InstructionSequence> LocalRegisterAllocation::transform_basic_block(const InstructionSequence *orig_bb) {
  // Reset data
  do_not_map.clear();
  local_reg_map.clear();
  spilled_regs.clear();
  spill_locations.clear();

  std::shared_ptr<InstructionSequence> result_iseq(new InstructionSequence());

  // Process vregs in block
  int last_arg_reg_used = process_registers(orig_bb);

  // VREG # of first local reg to allocate
  start_local_reg = last_arg_reg_used + 1;
  num_local_regs = 7 - start_local_reg;
  cur_local_reg_idx = 0;
  reverse_map = std::vector<int>(num_local_regs);
  for (int i = 0; i < num_local_regs; i++)
    reverse_map[i] = -1;

  if (max_reg_to_not_use == -1 || max_reg_to_not_use < 10)
    first_spill_reg = 10;
  else 
    first_spill_reg = max_reg_to_not_use + 1;

  // Perform local register allocation
  local_allocation(orig_bb, result_iseq);

  int total_reg_needed = spill_locations.size() + (first_spill_reg - 10);
  if (total_reg_needed > max_reg_spilled)
    max_reg_spilled = total_reg_needed;

  return result_iseq;
}

/**
 * Allocate local register, spilling if needed.
 **/
int LocalRegisterAllocation::allocate_register(std::shared_ptr<InstructionSequence> &result_iseq, HighLevelOpcode mov_opcode) {
  int to_spill = reverse_map[cur_local_reg_idx];
  while (to_spill != -1 && currently_mapped.count(to_spill) == 1) {
    cur_local_reg_idx = (cur_local_reg_idx + 1) % num_local_regs;
    to_spill = reverse_map[cur_local_reg_idx];
  }
  
  int local_reg_num = cur_local_reg_idx + start_local_reg;

  if (to_spill != -1) {
    // Find spill location
    int spill_index = -1;
    for (int i = 0; i < spill_locations.size(); i++) {
      if (!spill_locations[i]) {
        spill_index = i;
        spill_locations[i] = true;
        break;
      }
    }
    if (spill_index == -1) {
      spill_index = spill_locations.size();
      spill_locations.push_back(true);
    }
    // Spill register
    Operand spill_register(Operand::VREG, first_spill_reg + spill_index);
    Operand local_reg(Operand::VREG, local_reg_num);
    //result_iseq->append(new Instruction(HINS_mov_q, spill_register, local_reg));
    result_iseq->append(new Instruction(mov_opcode, spill_register, local_reg));

    // Update tracking of spilled registers
    spilled_regs[to_spill] = spill_index;
    reverse_map[cur_local_reg_idx] = -1;
    local_reg_map.erase(to_spill);
  }

  return local_reg_num;
}

/**
 * Allocates and assigns local register to operand.
 **/
void LocalRegisterAllocation::allocate_and_assign_register(std::shared_ptr<InstructionSequence> &result_iseq, Operand op, bool def, HighLevelOpcode mov_opcode) {
  // Obtain local register
  int local_reg_num = allocate_register(result_iseq, mov_opcode);

  int reg = op.get_base_reg();
  if (!def) {
    // If we are not dealing with the defined var, we need to move the reg's 
    // previous value into the newly assigned local reg
    Operand prev_loc = op;
    Operand local_reg(Operand::VREG, local_reg_num);
    if (spilled_regs.count(reg) == 1) {
      // If value is currently spilled
      int spill_index = spilled_regs[reg];
      prev_loc = Operand(Operand::VREG, first_spill_reg + spill_index); // TODO: should use op.get_kind() instead?
      spilled_regs.erase(reg);
      spill_locations[spill_index] = false;
    }

    //result_iseq->append(new Instruction(HINS_mov_q, local_reg, prev_loc));
    result_iseq->append(new Instruction(mov_opcode, local_reg, prev_loc));
  }

  local_reg_map[reg] = local_reg_num;
  reverse_map[cur_local_reg_idx] = reg;

  cur_local_reg_idx = (cur_local_reg_idx + 1) % num_local_regs;
}

int LocalRegisterAllocation::get_num_reg_spilled() {
  return max_reg_spilled;
}

// TODO: need to track largset number of spilled registers ever encountered
// pass this to fundef to change storage needed for vregs...

/*************** GLOBAL CALLEE SAVED REGISTER ASSIGNMENT ****************/
GlobalCalleeSavedRegAssignment::GlobalCalleeSavedRegAssignment() { }

GlobalCalleeSavedRegAssignment::~GlobalCalleeSavedRegAssignment() { }

/**
 * Get ref counts of all VREGs that track local variables.
 * Update internal map.
 **/ 
void GlobalCalleeSavedRegAssignment::update_ref_counts(std::shared_ptr<InstructionSequence> &orig_iseq, int last_local_var_reg) {
  for (auto i = orig_iseq->cbegin(); i != orig_iseq->cend(); i++) {
    Instruction *orig_ins = *i;
    for (int i = 0; i < orig_ins->get_num_operands(); i++) {
      Operand op = orig_ins->get_operand(i);
      if (!op.has_base_reg()) continue;
      int reg = op.get_base_reg();
      if (reg < 10 || reg > last_local_var_reg) continue;

      ref_counts[reg - 10].second++;
    }
  }
}

// Compare two VREG's ref counts for sorting in decreasing order.
bool cmp_ref_count(std::pair<int, int> &a, std::pair<int, int> &b) {
  return a.second > b.second;
}

/**
 * Assigns callee-saved mregs to local vars with highest ref counts.
 **/
void GlobalCalleeSavedRegAssignment::assign_mregs(std::vector<MachineReg> &assigned_mregs, int num_local_vars) {
  mapped_mregs.clear();
  assigned_mregs.clear();
  for (int i = 0; i < num_local_vars; i++) {
    // Stop when we have used all available callee-saved mregs
    if (i >= mregs.size()) break;

    MachineReg mreg = mregs[i];
    std::string &mreg_name = mreg_names[i];
    std::pair<int, int> &op_ref_count = ref_counts[i];
    mapped_mregs[op_ref_count.first] = mreg;
    assigned_mregs.push_back(mreg);

    printf("/* allocate machine register %s to vreg %d, with rank %d */\n", mreg_name.c_str(), op_ref_count.first, op_ref_count.second);
  }
}

/**
 * Duplicate iseq, but tag operands that have been assigned 
 * a machine register.
 **/
void GlobalCalleeSavedRegAssignment::tag_operands(std::shared_ptr<InstructionSequence> &orig_iseq, std::shared_ptr<InstructionSequence> &result_iseq) {
  for (auto i = orig_iseq->cbegin(); i != orig_iseq->cend(); i++) {
    Instruction *orig_ins = *i;
    if (i.has_label()) 
      result_iseq->define_label(i.get_label());

    int num_operands = orig_ins->get_num_operands();
    std::vector<Operand> new_ops(num_operands);

    for (int i = 0; i < num_operands; i++) {
      Operand op = orig_ins->get_operand(i);
      new_ops[i] = op;
      if (!op.has_base_reg()) 
        continue;

      int reg = op.get_base_reg();
      if (mapped_mregs.count(reg) == 1)
        new_ops[i].assign_mreg(mapped_mregs[reg]);
    }
    add_variable_length_ins(orig_ins, result_iseq, new_ops);
  }
}

/**
 * Perform global callee saved register assignment.
 **/
std::shared_ptr<InstructionSequence> GlobalCalleeSavedRegAssignment::optimize(std::shared_ptr<InstructionSequence> &orig_iseq) {
  std::shared_ptr<InstructionSequence> result_iseq(new InstructionSequence());
  Node *funcdef_ast = orig_iseq->get_funcdef_ast();
  std::shared_ptr<Symbol> funcdef_sym = funcdef_ast->get_symbol();

  int last_local_var_reg = funcdef_sym->get_vreg() - 1;
  int num_local_vars = last_local_var_reg - 9;
  
  ref_counts = std::vector<std::pair<int, int>>(num_local_vars);
  for (int i = 0; i < num_local_vars; i++) 
    ref_counts[i] = std::make_pair(10 + i, 0);
  
  // Get ref counts for each local var
  update_ref_counts(orig_iseq, last_local_var_reg);
  // Sort in decreasing order by ref counts
  std::sort(ref_counts.begin(), ref_counts.end(), cmp_ref_count);
  // Assign as many mregs as possible
  assign_mregs(funcdef_ast->get_assigned_mregs(), num_local_vars);
  // Iterate through instructions and tag operands with assigned mregs
  tag_operands(orig_iseq, result_iseq);

  return result_iseq;
}
