#include "highlevel_optimizations.h"
#include "highlevel.h"


/*************** DEAD STORE ELIMINATION ****************/
DeadStoreElimination::DeadStoreElimination(const std::shared_ptr<ControlFlowGraph> &cfg)
  : ControlFlowGraphTransform(cfg)
  , m_live_vregs(cfg) {
    m_live_vregs.execute();
}

DeadStoreElimination::~DeadStoreElimination() { }

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

      if (!live_after.test(dest.get_base_reg()))
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
 **/
int get_opcode_offset(HighLevelOpcode opcode) {

  if (match_hl(HINS_add_b, opcode)) 
    return opcode - HINS_add_b;
  else if (match_hl(HINS_sub_b, opcode)) 
    return opcode - HINS_sub_b;
  else if (match_hl(HINS_mul_b, opcode)) 
    return opcode - HINS_mul_b;
  else if (match_hl(HINS_div_b, opcode)) 
    return opcode - HINS_div_b;
  else if (match_hl(HINS_mod_b, opcode)) 
    return opcode - HINS_mod_b;
  else if (match_hl(HINS_cmplt_b, opcode)) 
    return opcode - HINS_cmplt_b;
  else if (match_hl(HINS_cmplte_b, opcode)) 
    return opcode - HINS_cmplte_b;
  else if (match_hl(HINS_cmpgt_b, opcode)) 
    return opcode - HINS_cmpgt_b;
  else if (match_hl(HINS_cmpgte_b, opcode)) 
    return opcode - HINS_cmpgte_b;
  else if (match_hl(HINS_cmpeq_b, opcode)) 
    return opcode - HINS_cmpeq_b;
  else if (match_hl(HINS_cmpneq_b, opcode)) 
    return opcode - HINS_cmpneq_b;
  else 
    assert(false);

  // TODO: add more variants to check
  return 0;
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
void LocalValueNumbering::constant_fold(std::shared_ptr<InstructionSequence> result_iseq, Instruction *orig_ins) {
  HighLevelOpcode opcode = (HighLevelOpcode) orig_ins->get_opcode();
  Operand dest = orig_ins->get_operand(0);
  int left = orig_ins->get_operand(1).get_imm_ival();
  int right = orig_ins->get_operand(2).get_imm_ival();
  int mov_shift = get_opcode_offset(opcode);
  Operator op = get_operator(opcode);
  int result;

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

  HighLevelOpcode mov_opcode = (HighLevelOpcode) ((int) HINS_mov_b + mov_shift);
  result_iseq->append(new Instruction(mov_opcode, dest, Operand(Operand::IMM_IVAL, result)));
}

/**
 * Checks if a instruction utilizes an algebraic identity.
 * If so, simplify instruction and return true.
 * Return false otherwise.
 **/
bool LocalValueNumbering::check_algebraic_identities(std::shared_ptr<InstructionSequence> result_iseq, Instruction *orig_ins) {
  HighLevelOpcode opcode = (HighLevelOpcode) orig_ins->get_opcode();
  Operand dest = orig_ins->get_operand(0);
  Operand left = orig_ins->get_operand(1);
  Operand right = orig_ins->get_operand(2);

  int mov_shift = get_opcode_offset(opcode);
  Operator op = get_operator(opcode);
  HighLevelOpcode mov_opcode = (HighLevelOpcode) ((int) HINS_mov_b + mov_shift);

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

/**
 * Apply LVN to a single block.
 **/
std::shared_ptr<InstructionSequence> LocalValueNumbering::transform_basic_block(const InstructionSequence *orig_bb) {
  // Clear map first
  lvn_map.clear();

  std::shared_ptr<InstructionSequence> result_iseq(new InstructionSequence());

  for (auto i = orig_bb->cbegin(); i != orig_bb->cend(); i++) {
    Instruction *orig_ins = *i;

    if (HighLevel::is_def(orig_ins)) {
      // For now assume num_operands = 2 (maybe deal with unary expressions later)
      unsigned num_operands = orig_ins->get_num_operands();
 
      printf("Num operands: %u\n", num_operands);

      if (num_operands < 2) {
        result_iseq->append(orig_ins->duplicate());
        continue;
      }

      Operand dest = orig_ins->get_operand(0);
      Operand left = orig_ins->get_operand(1);
      Operand right = orig_ins->get_operand(2);

      // Constant folding
      if (left.is_imm_ival() && right.is_imm_ival()) {
        constant_fold(result_iseq, orig_ins);
        continue;
      }
      // Algebraic identities
      if (check_algebraic_identities(result_iseq, orig_ins))
        continue;      

      // TODO commutativity?

      // Local Value Numbering
      HighLevelOpcode opcode = (HighLevelOpcode) orig_ins->get_opcode();
      Operator op = get_operator(opcode);
      // Create hash key
      LVN_key key(left, right, op);

      if (lvn_map.count(key) == 0) {
        // Key not present: append original instruction
        result_iseq->append(orig_ins->duplicate());
      } else {
        // Key present: replace instruction with copy
        Operand to_copy = lvn_map[key];
        int mov_shift = get_opcode_offset(opcode);
        HighLevelOpcode mov_opcode = (HighLevelOpcode) ((int) HINS_mov_b + mov_shift);
        result_iseq->append(new Instruction(mov_opcode, dest, to_copy));
      }
      // Update map
      lvn_map[key] = dest;
    } else {
      result_iseq->append(orig_ins->duplicate());
    }
  }
  return result_iseq;
}

/*************** CONSTANT PROPAGATION ****************/
ConstantPropagation::ConstantPropagation(const std::shared_ptr<ControlFlowGraph> &cfg)
  : ControlFlowGraphTransform(cfg)
  { }

ConstantPropagation::~ConstantPropagation() { }

std::shared_ptr<InstructionSequence> ConstantPropagation::transform_basic_block(const InstructionSequence *orig_bb) {
  // Clear map first
  constants_map.clear();

  std::shared_ptr<InstructionSequence> result_iseq(new InstructionSequence());

  for (auto i = orig_bb->cbegin(); i != orig_bb->cend(); i++) {
    Instruction *orig_ins = *i;

    // Do not check call, return, etc. instructions (those with less than 2 operands)
    if (orig_ins->get_num_operands() < 2) {
      result_iseq->append(orig_ins->duplicate());
      continue;
    }

    // Check for def
    if (HighLevel::is_def(orig_ins)) {
      HighLevelOpcode opcode = (HighLevelOpcode) orig_ins->get_opcode();
      unsigned num_operands = orig_ins->get_num_operands();
      Operand dest = orig_ins->get_operand(0);

      if (match_hl(HINS_mov_b, opcode)) {
        if (orig_ins->get_operand(1).is_imm_ival()) {
          // Dest now tracks a constant: add to map
          constants_map[dest] = orig_ins->get_operand(1);
        } else {
          // Dest no longer tracks a constant: remove from map
          constants_map.erase(dest);
        }
        // Duplicate instruction
        result_iseq->append(orig_ins->duplicate());
        continue;
      }

      if (num_operands == 2) {
        Operand new_src = orig_ins->get_operand(1);
        // Check if we have a stored constant for operand
        new_src = constants_map.count(new_src) == 1 ? constants_map[new_src] : new_src;
        result_iseq->append(new Instruction(opcode, dest, new_src));
      } else if (num_operands == 3) {
        Operand new_left = orig_ins->get_operand(1);
        Operand new_right = orig_ins->get_operand(1);
        // Check if we have a stored constant for operands
        new_left = constants_map.count(new_left) == 1 ? constants_map[new_left] : new_left;
        new_right = constants_map.count(new_right) == 1 ? constants_map[new_right] : new_right;
        result_iseq->append(new Instruction(opcode, dest, new_left, new_right));
      }
    } else {
      result_iseq->append(orig_ins->duplicate());
      continue;
    }
  }
  return result_iseq;
}

/*************** COPY PROPAGATION ****************/
CopyPropagation::CopyPropagation(const std::shared_ptr<ControlFlowGraph> &cfg)
  : ControlFlowGraphTransform(cfg)
  { }

CopyPropagation::~CopyPropagation() { }

std::shared_ptr<InstructionSequence> CopyPropagation::transform_basic_block(const InstructionSequence *orig_bb) {
  // Clear map first
  copy_map.clear();

  std::shared_ptr<InstructionSequence> result_iseq(new InstructionSequence());

  for (auto i = orig_bb->cbegin(); i != orig_bb->cend(); i++) {
    Instruction *orig_ins = *i;

    // Do not check call, return, etc. instructions (those with less than 2 operands)
    if (orig_ins->get_num_operands() < 2) {
      result_iseq->append(orig_ins->duplicate());
      continue;
    }

    // Check for def
    if (HighLevel::is_def(orig_ins)) {
      HighLevelOpcode opcode = (HighLevelOpcode) orig_ins->get_opcode();
      unsigned num_operands = orig_ins->get_num_operands();
      Operand dest = orig_ins->get_operand(0);

      if (match_hl(HINS_mov_b, opcode)) {
        if (orig_ins->get_operand(1).is_imm_ival()) {
          // Dest no longer tracks a vreg to copy: remove from map
          copy_map.erase(dest);
        } else {
          // Dest tracks vreg to copy into: add to map
          copy_map[dest] = orig_ins->get_operand(1);
        }
        // Duplicate instruction
        result_iseq->append(orig_ins->duplicate());
        continue;
      }

      if (num_operands == 2) {
        Operand new_src = orig_ins->get_operand(1);
        // Check if we have a stored constant for operand
        new_src = copy_map.count(new_src) == 1 ? copy_map[new_src] : new_src;
        result_iseq->append(new Instruction(opcode, dest, new_src));
      } else if (num_operands == 3) {
        Operand new_left = orig_ins->get_operand(1);
        Operand new_right = orig_ins->get_operand(1);
        // Check if we have a stored constant for operands
        new_left = copy_map.count(new_left) == 1 ? copy_map[new_left] : new_left;
        new_right = copy_map.count(new_right) == 1 ? copy_map[new_right] : new_right;
        result_iseq->append(new Instruction(opcode, dest, new_left, new_right));
      }
    } else {
      result_iseq->append(orig_ins->duplicate());
      continue;
    }
  }
  return result_iseq;
}