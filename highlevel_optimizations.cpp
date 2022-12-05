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

////////////
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
  } else if (op == Operator::MOD) {
    // Identity: mod by one 
    if (right.has_imm_ival() && right.get_imm_ival() == 1) {
      result_iseq->append(new Instruction(mov_opcode, dest, Operand(Operand::IMM_IVAL, 0)));
      return true;
    }
  }

  return false;
}


std::shared_ptr<InstructionSequence> LocalValueNumbering::transform_basic_block(const InstructionSequence *orig_bb) {
  // TODO
  // Clear map first
  lvn_map.clear();

  std::shared_ptr<InstructionSequence> result_iseq(new InstructionSequence());

  for (auto i = orig_bb->cbegin(); i != orig_bb->cend(); i++) {
    Instruction *orig_ins = *i;

    if (HighLevel::is_def(orig_ins)) {
      // For now assume num_operands = 2 (maybe deal with unary expressions later)
      unsigned num_operands = orig_ins->get_num_operands();
      if (num_operands != 2) continue;

      Operand dest = orig_ins->get_operand(0);
      Operand left = orig_ins->get_operand(1);
      Operand right = orig_ins->get_operand(2);

      if (left.is_imm_ival() && right.is_imm_ival()) {
        // Constant folding 
        constant_fold(result_iseq, orig_ins);
        continue;
      }

      // Algebraic identities
      if (check_algebraic_identities(result_iseq, orig_ins))
        continue;      

      // TODO commutativity?

      // LVN part
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
  // TODO

  return NULL;
}

/*************** COPY PROPAGATION ****************/
CopyPropagation::CopyPropagation(const std::shared_ptr<ControlFlowGraph> &cfg)
  : ControlFlowGraphTransform(cfg)
  { }

CopyPropagation::~CopyPropagation() { }

std::shared_ptr<InstructionSequence> CopyPropagation::transform_basic_block(const InstructionSequence *orig_bb) {
  // TODO

  return NULL;
}