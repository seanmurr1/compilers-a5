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

void LocalValueNumbering::constant_fold(std::shared_ptr<InstructionSequence> result_iseq, Instruction *orig_ins) {
  HighLevelOpcode opcode = orig_ins->get_opcode();
  Operand dest = orig_ins->get_operand(0);
  int left = orig_ins->get_operand(1).get_imm_ival();
  int right = orig_ins->get_operand(2).get_imm_ival();
  int result;
  int mov_shift;

  if (match_hl(HINS_add_b, opcode)) {
    result = left + right;
    mov_shift = opcode - HINS_add_b;
  } else if (match_hl(HINS_sub_b, opcode)) {
    result = left - right;
    mov_shift = opcode - HINS_sub_b;
  } else if (match_hl(HINS_mul_b, opcode)) {
    result = left * right;    
    mov_shift = opcode - HINS_mul_b;
  } else if (match_hl(HINS_div_b, opcode)) {
    result = left / right;
    mov_shift = opcode - HINS_div_b;
  } else if (match_hl(HINS_mod_b, opcode)) {
    result = left % right;
    mov_shift = opcode - HINS_mod_b;
  } else if (match_hl(HINS_cmplt_b, opcode)) {
    result = left < right;
    mov_shift = opcode - HINS_cmplt_b;
  } else if (match_hl(HINS_cmplte_b, opcode)) {
    result = left <= right;
    mov_shift = opcode - HINS_cmplte_b;
  } else if (match_hl(HINS_cmpgt_b, opcode)) {
    result = left > right;
    mov_shift = opcode - HINS_cmpgt_b;
  } else if (match_hl(HINS_cmpgte_b, opcode)) {
    result = left >= right;
    mov_shift = opcode - HINS_cmpgte_b;
  } else if (match_hl(HINS_cmpeq_b, opcode)) {
    result = left == right;
    mov_shift = opcode - HINS_cmpeq_b;
  } else if (match_hl(HINS_cmpneq_b, opcode)) {
    result = left != right;
    mov_shift = opcode - HINS_cmpneq_b;
  } else 
    assert(false);

  HighLevelOpcode mov_opcode = HINS_mov_b + mov_shift;

  result_iseq->append(new Instruction(mov_opcode, dest, Operand(Operand::IMM_IVAL, result)));
}


std::shared_ptr<InstructionSequence> LocalValueNumbering::transform_basic_block(const InstructionSequence *orig_bb) {
  // TODO
  std::shared_ptr<InstructionSequence> result_iseq(new InstructionSequence());

  for (auto i = orig_bb->cbegin(); i != orig_bb->cend(); i++) {
    Instruction *orig_ins = *i;

    if (HighLevel::is_def(orig_ins)) {
      Operand dest = orig_ins->get_operand(0);
      unsigned num_operands = orig_ins->get_num_operands();

      // For now assume num_operands = 2 (maybe deal with unary expressions later)
      if (num_operands != 2) continue;

      Operand left = orig_ins->get_operand(1);
      Operand right = orig_ins->get_operand(2);

      if (left.is_imm_ival() && right.is_imm_ival()) {
        // Constant folding 
        // TODO
        constant_fold(result_iseq, orig_ins);
        continue;
      }
      // TODO algebraic identities

      


      HighLevel::is_use(orig_ins, 5);

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
}

/*************** COPY PROPAGATION ****************/
CopyPropagation::CopyPropagation(const std::shared_ptr<ControlFlowGraph> &cfg)
  : ControlFlowGraphTransform(cfg)
  { }

CopyPropagation::~CopyPropagation() { }

std::shared_ptr<InstructionSequence> CopyPropagation::transform_basic_block(const InstructionSequence *orig_bb) {
  // TODO
}