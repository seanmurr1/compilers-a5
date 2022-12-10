#ifndef HIGHLEVEL_OPTIMIZATIONS_H
#define HIGHLEVEL_OPTIMIZATIONS_H

#include "cfg_transform.h"
#include "live_vregs.h"
#include "lowlevel.h"
#include "highlevel.h"

#include <unordered_map>
#include <set>
#include <utility>

/**
 * Utility class to perform all high-level optmizations.
 **/
class HighLevelOptimizer {
  public:
    HighLevelOptimizer();
    ~HighLevelOptimizer();

    std::shared_ptr<InstructionSequence> optimize(std::shared_ptr<InstructionSequence> &cur_hl_iseq);
};

/**
 * Dead store elimination optimization.
 **/
class DeadStoreElimination : public ControlFlowGraphTransform {
  private:
    LiveVregs m_live_vregs;

  public:
    DeadStoreElimination(const std::shared_ptr<ControlFlowGraph> &cfg);
    ~DeadStoreElimination();

    virtual std::shared_ptr<InstructionSequence> transform_basic_block(const InstructionSequence *orig_bb);
};

// Used to hash Operand objects.
class OperandHasher {
  public:
    size_t operator()(const Operand &o) const {
      return o.hash(); 
    }
};

/**
 * Simplify representation of some binary operators.
 **/
enum Operator {
  ADD,
  SUB,
  MUL,
  DIV,
  MOD,
  GT,
  GTE,
  LT,
  LTE,
  EQ,
  NEQ,
};

/**
 * Implement local-value-numbering (LVN), replacing redundant computations with 
 * copies of previously computed values.
 **/
class LocalValueNumbering : public ControlFlowGraphTransform {
  private:
    class LVN_key {
    public:
      Operand left;
      Operand right;
      Operator op;

      LVN_key(Operand l, Operand r, Operator o) : left(l), right(r), op(o) { }

      bool operator==(const LVN_key &other) const {
        return (left == other.left && right == other.right && op == other.op);
      }
      bool operator<(const LVN_key &other) const {
        return left < other.left || right < other.right || op < other.op;
      }
      bool contains(const Operand &op) const {
        return left == op || right == op;
      }
    };

    class LVN_hasher {
      public:
        size_t operator()(const LVN_key &k) const {
          return ((k.left.hash()
                  ^ (k.right.hash() << 1)) >> 1)
                  ^ (std::hash<int>()((int) k.op) << 1); 
        }
    };

    // Map LVN key to Operand/VREG
    std::unordered_map<LVN_key, Operand, LVN_hasher> lvn_map;
    // Pseudo-reverse mappings of above
    std::unordered_map<Operand, std::set<LVN_key>, OperandHasher> reverse_map;

    void constant_fold(std::shared_ptr<InstructionSequence> &result_iseq, Instruction *orig_ins);
    bool check_algebraic_identities(std::shared_ptr<InstructionSequence> &result_iseq, Instruction *orig_ins);
    void fix_commutativity(std::vector<Operand> &right_side);
    void process_definition(Instruction *orig_ins, std::shared_ptr<InstructionSequence> &result_iseq);

    void invalidate_mappings(Operand op);

  public:
    LocalValueNumbering(const std::shared_ptr<ControlFlowGraph> &cfg);
    ~LocalValueNumbering();

    virtual std::shared_ptr<InstructionSequence> transform_basic_block(const InstructionSequence *orig_bb);
};

/**
 * Constant propagation optimization.
 **/
class ConstantPropagation : public ControlFlowGraphTransform {
  private:
    // Map VREG to constant value
    std::unordered_map<Operand, int, OperandHasher> constants_map;

  public:
    ConstantPropagation(const std::shared_ptr<ControlFlowGraph> &cfg);
    ~ConstantPropagation();

    virtual std::shared_ptr<InstructionSequence> transform_basic_block(const InstructionSequence *orig_bb);
    void process_definition(Instruction *orig_ins, std::shared_ptr<InstructionSequence> &result_iseq);
};

/**
 * Copy propagation optimization.
 **/
class CopyPropagation : public ControlFlowGraphTransform {
  private:
    // Map VREG to VREG to copy
    std::unordered_map<Operand, Operand, OperandHasher> copy_map;
    // Reverse mappings of above
    std::unordered_map<Operand, std::set<Operand>, OperandHasher> reverse_map;

  public:
    CopyPropagation(const std::shared_ptr<ControlFlowGraph> &cfg);
    ~CopyPropagation();

    virtual std::shared_ptr<InstructionSequence> transform_basic_block(const InstructionSequence *orig_bb);
    void process_definition(Instruction *orig_ins, std::shared_ptr<InstructionSequence> &result_iseq);
};

class LocalRegisterAllocation : public ControlFlowGraphTransform {
  private:
    LiveVregs m_live_vregs;
    // Starting local reg # to use
    int start_local_reg;
    int num_local_regs;

    // Local reg # to VREG #
    std::vector<int> reverse_map;
    int cur_local_reg_idx;

    // Set of VREG #s that are alive at end of basic block and should not be mapped
    // or are already mapped to machine registers
    std::set<int> do_not_map;

    // Opearnds mapped in current instruction
    std::set<int> currently_mapped;

    // Map VREG # to spill location offset
    std::unordered_map<int, int> spilled_regs;
    // Available spill locations (vector of offsets)
    std::vector<bool> spill_locations;

    int max_reg_spilled;

    // Map VREG # to local register #
    std::unordered_map<int, int> local_reg_map;

    int process_registers(const InstructionSequence *orig_bb);
    void local_allocation(const InstructionSequence *orig_bb, std::shared_ptr<InstructionSequence> &result_iseq);
    int allocate_register(std::shared_ptr<InstructionSequence> &result_iseq, HighLevelOpcode mov_opcode);
    void allocate_and_assign_register(std::shared_ptr<InstructionSequence> &result_iseq, Operand op, bool def, HighLevelOpcode mov_opcode);

  public:
    LocalRegisterAllocation(const std::shared_ptr<ControlFlowGraph> &cfg);
    ~LocalRegisterAllocation();

    virtual std::shared_ptr<InstructionSequence> transform_basic_block(const InstructionSequence *orig_bb);
    int get_num_reg_spilled();

};

/**
 * Perform global callee-saved register assignment.
 **/
class GlobalCalleeSavedRegAssignment {
  private:
    std::vector<MachineReg> mregs = {MachineReg::MREG_RBX, MachineReg::MREG_R12, MachineReg::MREG_R13, MachineReg::MREG_R14, MachineReg::MREG_R15};
    std::vector<std::string> mreg_names = {"\%rbx", "\%r12", "\%r13", "\%r14", "\%r15"};

    std::vector<std::pair<int, int>> ref_counts;
    std::unordered_map<int, MachineReg> mapped_mregs;

    void update_ref_counts(std::shared_ptr<InstructionSequence> &orig_iseq, int last_local_var_reg);
    void assign_mregs(std::vector<MachineReg> &assigned_mregs, int num_local_vars);
    void tag_operands(std::shared_ptr<InstructionSequence> &orig_iseq, std::shared_ptr<InstructionSequence> &result_iseq);

  public:
    GlobalCalleeSavedRegAssignment();
    ~GlobalCalleeSavedRegAssignment();

    std::shared_ptr<InstructionSequence> optimize(std::shared_ptr<InstructionSequence> &orig_iseq);
};

#endif // HIGHLEVEL_OPTIMIZATIONS_H