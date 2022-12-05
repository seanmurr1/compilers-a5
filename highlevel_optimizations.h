#ifndef HIGHLEVEL_OPTIMIZATIONS_H
#define HIGHLEVEL_OPTIMIZATIONS_H

#include "cfg_transform.h"
#include "live_vregs.h"

#include <unordered_map>


class DeadStoreElimination : public ControlFlowGraphTransform {
  private:
    LiveVregs m_live_vregs;

  public:
    DeadStoreElimination(const std::shared_ptr<ControlFlowGraph> &cfg);
    ~DeadStoreElimination();

    virtual std::shared_ptr<InstructionSequence> transform_basic_block(const InstructionSequence *orig_bb);
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
    };

    class LVN_hasher {
    public:
      size_t operator()(const LVN_key &k) const {
        return ((k.left.hash()
                  ^ (k.right.hash() << 1)) >> 1)
                  ^ (std::hash<int>()((int) k.op) << 1); 
      }
    };

    std::unordered_map<LVN_key, Operand, LVN_hasher> lvn_map;

    void constant_fold(std::shared_ptr<InstructionSequence> result_iseq, Instruction *orig_ins);
    bool check_algebraic_identities(std::shared_ptr<InstructionSequence> result_iseq, Instruction *orig_ins);

  public:
    LocalValueNumbering(const std::shared_ptr<ControlFlowGraph> &cfg);
    ~LocalValueNumbering();

    virtual std::shared_ptr<InstructionSequence> transform_basic_block(const InstructionSequence *orig_bb);
};

class ConstantPropagation : public ControlFlowGraphTransform {
  private:
    // TODO

  public:
    ConstantPropagation(const std::shared_ptr<ControlFlowGraph> &cfg);
    ~ConstantPropagation();

    virtual std::shared_ptr<InstructionSequence> transform_basic_block(const InstructionSequence *orig_bb);
};

class CopyPropagation : public ControlFlowGraphTransform {
  private:
    // TODO

  public:
    CopyPropagation(const std::shared_ptr<ControlFlowGraph> &cfg);
    ~CopyPropagation();

    virtual std::shared_ptr<InstructionSequence> transform_basic_block(const InstructionSequence *orig_bb);
};

#endif // HIGHLEVEL_OPTIMIZATIONS_H