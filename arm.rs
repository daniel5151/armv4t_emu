use super::Cpu;
use super::Reg;

enum Instruction {
    BranchImm, // link, signed offset
    BranchImmEx, // half-offset, offset
    BranchAbsEx, // link, exchange
    DataProc,
}

pub trait ArmIsaCpu {
    fn execute(&mut self);
}

impl ArmIsaCpu for Cpu {
    fn execute(&mut self) {
        let inst = self.mmu.load32(self.reg[super::PC_REG]);
    }
}
