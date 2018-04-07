use super::*;
use super::bit_util::*;

#[derive(Clone, Copy, PartialEq, Debug)]
enum Instruction {
    BranchImm,
    BranchImmEx,
    BranchAbsEx,
    DataProc0,
    DataProc1,
    DataProc2,
    Invalid,
}

const INST_MATCH_ORDER: [Instruction; 7] = [
    Instruction::BranchAbsEx,
    Instruction::BranchImmEx,
    Instruction::BranchImm,
    Instruction::DataProc0,
    Instruction::DataProc1,
    Instruction::DataProc2,
    Instruction::Invalid,
];

impl Instruction {
    fn pattern(&self) -> (u32, u32) {
        use self::Instruction::*;
        #[cfg_attr(rustfmt, rustfmt_skip)]
        match *self {
            BranchImm   => (0x0e000000, 0x0a000000),
            BranchImmEx => (0xfe000000, 0xfa000000),
            BranchAbsEx => (0x0fffffd0, 0x012fff10),
            DataProc0   => (0x0e000010, 0x00000000),
            DataProc1   => (0x0e000090, 0x00000010),
            DataProc2   => (0x0e000000, 0x02000010),
            Invalid     => (0x00000000, 0x00000000),
        }
    }

    fn decode(inst: u32) -> Instruction {
        for typ in INST_MATCH_ORDER.iter() {
            let (mask, test) = typ.pattern();
            if mask_match(inst, mask, test) {
                return typ.clone();
            }
        }
        Instruction::Invalid
    }
}

fn cond_code(inst: u32) -> u32 {
    extract(inst, 28, 4)
}

fn cond_met(cond: u32, cpsr: u32) -> bool {
    true
}

pub trait ArmIsaCpu {
    fn execute(&mut self);
}

impl ArmIsaCpu for Cpu {
    fn execute(&mut self) {
        let pc = self.reg[PC_REG];
        let inst = self.mmu.load32(pc);
        // Decode first
        let cond = cond_code(inst);

        let cpsr = self.reg[CPSR_REG];

        if !cond_met(cond, cpsr) {
            self.reg[PC_REG] += 4;
            return;
        }

        let c = bit(cpsr, CPSR_C);

        use self::Instruction::*;
        let inst_type = self::Instruction::decode(inst);
        println!("Instruction: {:?}", inst_type);
        match inst_type {
            DataProc0 | DataProc1 | DataProc2 => {
                let i = bit(inst, 25);
                let s = bit(inst, 20);

                let opcode = extract(inst, 21, 4);

                let rn = extract(inst, 16, 4) as Reg;
                let rd = extract(inst, 12, 4) as Reg;

                /// Three modes:
                let (val2, shift_carry) = if inst_type == DataProc0
                        || inst_type == DataProc1 {
                    let r = bit(inst, 4);
                    debug_assert!((r == 1) == (inst_type == DataProc1));
                    let rm = extract(inst, 0, 4) as Reg;
                    let shift_type = extract(inst, 5, 2);

                    let shift = if inst_type == DataProc0 {
                        extract(inst, 7, 5)
                    } else {
                        let rs = extract(inst, 8, 4) as Reg;
                        debug_assert!(rs <= 14u8);

                        self.reg[rs] & 0xffu32
                    };

                    let valm = self.reg[rm]
                        + if rm == PC_REG { 8 + 4 * r } else { 0 };

                    if r == 0 && shift == 0 {
                        match shift_type {
                            0 /* LSL */ => (valm, c),
                            1 /* LSR */ => shift_lsr(valm, 32),
                            2 /* ASR */ => shift_asr(valm, 32),
                            3 /* ROR */ => {
                                // in this case its RRX#1
                                // so we rotate right by one and shift the
                                // carry bit in
                                ((valm >> 1) | (c << 31),
                                 bit(valm, 0))
                            },
                            _ => panic!(),
                        }
                    } else if shift != 0 {
                        match shift_type {
                            0 => shift_lsl(valm, shift),
                            1 => shift_lsr(valm, shift),
                            2 => shift_asr(valm, shift),
                            3 => shift_ror(valm, shift),
                            _ => panic!(),
                        }
                    } else {
                        // [Rs] == 0, so we do nothing
                        (valm, c)
                    }
                } else {
                    let shift = extract(inst, 8, 4) * 2;
                    let imm = extract(inst, 0, 8);
                    rotate_right(imm, shift)
                };

                let valn = self.reg[rn]
                    + if rn == PC_REG { 8 + 4 * r } else { 0 };
            }

            _ => (),
        };

        self.reg[PC_REG] += 4;
    }
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test_decode() {
        use super::Instruction::*;
        assert_eq!(
            DataProc0,
            Instruction::decode(0b11100000000101100111000001100011u32)
        );
    }
}
