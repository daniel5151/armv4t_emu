use super::*;
use super::bit_util::*;

#[derive(Clone, Copy, PartialEq, Debug)]
enum Instruction {
    BranchEx,
    Branch,
    DataProc0,
    DataProc1,
    DataProc2,
    PsrImm,
    PsrReg,
    Multiply,
    MulLong,
    Invalid,
}

const INST_MATCH_ORDER: [Instruction; 10] = [
    Instruction::BranchEx,
    Instruction::Branch,
    Instruction::PsrImm,
    Instruction::PsrReg,
    Instruction::Multiply,
    Instruction::MulLong,
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
            BranchEx    => (0x0fffffd0, 0x012fff10),
            Branch      => (0x0e000000, 0x0a000000),
            DataProc0   => (0x0e000010, 0x00000000),
            DataProc1   => (0x0e000090, 0x00000010),
            DataProc2   => (0x0e000000, 0x02000010),
            PsrImm      => (0x0fb00000, 0x03200000),
            PsrReg      => (0x0f900ff0, 0x01000000),
            Multiply    => (0x0fc000f0, 0x00000090),
            MulLong     => (0x0f8000f0, 0x00800090),
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
    let z = bit(cpsr, cpsr::Z);
    let c = bit(cpsr, cpsr::C);
    let v = bit(cpsr, cpsr::V);
    let n = bit(cpsr, cpsr::N);

    match cond {
        0x0 => z == 1,
        0x1 => z == 0,
        0x2 => c == 1,
        0x3 => c == 0,
        0x4 => n == 1,
        0x5 => n == 0,
        0x6 => v == 1,
        0x7 => v == 0,
        0x8 => c == 1 && z == 0,
        0x9 => c == 0 || z == 1,
        0xA => n == v,
        0xB => n != v,
        0xC => z == 0 && n == v,
        0xD => z == 1 || n != v,
        0xE => true,
        0xF => true, /* reserved, default to execute */
        _ => panic!(),
    }
}

fn build_flags(v: u32, c: u32, z: u32, n: u32) -> u32 {
    (v & 1) << 0 | (c & 1) << 1 | (z & 1) << 2 | (n & 1) << 3
}

pub trait ArmIsaCpu {
    fn execute(&mut self);
}

impl ArmIsaCpu for Cpu {
    fn execute(&mut self) {
        let pc = self.reg[reg::PC];
        let inst = self.mmu.load32(pc);
        // Decode first
        let cond = cond_code(inst);

        let cpsr = self.reg[reg::PC];

        if !cond_met(cond, cpsr) {
            self.reg[reg::PC] += 4;
            return;
        }

        use self::Instruction::*;
        let inst_type = self::Instruction::decode(inst);
        println!("Instruction: {:?}", inst_type);
        match inst_type {
            BranchEx => {
                let rn = extract(inst, 0, 4) as Reg;
                let new_pc = self.reg[rn];
                self.reg[reg::PC] = self.reg[rn] & !1u32;
                // maybe switch to thumb mode
                self.reg[reg::CPSR] = self.reg[reg::CPSR] | (bit(new_pc, 0) << cpsr::T);
            }
            Branch => {
                let l = bit(inst, 24);

                let offset = extract(inst, 0, 24);
                // Shift up to sign extend
                // shift right by 6 (instead of 8) to multiply by 4
                let s_offset = ((offset << 8) as i32 >> 6) as u32;
                let old_pc = self.reg[reg::PC];
                self.reg[reg::PC] = old_pc.wrapping_add(s_offset) + 4;
                if l != 0 {
                    self.reg[reg::LR] = old_pc + 4;
                }
            }
            DataProc0 | DataProc1 | DataProc2 => {
                let i = bit(inst, 25);
                let s = bit(inst, 20);
                let r = bit(inst, 4);

                let c = bit(cpsr, cpsr::C);
                let v = bit(cpsr, cpsr::V);

                let opcode = extract(inst, 21, 4);

                let rn = extract(inst, 16, 4) as Reg;
                let rd = extract(inst, 12, 4) as Reg;

                // Three modes:
                let (valm, shift_carry) = if inst_type == DataProc0 || inst_type == DataProc1 {
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

                    let valm = self.reg[rm] + if rm == reg::PC { 8 + 4 * r } else { 0 };

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
                    shift_ror(imm, shift)
                };

                let valn = self.reg[rn] +
                    if rn == reg::PC {
                        8 + 4 * (i == 0 && r == 1) as u32
                    } else {
                        0
                    };

                // execute the instruction now
                let (res, new_v, new_c) = match opcode {
                    0x0 /* AND */ |
                    0x8 /* TST */ => (valn & valm, v, shift_carry),
                    0x1 /* EOR */ |
                    0x9 /* TEQ */ => (valn ^ valm, v, shift_carry),
                    0x2 /* SUB */ |
                    0xA /* CMP */ => sub_flags(valn, valm, 0),
                    0x3 /* RSB */ => sub_flags(valm, valn, 0),
                    0x4 /* ADD */ |
                    0xB /* CMN */ => add_flags(valn, valm, 0),
                    0x5 /* ADC */ => add_flags(valn, valm, c),
                    0x6 /* SBC */ => sub_flags(valn, valm, 1-c),
                    0x7 /* RSC */ => sub_flags(valm, valn, 1-c),
                    0xC /* ORR */ => (valn | valm, v, shift_carry),
                    0xD /* MOV */ => (valm, v, shift_carry),
                    0xE /* BIC */ => (valn & !valm, v, shift_carry),
                    0xF /* MVN */ => (!valm, v, shift_carry),
                    _ => panic!(),
                };

                if s == 1 {
                    if true || rd != reg::PC {
                        let new_z = (res == 0) as u32;
                        let new_n = is_neg(res) as u32;
                        let new_flags = build_flags(new_v, new_c, new_z, new_n);
                        self.reg[reg::CPSR] = set(self.reg[reg::CPSR], 28, 4, new_flags);
                    } else {
                        // FIXME: do SPSR registers
                    }
                }

                match opcode {
                    0x8 | 0x9 | 0xA | 0xB => (), // no writeback
                    _ => self.reg[rd] = res,
                }
            }
            PsrImm | PsrReg => {
                // FIXME: requires SPSR registers and stuff
                panic!();
            }
            Multiply => {
                let a = bit(inst, 21);
                let s = bit(inst, 20);

                let rd = extract(inst, 16, 4) as Reg;
                let rn = extract(inst, 12, 4) as Reg;
                let rs = extract(inst, 8, 4) as Reg;
                let rm = extract(inst, 0, 4) as Reg;

                let res = self.reg[rm].wrapping_mul(self.reg[rs]).wrapping_add(
                    if a == 0 {
                        0
                    } else {
                        self.reg[rn]
                    },
                );

                self.reg[rd] = res;

                if s == 1 {
                    let v = bit(cpsr, cpsr::V);
                    let new_z = (res == 0) as u32;
                    let new_n = bit(res, 31);
                    let new_flags = build_flags(v, 0, new_z, new_n);

                    self.reg[reg::CPSR] = set(self.reg[reg::CPSR], 28, 4, new_flags);
                }
            }
            MulLong => {
                let u = bit(inst, 22);
                let a = bit(inst, 21);
                let s = bit(inst, 20);

                let rdhi = extract(inst, 16, 4) as Reg;
                let rdlo = extract(inst, 12, 4) as Reg;
                let rs = extract(inst, 8, 4) as Reg;
                let rm = extract(inst, 0, 4) as Reg;

                let res: u64 = if u == 0 {
                    let vs = self.reg[rs];
                    let vm = self.reg[rm];

                    let prod = (vs as u64) * (vm as u64);
                    prod.wrapping_add(if a == 0 {
                        0u64
                    } else {
                        combine64(self.reg[rdhi], self.reg[rdlo])
                    })
                } else {
                    let vs = self.reg[rs] as i32;
                    let vm = self.reg[rm] as i32;

                    let prod = (vs as i64) * (vm as i64);
                    prod.wrapping_add(if a == 0 {
                        0i64
                    } else {
                        combine64(self.reg[rdhi], self.reg[rdlo]) as i64
                    }) as u64
                };

                let (reshi, reslo) = split64(res);
                self.reg[rdhi] = reshi;
                self.reg[rdlo] = reslo;

                if s != 0 {
                    let new_z = (res == 0) as u32;
                    let new_n = bit(reshi, 31);
                    let new_flags = build_flags(0, 0, new_z, new_n);

                    self.reg[reg::CPSR] = set(self.reg[reg::CPSR], 28, 4, new_flags);
                }
            }
            Invalid => panic!(),
        };

        self.reg[reg::PC] += 4;
    }
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    #[cfg_attr(rustfmt, rustfmt_skip)]
    fn test_decode() {
        use super::Instruction::*;

        // Instructions assembled by unicorn.js
        assert_eq!(BranchEx,    Instruction::decode(0xE12FFF1C));
        assert_eq!(Branch,      Instruction::decode(0xEB0000F8));
        assert_eq!(DataProc0,   Instruction::decode(0xE1A0816C));
        assert_eq!(DataProc1,   Instruction::decode(0xE0923011));
        assert_eq!(DataProc1,   Instruction::decode(0xC0923011));
        assert_eq!(DataProc2,   Instruction::decode(0xE2A23AFF));
        assert_eq!(PsrImm,      Instruction::decode(0x1329F000));
        assert_eq!(PsrReg,      Instruction::decode(0xE10FA000));
        assert_eq!(Multiply,    Instruction::decode(0x80040393));
        assert_eq!(MulLong,     Instruction::decode(0xE0834192));
    }
}
