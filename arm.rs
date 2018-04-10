use super::*;
use super::bit_util::*;
use super::reg::*;

#[derive(Clone, Copy, PartialEq, Debug)]
enum Instruction {
    BranchEx, // Branch and exchange (i.e. switch to THUMB)
    Branch,
    DataProc0,
    DataProc1,
    DataProc2,
    PsrImm,
    PsrReg,
    Multiply,
    MulLong,
    SingleXferI, // Single data transfer, immediate offset
    SingleXferR, // Single data transfer, register offset
    HwSgnXferR, // Halfword and signed, register offset
    HwSgnXferI, // Halfword and signed, immediate offset
    BlockXfer,
    Invalid,
}

const INST_MATCH_ORDER: [Instruction; 15] = [
    Instruction::Branch,
    Instruction::BranchEx,
    Instruction::PsrImm,
    Instruction::PsrReg,
    Instruction::DataProc0,
    Instruction::DataProc1,
    Instruction::DataProc2,
    Instruction::Multiply,
    Instruction::MulLong,
    Instruction::SingleXferI,
    Instruction::SingleXferR,
    Instruction::HwSgnXferR,
    Instruction::HwSgnXferI,
    Instruction::BlockXfer,
    Instruction::Invalid,
];

impl Instruction {
    fn pattern(&self) -> (u32, u32) {
        use self::Instruction::*;
        #[cfg_attr(rustfmt, rustfmt_skip)]
        match *self {
            BranchEx    => (0x0ffffff0, 0x012fff10),
            Branch      => (0x0e000000, 0x0a000000),
            DataProc0   => (0x0e000010, 0x00000000),
            DataProc1   => (0x0e000090, 0x00000010),
            DataProc2   => (0x0e000000, 0x02000000),
            PsrImm      => (0x0fb00000, 0x03200000),
            PsrReg      => (0x0f900ff0, 0x01000000),
            Multiply    => (0x0fc000f0, 0x00000090),
            MulLong     => (0x0f8000f0, 0x00800090),
            SingleXferI => (0x0e000000, 0x04000000),
            SingleXferR => (0x0e000010, 0x06000000),
            HwSgnXferR  => (0x0e400f90, 0x00000090),
            HwSgnXferI  => (0x0e400090, 0x00400090),
            BlockXfer   => (0x0e000000, 0x08000000),
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

/// Compute the shifted value and shift carry when shift != 0
fn arg_shift(val: u32, shift: u32, shift_type: u32) -> (u32, u32) {
    debug_assert!(shift != 0);
    match shift_type {
        0 => shift_lsl(val, shift),
        1 => shift_lsr(val, shift),
        2 => shift_asr(val, shift),
        3 => shift_ror(val, shift),
        _ => panic!(),
    }
}

/// Compute the shifted value and shift carry when shift == 0
/// ARM has special logic encoded for when shift is 0, which requires
/// the previous carry in some cases
fn arg_shift0(val: u32, shift_type: u32, c: u32) -> (u32, u32) {
    match shift_type {
        0 /* LSL */ => (val, c),
        1 /* LSR */ => shift_lsr(val, 32),
        2 /* ASR */ => shift_asr(val, 32),
        3 /* ROR */ => {
            // in this case its RRX#1
            // so we rotate right by one and shift the
            // carry bit in
            ((val >> 1) | (c << 31),
             bit(val, 0))
        },
        _ => panic!(),
    }
}

pub trait ArmIsaCpu {
    /// Executes one instruction and returns whether the CPU should continue
    /// executing.
    fn execute(&mut self) -> bool;
}

impl ArmIsaCpu for Cpu {
    fn execute(&mut self) -> bool {
        let pc = self.reg[reg::PC];
        let inst = self.mmu.load32(pc);
        // Decode first
        let cond = extract(inst, 28, 4);

        let cpsr = self.reg[reg::CPSR];
        let cflags = extract(cpsr, 28, 4);

        debug!(
            "pc: {:#010x}, inst: {:#010x}, cond: {:#03x}, cflags: {:04b}",
            pc,
            inst,
            cond,
            cflags
        );

        if !cond_met(cond, cpsr) {
            debug!("cond not met");
            self.reg[reg::PC] = self.reg[reg::PC].wrapping_add(4);
            return true;
        }

        use self::Instruction::*;
        let inst_type = self::Instruction::decode(inst);
        debug!("Instruction: {:?}", inst_type);
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
                self.reg[reg::PC] = old_pc.wrapping_add(s_offset).wrapping_add(4);
                if l != 0 {
                    self.reg[reg::LR] = old_pc.wrapping_add(4);
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

                    let valm = self.reg[rm].wrapping_add(((rm == reg::PC) as u32) * (8 + 4 * r));

                    if r == 0 && shift == 0 {
                        arg_shift0(valm, shift_type, c)
                    } else if shift != 0 {
                        arg_shift(valm, shift, shift_type)
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
            SingleXferI | SingleXferR => {
                let p = bit(inst, 24);
                let u = bit(inst, 23);
                let b = bit(inst, 22);
                let w = bit(inst, 21);
                let l = bit(inst, 20);

                let rn = extract(inst, 16, 4) as Reg;
                let rd = extract(inst, 12, 4) as Reg;

                let offset = if inst_type == SingleXferI {
                    extract(inst, 0, 12)
                } else {
                    // We use the same logic here as for DataProc0
                    let shift = extract(inst, 7, 5);
                    let shift_type = extract(inst, 5, 2);

                    let rm = extract(inst, 0, 4) as Reg;

                    let valm = self.reg[rm];

                    let (shifted, _) = if shift == 0 {
                        let c = bit(cpsr, cpsr::C);
                        arg_shift0(valm, shift_type, 0)
                    } else {
                        arg_shift(valm, shift, shift_type)
                    };
                    shifted
                };

                let base = self.reg[rn].wrapping_add(((rn == reg::PC) as u32) * 8);
                let post_addr = if u == 0 {
                    base.wrapping_sub(offset)
                } else {
                    base.wrapping_add(offset)
                };

                let addr = if p == 0 { base } else { post_addr };

                if l == 0 {
                    // store
                    let val = self.reg[rd].wrapping_add(((rd == reg::PC) as u32) * 12);
                    if b == 0 {
                        // force alignment of the store
                        self.mmu.set32(addr & !3, val);
                    } else {
                        self.mmu.set8(addr, val as u8);
                    };
                } else {
                    self.reg[rd] = if b == 0 {
                        let val = self.mmu.load32(addr & !3);
                        // we need to rotate it so the addressed offset is
                        // at the base
                        let offset = addr & 3;
                        val.rotate_right(offset * 8)
                    } else {
                        self.mmu.load8(addr) as u32
                    };
                };

                // post-indexing implies writeback
                if p == 0 || w == 1 {
                    self.reg[rn] = post_addr;
                }
            }
            HwSgnXferR | HwSgnXferI => {
                let p = bit(inst, 24);
                let u = bit(inst, 23);
                let w = bit(inst, 21);
                let l = bit(inst, 20);

                let s = bit(inst, 6);
                let h = bit(inst, 5);

                let rn = extract(inst, 16, 4) as Reg;
                let rd = extract(inst, 12, 4) as Reg;

                let offset = if inst_type == HwSgnXferR {
                    let rn = extract(inst, 0, 4) as Reg;
                    self.reg[rn]
                } else {
                    (extract(inst, 8, 4) << 4) | extract(inst, 0, 4)
                };

                let base = self.reg[rn].wrapping_add(((rn == reg::PC) as u32) * 8);
                let post_addr = if u == 0 {
                    base.wrapping_sub(offset)
                } else {
                    base.wrapping_add(offset)
                };

                let addr = if p == 0 { base } else { post_addr };

                if l == 0 {
                    // store
                    debug_assert!(s == 0 && h == 1);
                    let val = self.reg[rd].wrapping_add(((rd == reg::PC) as u32) * 12);
                    self.mmu.set16(addr, val as u16);
                } else {
                    self.reg[rd] = match (s, h) {
                        (0, 0) /* SWP */ => panic!(),
                        (0, 1) /* halfword load */  => self.mmu.load16(addr) as u32,
                        (1, 0) /* signed byte */    => {
                            self.mmu.load8(addr) as i8 as i32 as u32
                        },
                        (1, 1) /* signed half */    => {
                            self.mmu.load16(addr) as i16 as i32 as u32
                        },
                        _ => panic!()
                    };
                };

                // post-indexing implies writeback
                if p == 0 || w == 1 {
                    self.reg[rn] = post_addr;
                }
            }
            BlockXfer => panic!(),
            Invalid => return false,
        };

        self.reg[reg::PC] = self.reg[reg::PC].wrapping_add(4);
        true
    }
}

#[cfg(test)]
mod test {
    use env_logger;

    use super::*;
    #[test]
    #[cfg_attr(rustfmt, rustfmt_skip)]
    fn test_decode() {
        use super::Instruction::*;

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
        assert_eq!(SingleXferI, Instruction::decode(0x85C67011));
        assert_eq!(SingleXferR, Instruction::decode(0xB7965100));
        assert_eq!(HwSgnXferR,  Instruction::decode(0xE10B00B1));
        assert_eq!(HwSgnXferI,  Instruction::decode(0xE1EB10B4));
        assert_eq!(BlockXfer,   Instruction::decode(0xE8BF8006));
        assert_eq!(Invalid,     Instruction::decode(0xEFDEAD10));
    }

    use std::sync::{Once, ONCE_INIT};
    static INIT: Once = ONCE_INIT;


    macro_rules! emutest {
        ($name:ident, $mem_checks: expr) => {
            #[test]
            fn $name () {
                use std::boxed::Box;

                use mmu::raw::Raw;

                INIT.call_once(env_logger::init);

                let prog = include_bytes!(concat!("testdata/",
                                                  stringify!($name),
                                                  ".bin"));
                let mmu = Raw::new_with_data(0x1000, prog);
                let mut cpu = super::Cpu::new(Box::new(mmu), 0x0);
                cpu.run();

                let mem = cpu.memory();
                for &(addr, val) in ($mem_checks).iter() {
                    assert_eq!(val, mem.load32(addr), "addr: {:#010x}", addr);
                }
            }
        }
    }

    emutest!(emutest_arm0, [(0x100, 5), (0x104, 0)]);
    emutest!(emutest_arm1, [(0x100, 5), (0x104, 5), (0x108, 5)]);
    emutest!(
        emutest_arm2,
        [(0x100, 6), (0x104, 0x200000e1), (0x108, 0xe100001c)]
    );
    emutest!(emutest_arm3, [(0x100, 64)]);
    emutest!(
        emutest_arm4,
        [
            (0x100, 6),
            (0x104, 0x200000e1),
            (0x108, 0xe100001c),
            (0x10c, 6),
            (0x110, 6 * 0x100),
        ]
    );
    emutest!(
        emutest_arm5,
        [(0x100, 0xf000), (0x104, 0xfff0), (0x108, 0x104)]
    );
    emutest!(
        emutest_arm6,
        [
            (0x1f4, 0xa),
            (0x1f8, 0xc),
            (0x1fc, 0x10),
            (0x200, 6),
            (0x204, 0x200),
        ]
    );
}
