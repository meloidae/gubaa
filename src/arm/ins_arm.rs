use super::{Condition, ArmCore, REG_PC, StatusRegister};

use crate::arm::common::{lsl_carry, lsl_no_carry, lsr_carry, lsr_no_carry, asr_carry, asr_no_carry,
    ror_carry, ror_no_carry, and, eor, add, adc, sub, sbc, orr, mov, bic, mvn, set_nz, set_nz_long,
    process_bit_format, specs_matches, err, DisResult, IndexBitPair};

use num_traits::FromPrimitive;

#[derive(Clone, Copy)]
pub struct ArmIns(u32);

impl ArmIns {
    pub fn new(bits: u32) -> ArmIns {
        ArmIns(bits)
    }

    fn slice(&self, start: u32, end: u32) -> u32 {
        let left = 32 - end;
        (self.0 << left) >> start
    }

    fn reg(&self, offset: u32) -> usize { 
        self.slice(offset, offset + 4) as usize
    }

    fn flag(&self, offset: u32) -> bool {
        self.slice(offset, offset + 1) != 0
    }

    fn cond(&self) -> Condition {
        Condition::from_u32(self.slice(28, 32)).unwrap()
    }

    pub fn discriminant(&self) -> u32 {
        // self.slice(4, 8) | self.slice(16, 17) << 4 | self.slice(20, 28) << 5
        self.slice(4, 8) | self.slice(20, 28) << 4
    }
}

pub type ArmFn = fn(&mut ArmCore, ArmIns);

fn get_rotated_immediate(arm: &mut ArmCore, ins: ArmIns, set_carry: bool) -> u32 {
    let value = ins.slice(0, 8);
    let rotate = ins.slice(8, 12) << 1;
    if rotate != 0 {
        if set_carry {
            arm.cpsr.c = (value >> (rotate - 1)) != 0;
        }
        value.rotate_right(rotate)
    } else {
        value
    }
}


fn get_shifted_register(arm: &mut ArmCore, ins: ArmIns, set_carry: bool) -> u32 {
    const LSL: u32 = 0b00;
    const LSR: u32 = 0b01;
    const ASR: u32 = 0b10;
    const ROR: u32 = 0b11;
    let shift_imm = ins.flag(4);
    let shift_type = ins.slice(5, 7);
    let shift = if shift_imm {
        ins.slice(7, 12)
    } else {
        arm.regs[ins.reg(8)] & 0xff // take bottom byte
    };
    let rm_value = arm.regs[ins.reg(0)];

    match (shift_type, set_carry) {
        (LSL, true) => lsl_carry(arm, rm_value, shift),
        (LSL, false) => lsl_no_carry(arm, rm_value, shift),
        (LSR, true) => lsr_carry(arm, rm_value, shift),
        (LSR, false) => lsr_no_carry(arm, rm_value, shift),
        (ASR, true) => asr_carry(arm, rm_value, shift),
        (ASR, false) => asr_no_carry(arm, rm_value, shift),
        (ROR, true) => ror_carry(arm, rm_value, shift),
        (ROR, false) => ror_no_carry(arm, rm_value, shift),
        _ => unreachable!()
    }
}


fn undefined(arm: &mut ArmCore, _: ArmIns) {
}


fn data_processing(arm: &mut ArmCore, ins: ArmIns) {
    const AND: u32 = 0b0000;
    const EOR: u32 = 0b0001;
    const SUB: u32 = 0b0010;
    const RSB: u32 = 0b0011;
    const ADD: u32 = 0b0100;
    const ADC: u32 = 0b0101;
    const SBC: u32 = 0b0110;
    const RSC: u32 = 0b0111;
    const TST: u32 = 0b1000;
    const TEQ: u32 = 0b1001;
    const CMP: u32 = 0b1010;
    const CMN: u32 = 0b1011;
    const ORR: u32 = 0b1100;
    const MOV: u32 = 0b1101;
    const BIC: u32 = 0b1110;
    const MVN: u32 = 0b1111;

    let op_code = ins.slice(21, 25);
    let rn_idx = ins.reg(16);
    let rd_idx = ins.reg(12);
    let imm = ins.flag(25);
    let set_flags = ins.flag(20);

    let op1 = arm.regs[rn_idx];
    let op2 = if imm {
        get_rotated_immediate(arm, ins, set_flags)
    } else {
        get_shifted_register(arm, ins, set_flags)
    };

    if rd_idx == REG_PC && set_flags {
        // TODO
    }

    // Calculate result of operation
    let result = match op_code {
        AND | TST => and(arm, op1, op2, set_flags),
        EOR | TEQ => eor(arm, op1, op2, set_flags),
        SUB | CMP => sub(arm, op1, op2, set_flags),
        RSB => sub(arm, op2, op1, set_flags),
        ADD | CMN => add(arm, op1, op2, set_flags),
        ADC => adc(arm, op1, op2, set_flags),
        SBC => sbc(arm, op1, op2, set_flags),
        RSC => sbc(arm, op2, op1, set_flags),
        ORR => orr(arm, op1, op2, set_flags),
        MOV => mov(arm, op1, op2, set_flags),
        BIC => bic(arm, op1, op2, set_flags),
        MVN => mvn(arm, op1, op2, set_flags),
        _ => unreachable!(),
    };

    match op_code {
        TST | TEQ | CMP | CMN => {},
        _ => arm.set_reg(rd_idx, result),
    };

}


// psr transfer

fn mrs(arm: &mut ArmCore, ins: ArmIns) {
    if ins.slice(0, 12) != 0 || ins.slice(16, 20) != 0xF {
        undefined(arm, ins);
    }

    let spsr_flag = ins.flag(22);
    let rd_idx = ins.reg(12);

    assert!(rd_idx != REG_PC);

    let value: u32 = if spsr_flag {
        if let Some(spsr) = arm.get_spsr() {
            spsr.into()
        } else {
            // TODO: proper error logging
            print!("Tried to get SPSR in mode {:?} that has no SPSR", arm.cpsr.mode);
            0
        }
    } else {
        arm.cpsr.into()
    };

    arm.regs[rd_idx] = value;
}

fn msr_reg(arm: &mut ArmCore, ins: ArmIns) {
    if ins.slice(8, 20) & !0x100 != 0b1000_1111_0000 {
        undefined(arm, ins);
    }
    let flags_only = !ins.flag(16);
    if flags_only {
        msr_reg_flag(arm, ins);
    } else {
        msr_reg_all(arm, ins);
    }
}

fn msr_reg_all(arm: &mut ArmCore, ins: ArmIns) {
    let spsr_flag = ins.flag(22);
    let rm_idx = ins.reg(0);

    assert!(rm_idx != REG_PC);

    let rm_value = arm.regs[rm_idx];

    if spsr_flag {
        if let Some(spsr) = arm.get_spsr_mut() {
            *spsr = rm_value.into();
        } else {
            // TODO: proper error logging
            print!("Tried to set SPSR in mode {:?} that has no SPSR", arm.cpsr.mode);
        }
    } else {
        arm.set_cpsr(rm_value);
    }
}

fn msr_reg_flag(arm: &mut ArmCore, ins: ArmIns) {
    let spsr_flag = ins.flag(22);
    let rm_idx = ins.reg(0);

    assert!(rm_idx != REG_PC);

    let rm_value = arm.regs[rm_idx];

    if spsr_flag {
        if let Some(spsr) = arm.get_spsr_mut() {
            spsr.set_flags(rm_value);
        } else  {
            // TODO: proper error logging
            print!("Tried to set SPSR in mode {:?} that has no SPSR", arm.cpsr.mode);
        }
    } else {
        arm.cpsr.set_flags(rm_value);
    }
}

fn msr_imm_flag(arm: &mut ArmCore, ins: ArmIns) {
    if ins.slice(12, 20) != 0b1000_1111 {
        undefined(arm, ins)
    }

    let spsr_flag = ins.flag(22);
    // TODO: should carry be set?
    let value = get_rotated_immediate(arm, ins, false);

    if spsr_flag {
        if let Some(spsr) = arm.get_spsr_mut() {
            spsr.set_flags(value);
        } else {
            // TODO: proper error logging
            print!("Tried to set SPSR in mode {:?} that has no SPSR", arm.cpsr.mode);
        }
    } else {
        arm.cpsr.set_flags(value);
    }
}

fn multiply(arm: &mut ArmCore, ins: ArmIns) {
    let rd_idx = ins.reg(16);
    let rn_idx = ins.reg(12);
    let rs_idx = ins.reg(8);
    let rm_idx = ins.reg(0);

    let set_flags = ins.flag(20);
    let acc = ins.flag(21);
    
    // restrictions on operand registers
    assert!(rd_idx != rm_idx);
    assert!(rd_idx != REG_PC);
    assert!(rs_idx != REG_PC);
    assert!(rn_idx != REG_PC);
    assert!(rm_idx != REG_PC);

    let rn = arm.regs[rn_idx];
    let rs = arm.regs[rs_idx];
    let rm = arm.regs[rm_idx];
    let result = if acc {
        // TODO: add cycles for accumulation
        rm.wrapping_mul(rs).wrapping_add(rn)
    } else {
        rm.wrapping_mul(rs)
    };

    if set_flags {
        // c is set to a meaningless value, v is unaffected
        set_nz(arm, result);
    }

}

fn multiply_long(arm: &mut ArmCore, ins: ArmIns) {
    let rdhi_idx = ins.reg(16);
    let rdlo_idx = ins.reg(12);
    let rs_idx = ins.reg(8);
    let rm_idx = ins.reg(0);

    let signed = ins.flag(22);
    let acc = ins.flag(21);
    let set_flags = ins.flag(20);

    assert!(rdhi_idx != REG_PC);
    assert!(rdlo_idx != REG_PC);
    assert!(rs_idx != REG_PC);
    assert!(rm_idx != REG_PC);
    assert!(rdhi_idx != rdlo_idx);
    assert!(rdhi_idx != rm_idx);
    assert!(rdlo_idx != rm_idx);

    let result: u64 = if signed {
        let rm = arm.regs[rm_idx] as i32 as i64;
        let rs = arm.regs[rs_idx] as i32 as i64;
        if acc {
            let rdhi = arm.regs[rdhi_idx] as i32 as i64;
            let rdlo = arm.regs[rdlo_idx] as i32 as i64;
            let rdfull = rdlo | (rdhi << 32);
            rm.wrapping_mul(rs).wrapping_add(rdfull) as u64
        } else {
            rm.wrapping_mul(rs) as u64
        }
    } else {
        let rm = arm.regs[rm_idx] as u64;
        let rs = arm.regs[rs_idx] as u64;
        if acc {
            let rdhi = arm.regs[rdhi_idx] as u64;
            let rdlo = arm.regs[rdlo_idx] as u64;
            let rdfull = rdlo | (rdhi << 32);
            rm.wrapping_mul(rs).wrapping_add(rdfull)
        } else {
            rm.wrapping_mul(rs)
        }
    };

    if set_flags {
        // c and v are set to meaningless values
        set_nz_long(arm, result);
    }

    arm.regs[rdhi_idx] = (result >> 32) as u32;
    arm.regs[rdlo_idx] = result as u32;
}

fn single_data_swap(arm: &mut ArmCore, ins: ArmIns) {
    let byte = ins.flag(8);
}

fn single_data_swap_word(arm: &mut ArmCore, ins: ArmIns) {
    let rn_idx = ins.reg(16);
    let rd_idx = ins.reg(12);
    let rm_idx = ins.reg(0);

    assert!(rn_idx != REG_PC);
    assert!(rd_idx != REG_PC);
    assert!(rm_idx != REG_PC);
}

fn single_data_swap_byte(arm: &mut ArmCore, ins: ArmIns) {
    let rn_idx = ins.reg(16);
    let rd_idx = ins.reg(12);
    let rm_idx = ins.reg(0);

    assert!(rn_idx != REG_PC);
    assert!(rd_idx != REG_PC);
    assert!(rm_idx != REG_PC);
}


// TODO
fn branch_and_exchange(arm: &mut ArmCore, ins: ArmIns) {
    let address = arm.regs[ins.reg(0)];
    // if address & 0x1 {
    // } else {
    // }
}

fn halfword_data_transfer(arm: &mut ArmCore, ins: ArmIns) {
}


fn single_data_transfer(arm: &mut ArmCore, ins: ArmIns) {
    let rd_idx = ins.reg(12);
    let rn_idx = ins.reg(16);

    let imm = !ins.flag(25);
    let preindex = ins.flag(24);
    let up = ins.flag(23);
    let byte = ins.flag(22);
    let writeback = ins.flag(21);
    let load = ins.flag(20);

    let offset = if imm {
        ins.slice(0, 12)
    } else {
        get_shifted_register(arm, ins, false)
    };
}

fn block_data_transfer(arm: &mut ArmCore, ins: ArmIns) {
}

fn branch(arm: &mut ArmCore, ins: ArmIns) {
}

fn software_interrupt(arm: &mut ArmCore, ins: ArmIns) {
}

struct ArmSpecs<'a>(Vec<IndexBitPair>, &'a str, ArmFn);

impl<'a> ArmSpecs<'a> {
    fn try_match_discriminant(&self, disc: u32) -> Option<ArmFn> {
        if specs_matches(&self.0, disc) {
            Some(self.2)
        } else {
            None
        }
    }
}

struct ArmLookupTable {
    disc2idx: Vec<u8>,
    idx2fn: Vec<ArmFn>,
}


impl ArmLookupTable {
    pub fn compute() -> ArmLookupTable {
        let arm_specs_table = ARM_PATTERN_TABLE.iter()
            .map(|&(fmt, syntax, arm_fn)| ArmSpecs(process_arm_format(fmt), syntax, arm_fn) )
            .collect::<Vec<ArmSpecs>>();

        let mut arm_fns = Vec::<ArmFn>::new();
        let disc2idx = (0..=0xFFF * 2).map(|disc| {
            let matched_fns = arm_specs_table.iter()
                .filter_map(|s| s.try_match_discriminant(disc))
                .collect::<Vec<ArmFn>>();
            let n_matches = matched_fns.len();
            let matched_fn: ArmFn = match n_matches {
                0 => Ok(undefined as ArmFn),
                1 => Ok(matched_fns[0]),
                _ => Err(err(format!("{} matching specs for discriminant {:03X}", n_matches, disc))),
            }?;
            Ok(arm_fns.iter()
               .position(|&f| f as usize == matched_fn as usize) // compare function pointers
               .unwrap_or_else(|| {
                   arm_fns.push(matched_fn);
                   arm_fns.len() - 1
               }) as u8)
        }).collect::<DisResult<Vec<u8>>>().unwrap();

        ArmLookupTable{disc2idx: disc2idx, idx2fn: arm_fns}
    }
}

fn process_arm_format(fmt: &str) -> Vec<IndexBitPair> {
    process_bit_format(fmt, |i| (4 <= i && i < 8) || (20 <= i && i < 28))
}

pub static ARM_PATTERN_TABLE: &[(&str, &str, ArmFn)] = &[
    // data processing
    ("000 0000 S nnnn dddd iiii 0ii1 iiii", "AND<S> %Rn, %Rd, <op2>", data_processing),
    ("000 0000 S nnnn dddd iiii iii0 iiii", "AND<S> %Rn, %Rd, <op2>", data_processing),
    ("001 0000 S nnnn dddd iiii iiii iiii", "AND<S> %Rn, %Rd, <op2>", data_processing),
    ("000 0001 S nnnn dddd iiii 0ii1 iiii", "EOR<S> %Rn, %Rd, <op2>", data_processing),
    ("000 0001 S nnnn dddd iiii iii0 iiii", "EOR<S> %Rn, %Rd, <op2>", data_processing),
    ("001 0001 S nnnn dddd iiii iiii iiii", "EOR<S> %Rn, %Rd, <op2>", data_processing),
    ("000 0010 S nnnn dddd iiii 0ii1 iiii", "SUB<S> %Rn, %Rd, <op2>", data_processing),
    ("000 0010 S nnnn dddd iiii iii0 iiii", "SUB<S> %Rn, %Rd, <op2>", data_processing),
    ("001 0010 S nnnn dddd iiii iiii iiii", "SUB<S> %Rn, %Rd, <op2>", data_processing),
    ("000 0011 S nnnn dddd iiii 0ii1 iiii", "RSB<S> %Rn, %Rd, <op2>", data_processing),
    ("000 0011 S nnnn dddd iiii iii0 iiii", "RSB<S> %Rn, %Rd, <op2>", data_processing),
    ("001 0011 S nnnn dddd iiii iiii iiii", "RSB<S> %Rn, %Rd, <op2>", data_processing),
    ("000 0100 S nnnn dddd iiii 0ii1 iiii", "ADD<S> %Rn, %Rd, <op2>", data_processing),
    ("000 0100 S nnnn dddd iiii iii0 iiii", "ADD<S> %Rn, %Rd, <op2>", data_processing),
    ("001 0100 S nnnn dddd iiii iiii iiii", "ADD<S> %Rn, %Rd, <op2>", data_processing),
    ("000 0101 S nnnn dddd iiii 0ii1 iiii", "ADC<S> %Rn, %Rd, <op2>", data_processing),
    ("000 0101 S nnnn dddd iiii iii0 iiii", "ADC<S> %Rn, %Rd, <op2>", data_processing),
    ("001 0101 S nnnn dddd iiii iiii iiii", "ADC<S> %Rn, %Rd, <op2>", data_processing),
    ("000 0110 S nnnn dddd iiii 0ii1 iiii", "SBC<S> %Rn, %Rd, <op2>", data_processing),
    ("000 0110 S nnnn dddd iiii iii0 iiii", "SBC<S> %Rn, %Rd, <op2>", data_processing),
    ("001 0110 S nnnn dddd iiii iiii iiii", "SBC<S> %Rn, %Rd, <op2>", data_processing),
    ("000 0111 S nnnn dddd iiii 0ii1 iiii", "RSC<S> %Rn, %Rd, <op2>", data_processing),
    ("000 0111 S nnnn dddd iiii iii0 iiii", "RSC<S> %Rn, %Rd, <op2>", data_processing),
    ("001 0111 S nnnn dddd iiii iiii iiii", "RSC<S> %Rn, %Rd, <op2>", data_processing),
    ("000 1000 1 nnnn dddd iiii 0ii1 iiii", "TST %Rn, %Rd, <op2>", data_processing),
    ("000 1000 1 nnnn dddd iiii iii0 iiii", "TST %Rn, %Rd, <op2>", data_processing),
    ("001 1000 1 nnnn dddd iiii iiii iiii", "TST %Rn, %Rd, <op2>", data_processing),
    ("000 1001 1 nnnn dddd iiii 0ii1 iiii", "TEQ %Rn, %Rd, <op2>", data_processing),
    ("000 1001 1 nnnn dddd iiii iii0 iiii", "TEQ %Rn, %Rd, <op2>", data_processing),
    ("001 1001 1 nnnn dddd iiii iiii iiii", "TEQ %Rn, %Rd, <op2>", data_processing),
    ("000 1010 1 nnnn dddd iiii 0ii1 iiii", "CMP %Rn, %Rd, <op2>", data_processing),
    ("000 1010 1 nnnn dddd iiii iii0 iiii", "CMP %Rn, %Rd, <op2>", data_processing),
    ("001 1010 1 nnnn dddd iiii iiii iiii", "CMP %Rn, %Rd, <op2>", data_processing),
    ("000 1011 1 nnnn dddd iiii 0ii1 iiii", "CMN %Rn, %Rd, <op2>", data_processing),
    ("000 1011 1 nnnn dddd iiii iii0 iiii", "CMN %Rn, %Rd, <op2>", data_processing),
    ("001 1011 1 nnnn dddd iiii iiii iiii", "CMN %Rn, %Rd, <op2>", data_processing),
    ("000 1100 S nnnn dddd iiii 0ii1 iiii", "ORR<S> %Rn, %Rd, <op2>", data_processing),
    ("000 1100 S nnnn dddd iiii iii0 iiii", "ORR<S> %Rn, %Rd, <op2>", data_processing),
    ("001 1100 S nnnn dddd iiii iiii iiii", "ORR<S> %Rn, %Rd, <op2>", data_processing),
    ("000 1101 S nnnn dddd iiii 0ii1 iiii", "MOV<S> %Rn, %Rd, <op2>", data_processing),
    ("000 1101 S nnnn dddd iiii iii0 iiii", "MOV<S> %Rn, %Rd, <op2>", data_processing),
    ("001 1101 S nnnn dddd iiii iiii iiii", "MOV<S> %Rn, %Rd, <op2>", data_processing),
    ("000 1110 S nnnn dddd iiii 0ii1 iiii", "BIC<S> %Rn, %Rd, <op2>", data_processing),
    ("000 1110 S nnnn dddd iiii iii0 iiii", "BIC<S> %Rn, %Rd, <op2>", data_processing),
    ("001 1110 S nnnn dddd iiii iiii iiii", "BIC<S> %Rn, %Rd, <op2>", data_processing),
    ("000 1111 S nnnn dddd iiii 0ii1 iiii", "MVN<S> %Rn, %Rd, <op2>", data_processing),
    ("000 1111 S nnnn dddd iiii iii0 iiii", "MVN<S> %Rn, %Rd, <op2>", data_processing),
    ("001 1111 S nnnn dddd iiii iiii iiii", "MVN<S> %Rn, %Rd, <op2>", data_processing),


    // psr transfer
    ("0001 0 s 00 1111 dddd 0000 0000 0000", "MRS %Rd, <psr>", mrs),
    ("0001 0 d 10 100f 1111 0000 0000 mmmm", "MSR <psr>, %Rm", msr_reg),
    ("0011 0 d 10 1000 1111 rrrr iiii iiii", "MSR <psr>, <rot_imm>", msr_imm_flag),

    ("0000 000S dddd nnnn ssss 1001 mmmm", "MUL<S> %Rd, %Rm, %Rs", multiply),
    ("0000 001S dddd nnnn ssss 1001 mmmm", "MLA<S> %Rd, %Rm, %Rs, %Rn", multiply),

    ("0000 100S hhhh llll ssss 1001 mmmm", "UMULL<S> %Rl, %Rh, %Rm, %Rs", multiply_long),
    ("0000 101S hhhh llll ssss 1001 mmmm", "UMLAL<S> %Rl, %Rh, %Rm, %Rs", multiply_long),
    ("0000 110S hhhh llll ssss 1001 mmmm", "SMULL<S> %Rl, %Rh, %Rm, %Rs", multiply_long),
    ("0000 111S hhhh llll ssss 1001 mmmm", "SMLAL<S> %Rl, %Rh, %Rm, %Rs", multiply_long),

    ("0001 0000 nnnn dddd 0000 1001 mmmm", "SWP %Rd, %Rm [%Rn]", single_data_swap),
    ("0001 0100 nnnn dddd 0000 1001 mmmm", "SWPB %Rd, %Rm [%Rn]", single_data_swap),

    ("0001 0010 1111 1111 1111 0001 nnnn", "BX %Rn", branch_and_exchange),

    ("000 P U0W1 nnnn dddd 0000 1011 mmmm", "LDRH %Rd, [%Rn, %Rm]", halfword_data_transfer),
    ("000 P U0W1 nnnn dddd 0000 1101 mmmm", "LDRSB %Rd, [%Rn, %Rm]", halfword_data_transfer),
    ("000 P U0W1 nnnn dddd 0000 1111 mmmm", "LDRSH %Rd, [%Rn, %Rm]", halfword_data_transfer),
    ("000 P U0W0 nnnn dddd 0000 1111 mmmm", "STRH %Rd, [%Rn, %Rm]", halfword_data_transfer),

    ("000 P U1W1 nnnn dddd iiii 1011 iiii", "LDRH %Rd, [%Rn, #[i]]", halfword_data_transfer),
    ("000 P U1W1 nnnn dddd iiii 1101 iiii", "LDRSB %Rd, [%Rn, #[i]]", halfword_data_transfer),
    ("000 P U1W1 nnnn dddd iiii 1111 iiii", "LDRSH %Rd, [%Rn, #[i]]", halfword_data_transfer),
    ("000 P U1W0 nnnn dddd iiii 1111 iiii", "STRH %Rd, [%Rn, #[i]]", halfword_data_transfer),

    ("010 P U0W1 nnnn dddd oooo oooo oooo", "LDR %Rd, [%Rn, #offset[i]]", single_data_transfer),
    ("011 P U0W1 nnnn dddd oooo oooo oooo", "LDR %Rd, [%Rn, <op2_reg>]", single_data_transfer),
    ("010 P U1W1 nnnn dddd oooo oooo oooo", "LDRB %Rd, [%Rn, #offset[i]]", single_data_transfer),
    ("011 P U1W1 nnnn dddd oooo oooo oooo", "LDRB %Rd, [%Rn, <op2_reg>]", single_data_transfer),

    ("010 P U0W0 nnnn dddd oooo oooo oooo", "STR %Rd, [%Rn, #offset[i]]", single_data_transfer),
    ("011 P U0W0 nnnn dddd oooo oooo oooo", "STR %Rd, [%Rn, <op2_reg>]", single_data_transfer),
    ("010 P U1W0 nnnn dddd oooo oooo oooo", "STRB %Rd, [%Rn, #offset[i]]", single_data_transfer),
    ("011 P U1W0 nnnn dddd oooo oooo oooo", "STRB %Rd, [%Rn, <op2_reg>]", single_data_transfer),

    // Shouldn't occur
    // ("011 x xxxx xxxx xxxx xxxx xxx1 xxxx", "Undefined", |_: &mut ArmCore, _: ArmIns| {}),
    
    ("100 0 1SW1 nnnn rrrr rrrr rrrr rrrr", "LDMIA<S> %Rn<wb>, { %+Rr }", block_data_transfer),
    ("100 1 1SW1 nnnn rrrr rrrr rrrr rrrr", "LDMIB<S> %Rn<wb>, { %+Rr }", block_data_transfer),
    ("100 0 0SW1 nnnn rrrr rrrr rrrr rrrr", "LDMDA<S> %Rn<wb>, { %+Rr }", block_data_transfer),
    ("100 1 0SW1 nnnn rrrr rrrr rrrr rrrr", "LDMDB<S> %Rn<wb>, { %+Rr }", block_data_transfer),

    ("100 0 1SW0 nnnn rrrr rrrr rrrr rrrr", "STMIA<S> %Rn<wb>, { %+Rr }", block_data_transfer),
    ("100 1 1SW0 nnnn rrrr rrrr rrrr rrrr", "STMIB<S> %Rn<wb>, { %+Rr }", block_data_transfer),
    ("100 0 0SW0 nnnn rrrr rrrr rrrr rrrr", "STMDA<S> %Rn<wb>, { %+Rr }", block_data_transfer),
    ("100 1 0SW0 nnnn rrrr rrrr rrrr rrrr", "STMDB<S> %Rn<wb>, { %+Rr }", block_data_transfer),

    ("101 0 oooo oooo oooo oooo oooo oooo", "B $offset[o]", branch),
    ("101 1 oooo oooo oooo oooo oooo oooo", "BL $offset[o]", branch),

    // Shouldn't occur
    ("110P UNWL nnnn dddd #### oooo oooo", "CP_DATA_TRANS*", |_: &mut ArmCore, _: ArmIns| {}),
    ("1110 cccc nnnn dddd #### ppp0 mmmm", "CP_REG_OP*", |_: &mut ArmCore, _: ArmIns| {}),
    ("1110 cccL nnnn dddd #### ppp1 mmmm", "CP_DATA_OP*", |_: &mut ArmCore, _: ArmIns| {}),

    ("1111 iiii iiii iiii iiii iiii iiii", "SW #[i]", software_interrupt),
];

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test() {
        let lut = ArmLookupTable::compute();
        assert!(true);
    }
}
