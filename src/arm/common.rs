use std::error;

use super::{ArmCore};


pub type DisResult<T> = Result<T, Box<dyn error::Error>>;

// logical shift left and return (shifted_val, carry)
pub fn lsl_carry(arm: &mut ArmCore, rm: u32, shift: u32) -> u32 {
    if shift == 32 {
        arm.cpsr.c = (rm & 1) != 0;
        0
    } else if shift > 32 {
        arm.cpsr.c = false;
        0
    } else if shift != 0{
        arm.cpsr.c = ((rm >> (shift - 1)) & 1) != 0;
        rm << shift
    } else {  // no
        rm
    }
}

pub fn lsl_no_carry(_arm: &ArmCore, rm: u32, shift: u32) -> u32 {
    if shift >= 32 {
        0
    } else if shift != 0 {
        rm << shift
    } else {
        rm
    }
}

// logical shift right and return (shifted_val, carry)
pub fn lsr_carry(arm: &mut ArmCore, rm: u32, shift: u32) -> u32 {
    if shift == 32 || shift == 0 {
        arm.cpsr.c = (rm >> 31) != 0;
        0
    } else if shift > 32 {
        arm.cpsr.c = false;
        0
    } else {
        arm.cpsr.c = ((rm >> (shift - 1)) & 1) != 0;
        rm >> shift
    }
}

pub fn lsr_no_carry(_arm: &ArmCore, rm: u32, shift: u32) -> u32 {
    if shift >= 32 || shift == 0 {
        0
    } else {
        rm >> shift
    }
}

// arithematic shift right and return (shifted_val, carry)
pub fn asr_carry(arm: &mut ArmCore, rm: u32, shift: u32) -> u32 {
    if shift >= 32 || shift == 0 {
        if (rm >> 31) != 0 {
            arm.cpsr.c = true;
            0xffff_ffff
        } else {
            arm.cpsr.c = false;
            0
        }
    } else {
        arm.cpsr.c = (((rm as i32) >> (shift - 1)) & 1) != 0;
        ((rm as i32) >> shift) as u32
    }
}

pub fn asr_no_carry(_arm: &ArmCore, rm: u32, shift: u32) -> u32 {
    if shift >= 32 || shift == 0 {
        if (rm >> 31) != 0 {
            0xffff_ffff
        } else {
            0
        }
    } else {
        ((rm as i32) >> shift) as u32
    }
}

// rotate right and return (shifted_val, carry)
pub fn ror_carry(arm: &mut ArmCore, rm: u32, shift: u32) -> u32 {
    if shift == 0 { // rrx
        let carry_in = arm.cpsr.c as u32;
        arm.cpsr.c = (rm & 1) != 0;
        (rm >> 1) | (carry_in << 31)
    } else {
        arm.cpsr.c = ((rm >> ((shift - 1) & 0x1f)) & 1) != 0;
        rm.rotate_right(shift)
    }
}

pub fn ror_no_carry(arm: &ArmCore, rm: u32, shift: u32) -> u32 {
    if shift == 0 { // rrx
        (rm >> 1) | ((arm.cpsr.c as u32) << 31)
    } else {
        rm.rotate_right(shift)
    }
}

pub fn and(arm: &mut ArmCore, op1: u32, op2: u32, set_flag: bool) -> u32 {
    let result = op1 & op2;
    if set_flag {
        set_nz(arm, result);
    }
    result
}

pub fn eor(arm: &mut ArmCore, op1: u32, op2: u32, set_flag: bool) -> u32 {
    let result = op1 ^ op2;
    if set_flag {
        set_nz(arm, result);
    }
    result
}

pub fn add(arm: &mut ArmCore, op1: u32, op2: u32, set_flag: bool) -> u32 {
    let (result, carry) = op1.overflowing_add(op2);
    if set_flag {
        arm.cpsr.v = (op1 as i32).overflowing_add(op2 as i32).1;
        arm.cpsr.c = carry;
        set_nz(arm, result);
    }
    result
}

pub fn adc(arm: &mut ArmCore, op1: u32, op2: u32, set_flag: bool) -> u32 {
    let (result1, c1) = op2.overflowing_add(arm.cpsr.c as u32);
    let (result2, c2) = op1.overflowing_add(result1);
    if set_flag {
        let (s_result1, v1) = (op2 as i32).overflowing_add(arm.cpsr.c as i32);
        let (_, v2) = (op1 as i32).overflowing_add(s_result1);
        arm.cpsr.v = v1 | v2;
        arm.cpsr.c = c1 | c2;
        set_nz(arm, result2);
    }
    result2
}

pub fn sub(arm: &mut ArmCore, op1: u32, op2: u32, set_flag: bool) -> u32 {
    let (result, carry) = op1.overflowing_sub(op2);
    if set_flag {
        arm.cpsr.v = (op1 as i32).overflowing_sub(op2 as i32).1;
        arm.cpsr.c = carry;
        set_nz(arm, result);
    }
    result
}

pub fn sbc(arm: &mut ArmCore, op1: u32, op2: u32, set_flag: bool) -> u32 {
    let (result1, c1) = op2.overflowing_add(!arm.cpsr.c as u32);
    let (result2, c2) = op1.overflowing_sub(result1);
    if set_flag {
        let (s_result1, v1) = (op2 as i32).overflowing_add(!arm.cpsr.c as i32);
        let (_, v2) = (op1 as i32).overflowing_sub(s_result1);
        arm.cpsr.v = v1 | v2;
        arm.cpsr.c = c1 | c2;
        set_nz(arm, result2);
    }
    result2
}

pub fn orr(arm: &mut ArmCore, op1: u32, op2: u32, set_flag: bool) -> u32 {
    let result = op1 | op2;
    if set_flag {
        set_nz(arm, result);
    }
    result
}

pub fn mov(arm: &mut ArmCore, _op1: u32, op2: u32, set_flag: bool) -> u32 {
    if set_flag {
        set_nz(arm, op2);
    }
    op2
}

pub fn bic(arm: &mut ArmCore, op1: u32, op2: u32, set_flag: bool) -> u32 {
    let result = op1 & !op2;
    if set_flag {
        set_nz(arm, result);
    }
    result
}

pub fn mvn(arm: &mut ArmCore, _op1: u32, op2: u32, set_flag: bool) -> u32 {
    let result = !op2;
    if set_flag {
        set_nz(arm, result);
    }
    result
}

#[inline(always)]
pub fn set_nz(arm: &mut ArmCore, value: u32) {
    arm.cpsr.n = value >> 31 != 0;
    arm.cpsr.z = value == 0;
}

#[inline(always)]
pub fn set_nz_long(arm: &mut ArmCore, value: u64) {
    arm.cpsr.n = value >> 63 != 0;
    arm.cpsr.z = value == 0;
}

#[derive(Clone, Copy)]
pub enum Bit {
    Any = 0,
    Zero = 1,
    One = 2,
}

pub(super) type IndexBitPair = (usize, Bit);

pub(super) fn process_bit_format(fmt: &str, accept_index: fn(usize) -> bool) -> Vec<IndexBitPair> {
    fmt.chars()
        .filter(|&c| !c.is_whitespace())
        .rev()
        .enumerate()
        .filter(|&(i, _)| accept_index(i))
        .map(|(_, c)| spec_char_to_bit(c))
        .filter(|&b| match b { Bit::Any => false, _ => true })
        .enumerate()
        .collect::<Vec<IndexBitPair>>()
}

fn spec_char_to_bit(c: char) -> Bit {
    match c {
        '0' => Bit::Zero,
        '1' => Bit::One,
        _ => Bit::Any,
    }
}

pub(super) fn specs_matches(index_bits: &[IndexBitPair], disc: u32) -> bool {
    for (i, b) in index_bits.iter() {
        let target = (disc >> i) & 1;
        match b {
            Bit::Zero => if target != 0 { return false },
            Bit::One => if target != 1 { return false },
            _ => unreachable!(),
        };
    }
    true
}
