use super::{ArmCore};


// logical shift left and return (shifted_val, carry)
pub fn shift_lsl(arm: &ArmCore, rm: u32, shift: u32) -> (u32, bool) {
    if shift == 32 {
        (0, (rm & 1) != 0)
    } else if shift > 32 {
        (0, false)
    } else {
        if shift != 0 {
            (rm << shift, ((rm >> (shift - 1)) & 1) != 0)
        } else {
            (rm, arm.cpsr.c)
        }
    }
}

// logical shift right and return (shifted_val, carry)
pub fn shift_lsr(arm: &ArmCore, rm: u32, shift: u32) -> (u32, bool) {
    if shift == 32 || shift == 0 {
        (0, (rm >> 31) != 0)
    } else if shift > 32 {
        (0, false)
    } else {
        (rm >> shift, ((rm >> (shift - 1)) & 1) != 0)
    }
}

// arithematic shift right and return (shifted_val, carry)
pub fn shift_asr(arm: &ArmCore, rm: u32, shift: u32) -> (u32, bool) {
    if shift >= 32 || shift == 0 {
        if (rm >> 31) != 0 {
            (0xffff_ffff, true)
        } else {
            (0, false)
        }
    } else {
        let carry = (((rm as i32) >> (shift - 1)) & 1) != 0;
        (((rm as i32) >> shift) as u32, carry)
    }
}

// rotate right and return (shifted_val, carry)
pub fn shift_ror(arm: &ArmCore, rm: u32, shift: u32) -> (u32, bool) {
    if shift == 0 { // rrx
        let carry_in = arm.cpsr.c as u32;
        ((rm >> 1) | (carry_in << 31), (rm & 1) != 0)
    } else {
        let carry = ((rm >> ((shift - 1) & 0x1f)) & 1) != 0;
        (rm.rotate_right(shift), carry)
    }
}


#[inline(always)]
pub fn set_nz(arm: &mut ArmCore, value: u32) {
    arm.cpsr.n = value >> 31 != 0;
    arm.cpsr.z = value == 0;
}

#[inline(always)]
pub fn add_set_vc(arm: &mut ArmCore, a: u32, b: u32) {
    arm.cpsr.v = (a as i32).overflowing_add(b as i32).1;
}

#[inline(always)]
pub fn sub_set_vc(arm: &mut ArmCore, a: u32, b: u32) {
    arm.cpsr.v = (a as i32).overflowing_sub(b as i32).1;
}
