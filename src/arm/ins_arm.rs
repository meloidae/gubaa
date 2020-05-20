use super::{Condition, ArmCore};

use crate::arm::common::{shift_lsl, shift_lsr, shift_asr, shift_ror, set_nz, add_set_vc, sub_set_vc};

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
}


fn get_rotated_immediate(arm: &mut ArmCore, ins: ArmIns) -> (u32, bool) {
    let value = ins.slice(0, 8);
    let rotate = ins.slice(8, 12) << 1;
    if rotate != 0 {
        let carry = (value >> (rotate - 1)) != 0;
        (value.rotate_right(rotate), carry)
    } else {
        (value, arm.cpsr.c)
    }
}


fn get_shifted_register(arm: &mut ArmCore, ins: ArmIns) -> (u32, bool) {
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

    let (value, carry) = match shift_type {
        LSL => shift_lsl(arm, rm_value, shift),
        LSR => shift_lsr(arm, rm_value, shift),
        ASR => shift_asr(arm, rm_value, shift),
        ROR => shift_ror(arm, rm_value, shift),
        _ => unreachable!()
    };
    (value, carry)
}

// TODO
fn branch_and_exchange(arm: &mut ArmCore, ins: ArmIns) {
    let address = arm.regs[ins.reg(0)];
    // if address & 0x1 {
    // } else {
    // }
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
    let imm_flag = ins.flag(25);
    let cond_flag = ins.flag(20);

    let op1 = arm.regs[rn_idx];
    let (op2, carry) = if imm_flag {
        get_rotated_immediate(arm, ins)
    } else {
        get_shifted_register(arm, ins)
    };


    if rd_idx == 15 && cond_flag {

    }

    let result = match op_code {
        AND | TST=> op1 & op2,
        EOR | TEQ=> op1 ^ op2,
        SUB | CMP=> op1.wrapping_sub(op2),
        RSB => op2.wrapping_sub(op1),
        ADD | CMN=> op1.wrapping_add(op2),
        ADC => op1.wrapping_add(op2.wrapping_add(arm.cpsr.c as u32)),
        SBC => op1.wrapping_sub(op2.wrapping_add(!arm.cpsr.c as u32)),
        RSC => op2.wrapping_sub(op1.wrapping_add(!arm.cpsr.c as u32)),
        ORR => op1 | op2,
        MOV => op2,
        BIC => op1 & !op2,
        MVN => !op2,
        _ => unreachable!(),
    };
}

