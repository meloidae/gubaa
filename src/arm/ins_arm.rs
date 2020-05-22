use super::{Condition, ArmCore};

use crate::arm::common::{lsl_carry, lsl_no_carry, lsr_carry, lsr_no_carry, asr_carry, asr_no_carry,
    ror_carry, ror_no_carry, and, eor, add, adc, sub, sbc, orr, mov, bic, mvn};

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
    let op2 = if imm_flag {
        get_rotated_immediate(arm, ins, cond_flag)
    } else {
        get_shifted_register(arm, ins, cond_flag)
    };


    if rd_idx == 15 && cond_flag {

    }

    // Calculate result of operation
    let result = match op_code {
        AND | TST => and(arm, op1, op2, cond_flag),
        EOR | TEQ => eor(arm, op1, op2, cond_flag),
        SUB | CMP => sub(arm, op1, op2, cond_flag),
        RSB => sub(arm, op2, op1, cond_flag),
        ADD | CMN => add(arm, op1, op2, cond_flag),
        ADC => adc(arm, op1, op2, cond_flag),
        SBC => sbc(arm, op1, op2, cond_flag),
        RSC => sbc(arm, op2, op1, cond_flag),
        ORR => orr(arm, op1, op2, cond_flag),
        MOV => mov(arm, op1, op2, cond_flag),
        BIC => bic(arm, op1, op2, cond_flag),
        MVN => mvn(arm, op1, op2, cond_flag),
        _ => unreachable!(),
    };

    match op_code {
        TST | TEQ | CMP | CMN => {},
        _ => arm.set_reg(rd_idx, result),
    };

}

fn multiply(arm: &mut ArmCore, ins: ArmIns) {
}
