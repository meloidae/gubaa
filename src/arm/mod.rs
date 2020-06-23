#[macro_use]
mod common;
mod ins_arm;

use num_traits::FromPrimitive;
use std::mem;

use crate::arm::ins_arm::{ArmIns, ArmLookupTable};
use crate::memory::{Memory};


#[derive(Debug, PartialEq, Eq, Clone, Copy, Primitive)]
pub enum Condition {
    Eq = 0b0000,
    Ne = 0b0001,
    Cs = 0b0010,
    Cc = 0b0011,
    Mi = 0b0100,
    Pl = 0b0101,
    Vs = 0b0110,
    Vc = 0b0111,
    Hi = 0b1000,
    Ls = 0b1001,
    Ge = 0b1010,
    Lt = 0b1011,
    Gt = 0b1100,
    Le = 0b1101,
    Al = 0b1110,
    Nv = 0b1111,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Primitive)]
pub enum OperatingMode {
    None = 0x00,  // spsr.mode is 0 for bios
    User = 0x10,
    Fiq = 0x11,
    Irq = 0x12,
    Supervisor = 0x13,
    Abort = 0x17,
    Undefined = 0x1b,
    System = 0x1f,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Primitive)]
enum Bank {
    Fiq = 0,
    Supervisor = 1,
    Abort = 2,
    Irq = 3,
    Undefined = 4,
    None = 5,
}

enum Access {
    Nonsequential = 0,
    Sequential = 1,
}

const BANK_COUNT: usize = 6;

struct Pipeline {
    fetch_type: Access,
    ins: [u32; 2],
}

impl Default for Pipeline {
    fn default() -> Pipeline {
        Pipeline {
            fetch_type: Access::Nonsequential,
            ins: Default::default(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct StatusRegister {
    pub n: bool,
    pub z: bool,
    pub c: bool,
    pub v: bool,

    irq_disable: bool,
    fiq_disable: bool,
    thumb_state: bool,
    mode: OperatingMode,
}

impl From<u32> for StatusRegister {
    fn from(bits: u32) -> StatusRegister {
        StatusRegister {
            n: (1 << 31) & bits != 0,
            z: (1 << 30) & bits != 0,
            c: (1 << 29) & bits != 0,
            v: (1 << 28) & bits != 0,

            irq_disable: (1 << 7) & bits != 0,
            fiq_disable: (1 << 6) & bits != 0,
            thumb_state: (1 << 5) & bits != 0,
            mode: OperatingMode::from_u32(bits & 0x1f).unwrap(),
        }
    }
}

impl From<StatusRegister> for u32 {
    fn from(sr: StatusRegister) -> u32 {
        let mut bits: u32 = 0;
        bits |= (sr.n as u32) << 31;
        bits |= (sr.z as u32) << 30;
        bits |= (sr.c as u32) << 29;
        bits |= (sr.v as u32) << 28;

        bits |= (sr.irq_disable as u32) << 7;
        bits |= (sr.fiq_disable as u32) << 6;
        bits |= (sr.thumb_state as u32) << 5;
        bits |= sr.mode as u32;
        bits
    }
}

impl StatusRegister {
    fn set_flags(&mut self, bits: u32) {
        let sr = StatusRegister::from(bits);
        self.n = sr.n;
        self.z = sr.z;
        self.c = sr.c;
        self.v = sr.v;
    }
}

impl Default for StatusRegister {
    fn default() -> StatusRegister {
        StatusRegister {
            n: false,
            z: false,
            c: false,
            v: false,

            irq_disable: false,
            fiq_disable: false,
            thumb_state: false,
            mode: OperatingMode::None,
        }
    }
}

pub const REG_SP: usize = 13;
pub const REG_LR: usize = 14;
pub const REG_PC: usize = 15;

pub struct ArmCore<'a> {
    regs: [u32; 16],
    banks: [[u32; BANK_COUNT - 1]; 7],
    cpsr: StatusRegister,
    spsr: [StatusRegister; BANK_COUNT - 1],
    pipe: Pipeline,
    memory: &'a mut dyn Memory,
    arm_lut: ArmLookupTable,
}

impl ArmCore<'_> {
    pub fn new(memref: &mut dyn Memory) -> ArmCore {
        ArmCore {
            regs: Default::default(),
            banks: Default::default(),
            cpsr: Default::default(),
            spsr: Default::default(),
            pipe: Default::default(),
            memory: memref,
            arm_lut: ArmLookupTable::compute(),
        }
    }

    fn run(&mut self) {
        let ins = self.pipe.ins[0];
        if self.cpsr.thumb_state {
            // thumb
        } else {
            // arm
        }
    }

    fn check_condition(&self, cond: Condition) -> bool {
        match cond {
            Condition::Eq => self.cpsr.z,
            Condition::Ne => !self.cpsr.z,
            Condition::Cs => self.cpsr.c,
            Condition::Cc => !self.cpsr.c,
            Condition::Mi => self.cpsr.n,
            Condition::Pl => !self.cpsr.n,
            Condition::Vs => self.cpsr.v,
            Condition::Vc => !self.cpsr.v,
            Condition::Hi => self.cpsr.c & !self.cpsr.z,
            Condition::Ls => !self.cpsr.c | self.cpsr.z,
            Condition::Ge => self.cpsr.n == self.cpsr.v,
            Condition::Lt => self.cpsr.n != self.cpsr.v,
            Condition::Gt => !self.cpsr.z & (self.cpsr.n == self.cpsr.v),
            Condition::Le => self.cpsr.z | (self.cpsr.n != self.cpsr.v),
            Condition::Al => true,
            Condition::Nv => panic!("should be reserved"),
        }
    }

    fn get_register_bank_mode(&self, mode: OperatingMode) -> Bank {
        match mode {
            OperatingMode::User | OperatingMode::System => Bank::None,
            OperatingMode::Fiq => Bank::Fiq,
            OperatingMode::Irq => Bank::Irq,
            OperatingMode::Supervisor => Bank::Supervisor,
            OperatingMode::Abort => Bank::Abort,
            OperatingMode::Undefined => Bank::Undefined,
            _ => Bank::Undefined,
        }
    }

    fn switch_mode(&mut self, mode: OperatingMode) {
        let from_mode = self.cpsr.mode;
        self.swap_bank(from_mode);
        self.swap_bank(mode);
        self.cpsr.mode = mode;
    }

    fn set_reg(&mut self, index: usize, value: u32) {
        self.regs[index] = value;
    }

    fn set_cpsr(&mut self, bits: u32) {
        let new_cpsr = StatusRegister::from(bits);
        // thumb state bit of cpsr should not be changed
        assert!(new_cpsr.thumb_state == self.cpsr.thumb_state);
        if new_cpsr.mode != self.cpsr.mode {
            let from_mode = self.cpsr.mode;
            self.swap_bank(from_mode);
            self.swap_bank(new_cpsr.mode);
        }
        self.cpsr = new_cpsr;
    }

    fn swap_bank(&mut self, mode: OperatingMode) {
        const BANK_OFFSET: usize = 8;
        let bank_mode = self.get_register_bank_mode(mode);
        let bank_idxs: &[usize] = match bank_mode {
            Bank::None => &[],
            Bank::Fiq => {
                if self.cpsr.thumb_state {
                    &[5, 6]
                } else { // arm state for FIQ has 7 banked registers
                    &[0, 1, 2, 3, 4, 5, 6]
                }
            },
            Bank::Irq | Bank::Supervisor | Bank::Abort | Bank::Undefined => &[5, 6]
        };

        for &idx in bank_idxs {
            mem::swap(&mut self.regs[idx + BANK_OFFSET], &mut self.banks[bank_mode as usize][idx]);
        }
    }

    fn get_spsr(&self) -> Option<StatusRegister> {
        let bank_mode = self.get_register_bank_mode(self.cpsr.mode);
        if bank_mode == Bank::None {
            None
        } else {
            Some(self.spsr[bank_mode as usize])
        }
    }

    fn get_spsr_mut(&mut self) -> Option<&mut StatusRegister> {
        let bank_mode = self.get_register_bank_mode(self.cpsr.mode);
        if bank_mode == Bank::None {
            None
        } else {
            Some(&mut self.spsr[bank_mode as usize])
        }
    }

    // // for testing purpose
    // pub fn get_arm_lookup_table(&self) -> ArmLookupTable {
    //     ArmLookupTable::compute()
    // }
}
