use crate::memory::{Memory, HeapBuffer};


pub const EWRAM_SIZE: u32 = 256 * 1024;
const EWRAM_MASK: u32 = EWRAM_SIZE - 1;
pub const IWRAM_SIZE: u32 = 32 * 1024;
const IWRAM_MASK: u32 = IWRAM_SIZE - 1;

pub const BASE_ROM: u32 = 0x0;
pub const BASE_EWRAM: u32 = 0x2;
pub const BASE_IWRAM: u32 = 0x3;
pub const BASE_IOREGS: u32 = 0x4;
pub const BASE_PALETTE: u32 = 0x5;
pub const BASE_VRAM: u32 = 0x06;
pub const BASE_OAM: u32 = 0x07;
pub const BASE_GAMEPAK_START: u32 = 0x8;
const BASE_GAMEPAK_END: u32 = 0xd;

pub struct Cpu {
    rom: HeapBuffer,
    ewram: HeapBuffer,
    iwram: HeapBuffer,
}

impl Cpu {
    pub fn new(bios: &[u8], game: &[u8]) -> Box<Cpu> {
        let mut result = Box::new(Cpu {
            rom: HeapBuffer::new(bios),
            ewram: HeapBuffer::with_capacity(EWRAM_SIZE),
            iwram: HeapBuffer::with_capacity(IWRAM_SIZE),
        });
        result
    }
}

impl Memory for Cpu {
    fn read_byte(&self, addr: u32) -> u8 {
        match addr >> 24 {
            BASE_ROM => {
                self.rom.read_byte(addr)
            },
            BASE_EWRAM => self.ewram.read_byte(addr & EWRAM_MASK),
            BASE_IWRAM => self.iwram.read_byte(addr & IWRAM_MASK),
            _ => unreachable!(),
        }
    }

    fn read_half(&self, addr: u32) -> u16 {
        match addr >> 24 {
            BASE_ROM => {
                self.rom.read_half(addr)
            },
            BASE_EWRAM => self.ewram.read_half(addr & EWRAM_MASK),
            BASE_IWRAM => self.iwram.read_half(addr & IWRAM_MASK),
            _ => unreachable!(),
        }
    }

    fn read_word(&self, addr: u32) -> u32 {
        match addr >> 24 {
            BASE_ROM => {
                self.rom.read_word(addr)
            },
            BASE_EWRAM => self.ewram.read_word(addr & EWRAM_MASK),
            BASE_IWRAM => self.iwram.read_word(addr & IWRAM_MASK),
            _ => unreachable!(),
        }
    }

    fn write_byte(&mut self, addr: u32, value: u8){
        match addr >> 24 {
            BASE_ROM => {
                self.rom.write_byte(addr, value)
            },
            BASE_EWRAM => self.ewram.write_byte(addr & EWRAM_MASK, value),
            BASE_IWRAM => self.iwram.write_byte(addr & IWRAM_MASK, value),
            _ => unreachable!(),
        };
    }

    fn write_half(&mut self, addr: u32, value: u16){
        match addr >> 24 {
            BASE_ROM => {
                self.rom.write_half(addr, value)
            },
            BASE_EWRAM => self.ewram.write_half(addr & EWRAM_MASK, value),
            BASE_IWRAM => self.iwram.write_half(addr & IWRAM_MASK, value),
            _ => unreachable!(),
        };
    }

    fn write_word(&mut self, addr: u32, value: u32){
        match addr >> 24 {
            BASE_ROM => {
                self.rom.write_word(addr, value)
            },
            BASE_EWRAM => self.ewram.write_word(addr & EWRAM_MASK, value),
            BASE_IWRAM => self.iwram.write_word(addr & IWRAM_MASK, value),
            _ => unreachable!(),
        };
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test() {
        assert!(true);
    }
}
