use crate::memory::Memory;


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

}

impl Memory for Cpu {
    fn read_byte(&self, addr: u32) -> u8 {
        0u8
    }
    fn read_half(&self, addr: u32) -> u16 {
        0u16
    }
    fn read_word(&self, addr: u32) -> u32 {
        0u32
    }

    fn write_byte(&mut self, addr: u32, value: u8){
    }
    fn write_half(&mut self, addr: u32, value: u16){
    }
    fn write_word(&mut self, addr: u32, value: u32){
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
