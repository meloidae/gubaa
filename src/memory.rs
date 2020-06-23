use std::ops::{Deref, DerefMut};
use std::convert::TryInto;
use std::ptr;

use crate::cpu::Cpu;

pub trait Memory {
    fn read_byte(&self, addr: u32) -> u8;
    fn read_half(&self, addr: u32) -> u16;
    fn read_word(&self, addr: u32) -> u32;

    fn write_byte(&mut self, addr: u32, value: u8);
    fn write_half(&mut self, addr: u32, value: u16);
    fn write_word(&mut self, addr: u32, value: u32);
}

// #[derive(Clone)]
// pub struct MemoryPtr(*mut dyn Memory);
// 
// impl MemoryPtr {
//     pub fn null() -> MemoryPtr {
//         MemoryPtr(ptr::null_mut::<Cpu>() as *mut dyn Memory)
//     }
// 
//     pub fn new(memory: *mut dyn Memory) -> Self {
//         Self(memory)
//     }
// }
// 
// impl Default for MemoryPtr {
//     fn default() -> Self {
//         Self::null()
//     }
// }
// 
// impl Deref for MemoryPtr {
//     type Target = dyn Memory;
// 
//     fn deref(&self) -> &Self::Target {
//         &self.0
//     }
// }



#[derive(Clone)]
pub struct HeapBuffer {
    buffer: Box<[u8]>,
}

impl HeapBuffer {
    pub fn new(buffer: &[u8]) -> HeapBuffer {
        HeapBuffer {
            buffer: buffer.to_vec().into_boxed_slice()
        }
    }

    pub fn with_capacity(capacity: u32) -> HeapBuffer {
        HeapBuffer {
            buffer: vec![0u8; capacity as usize].into_boxed_slice()
        }
    }
}

impl Memory for HeapBuffer {
    fn read_byte(&self, addr: u32) -> u8 {
        self.buffer[addr as usize]
    }
    fn read_half(&self, addr: u32) -> u16 {
        assert!(addr & 1 == 0);
        let start = addr as usize;
        let end = start + 2;
        u16::from_le_bytes(self.buffer[start..end].try_into().unwrap())
    }
    fn read_word(&self, addr: u32) -> u32 {
        assert!(addr & 3 == 0);
        let start = addr as usize;
        let end = start + 4;
        u32::from_le_bytes(self.buffer[start..end].try_into().unwrap())
    }

    fn write_byte(&mut self, addr: u32, value: u8) {
        self.buffer[addr as usize] = value;
    }
    fn write_half(&mut self, addr: u32, value: u16) {
        assert!(addr & 1 == 0);
        let start = addr as usize;
        let end = start + 2;
        let bytes = value.to_le_bytes();
        self.buffer[start..end].copy_from_slice(&bytes);
    }
    fn write_word(&mut self, addr: u32, value: u32) {
        assert!(addr & 3 == 0);
        let start = addr as usize;
        let end = start + 4;
        let bytes = value.to_le_bytes();
        self.buffer[start..end].copy_from_slice(&bytes);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test() {
        let mut arr: [u8; 4] = [1, 1, 1, 1];
        let arr2: [u8; 2] = [2, 2];
        arr[1..=2].copy_from_slice(&arr2);
        println!("arr[0..1] = {}", u16::from_le_bytes(arr[0..=1].try_into().unwrap()));
    }
}
