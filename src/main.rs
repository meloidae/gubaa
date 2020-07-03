#[macro_use]
extern crate enum_primitive_derive;
extern crate num_traits;
extern crate log;

#[macro_use]
mod memory;
mod cpu;
mod arm;

use cpu::Cpu;

use std::env;

fn main() {
    env::set_var("RUST_LOG", "info");
    env_logger::init();
    let bios = &[0u8];
    let game = &[0u8];
    let mut cpu = Cpu::new(bios, game);
}
