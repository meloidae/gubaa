#[macro_use]
extern crate enum_primitive_derive;
extern crate num_traits;


#[macro_use]
mod memory;
mod cpu;
mod arm;
use crate::arm::StatusRegister;
use crate::arm::ArmCore;

struct Foo<'a> {
    x: &'a i32,
}

impl<'a> Foo<'a> {
    fn x(&self) -> &'a i32 { self.x }
}

struct Point {
    x: i32,
    y: i32,
}

struct PointRef<'a> {
    x: &'a mut i32,
    y: &'a mut i32,
}

struct Point3d {
    x: i32,
    y: i32,
    z: i32,
}


fn main() {
    let x: usize = 15;
    let y: u32 = 15;
    println!("{}", x == (y as usize));
}

fn double(x: i32) -> i32 {
    x * 2
}

fn not(x: bool) -> bool {
    !x
}

fn print_each(v: &Vec<i32>) {
    for val in v {
        println!("{}", val);
    }
}

fn point_coord(p: &Point3d) -> (i32, i32, i32) {
    let x = p.x;
    let y = p.y;
    let z = p.z;
    (x, y, z)
}
