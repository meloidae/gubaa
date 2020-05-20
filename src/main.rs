#[macro_use]
extern crate enum_primitive_derive;
extern crate num_traits;

mod arm;
use crate::arm::StatusRegister;

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
    // let mut point = Point3d { x: 0, y: 0, z:2 };
    // point = Point3d { y: 1, .. point };
    // println!("The point is at ({}, {}, {})", point.x, point.y, point.z);
    // let (.., z) = point_coord(&point);
    // println!("The point's z is {}", z);
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
