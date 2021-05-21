mod cpu8080;
mod invaders_machine;

use crate::cpu8080::*;
use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use sdl2::pixels::Color;
use sdl2::render::Texture;
use sdl2::rect::*;
use std::time::{Duration, Instant};
use std::sync::RwLock;
use std::thread;
use std::sync::Arc;

use invaders_machine::*;

fn main() {
    let mut machine = InvadersMachine::new();

    machine.boot();
}
