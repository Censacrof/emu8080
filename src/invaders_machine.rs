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

const N_WIDTH: u32 = 256;
const N_HEIGHT: u32 = 224;
const PX_SIZE: u32 = 3;
const WND_WIDTH: u32 = N_HEIGHT * PX_SIZE; // the monitor is rotated 90 degree clockwise
const WND_HEIGHT: u32 = N_WIDTH * PX_SIZE;

const VRAM_OFFSET: usize = 0x2400;
const VRAM_END: usize = 0x4000;
const VRAM_BUFF_LEN: usize = VRAM_END - VRAM_OFFSET;
pub struct InvadersMemoryMap {
    ram: [u8; 65536],
    vram: Arc<RwLock<Box<[u8; VRAM_BUFF_LEN]>>>,
}


impl MemoryMap for InvadersMemoryMap {
    fn read_b(&self, addr: u16) -> Result<u8, MemoryMapError> {
        let b: u8 = self.ram[addr as usize];
        return Ok(b);
    }

    fn write_b(&mut self, addr: u16, b: u8) -> Result<(), MemoryMapError> {
        self.ram[addr as usize] = b;

        if addr >= VRAM_OFFSET as u16 && addr < VRAM_END as u16 {
            let mut buff_guard = self.vram.write().unwrap();
            let buff_index: usize = addr as usize - VRAM_OFFSET;
            buff_guard[buff_index] = b;
        }

        return Ok(());
    }

    fn read_w(&self, addr: u16) -> Result<u16, MemoryMapError> {
        let addr_i = addr as usize;
        let w: u16 = (self.ram[addr_i] as u16) + ((self.ram[addr_i + 1] as u16) << 8);
        return Ok(w);
    }

    fn write_w(&mut self, addr: u16, w: u16) -> Result<(), MemoryMapError> {
        let addr_i = addr as usize;
        
        let l: u8 = (w & 0x00ff) as u8;
        let h: u8 = ((w & 0xff00) >> 8) as u8;

        self.ram[addr_i] = l;
        self.ram[addr_i + 1] = h;

        if addr >= VRAM_OFFSET as u16 && addr < VRAM_END as u16 {
            let mut bugg_guard = self.vram.write().unwrap();
            let buff_index: usize = addr as usize - VRAM_OFFSET;
            bugg_guard[buff_index] = l;
            bugg_guard[buff_index + 1] = h;
        }

        return Ok(());
    }
}


#[derive(Default)]
pub struct InvadersIOBus {
    reg_shift: u16,
    reg_shift_offset: u8,
}

const IO_SHIFT_OFFSET: u8 = 2;
const IO_SHIFT_READ: u8 = 3;
const IO_SHIFT_DATA: u8 = 4;

impl IOBus for InvadersIOBus {  
    fn in_port(&mut self, port: u8) -> u8 {
        match port {
            IO_SHIFT_READ => {
                if self.reg_shift_offset > 8 {
                    0
                }
                else {
                    ((self.reg_shift >> (8 - self.reg_shift_offset)) & 0x00ff) as u8
                }                
            },
            _ => { 0 },
        }
    }

    fn out_port(&mut self, port: u8, data: u8) {
        match port {
            IO_SHIFT_OFFSET => {
                self.reg_shift_offset = data;
            },
            IO_SHIFT_DATA => {
                self.reg_shift >>= 8;
                self.reg_shift += (data as u16) << 8;
            }
            _ => {},
        }
    }
}


const ROM_H: &[u8] = include_bytes!("ROM/invaders.h");
const ROM_G: &[u8] = include_bytes!("ROM/invaders.g");
const ROM_F: &[u8] = include_bytes!("ROM/invaders.f");
const ROM_E: &[u8] = include_bytes!("ROM/invaders.e");

pub struct InvadersMachine {
    pub cpu: Arc<RwLock<Cpu8080>>,
    pub addr_space: InvadersMemoryMap,
    pub io_space: InvadersIOBus,
}


impl InvadersMachine {
    pub fn new() -> InvadersMachine {
        InvadersMachine {
            cpu: Arc::new(RwLock::new(Cpu8080::new())),
            addr_space: InvadersMemoryMap { 
                ram: [0x00; 65536],
                vram: Arc::new(RwLock::new(Box::new([0; VRAM_BUFF_LEN]))),
            },
            io_space: Default::default(),
        }
    }

    pub fn boot(&mut self) -> Result<(), String> {
        self.cpu.write().unwrap()
            .state.interrupt_enabled = false;

        // load the rom
        for i in 0..ROM_H.len() {
            self.addr_space.ram[i] = ROM_H[i];
        }

        for i in 0..ROM_G.len() {
            self.addr_space.ram[0x800 + i] = ROM_G[i];
        }

        for i in 0..ROM_F.len() {
            self.addr_space.ram[0x1000 + i] = ROM_F[i];
        }

        for i in 0..ROM_E.len() {
            self.addr_space.ram[0x1800 + i] = ROM_E[i];
        }        

        let vram = self.addr_space.vram.clone();
        let cpu = self.cpu.clone();

        let must_quit_ref = Arc::new(RwLock::new(false));
        let must_quit_display = must_quit_ref.clone();

        thread::spawn(move || {
            let sdl_context = sdl2::init().unwrap();
            let video_subsystem = sdl_context.video().unwrap();

            let window = video_subsystem
                .window("Space Invaders", WND_WIDTH, WND_HEIGHT)
                .position_centered()
                .opengl()
                .build()
                .map_err(|e| e.to_string()).unwrap();
            
            let mut canvas = window.into_canvas().build().map_err(|e| e.to_string()).unwrap();

            canvas.set_draw_color(Color::RGB(255, 0, 0));
            canvas.clear();
            canvas.present();
            let mut event_pump = sdl_context.event_pump().unwrap();

            let tex_c = canvas.texture_creator();
            let mut tex: Texture = tex_c.create_texture(
                Some(sdl2::pixels::PixelFormatEnum::RGB888),
                sdl2::render::TextureAccess::Streaming,
                N_HEIGHT,
                N_WIDTH
            ).unwrap();            

            let mut next_interrupt_type = 1u8;

            while *must_quit_display.read().unwrap() == false {
                for event in event_pump.poll_iter() {
                    match event {
                        Event::Quit { .. }
                        | Event::KeyDown {
                            keycode: Some(Keycode::Escape),
                            ..
                        } => { 
                            let mut w = must_quit_display.write().unwrap();
                            *w = true;
                        },
                        _ => {}
                    }
                }

                cpu.write().unwrap().interrupt_rst(next_interrupt_type);
                next_interrupt_type = if next_interrupt_type == 1 { 2 } else { 1 };
                

                let vram_guard = vram.write().unwrap();
                tex.with_lock(None, |buffer: &mut [u8], pitch: usize| {
                    let mut pixel_count: usize = 0;
                    for y in 0..N_HEIGHT as usize {
                        for x in 0..N_WIDTH as usize {
                            let tx = y;
                            let ty = (N_WIDTH - 1) as usize - x;

                            let buff_offset = ty * pitch + tx * 4;
                            
                            let vram_offset = pixel_count >> 3; // fast division by 8
                            let vram_bit = pixel_count & 0x07;  // fast mod by 8
                            let val: u8 = if (vram_guard[vram_offset] >> vram_bit) & 0x01 != 0 { 0xff } else { 0x00 };

                            buffer[buff_offset + 0] = val;   // B
                            buffer[buff_offset + 1] = val;   // G
                            buffer[buff_offset + 2] = val;   // R
                            buffer[buff_offset + 3] = 0xff;  // A

                            pixel_count += 1;
                        }
                    }
                }).unwrap();

                canvas.copy(
                    &tex,
                    None,
                    Rect::new(0, 0, WND_WIDTH, WND_HEIGHT),
                ).unwrap();
                canvas.present();

                ::std::thread::sleep(Duration::from_millis(17));
            }
        });


        while *must_quit_ref.read().unwrap() == false {
            self.cpu.write().unwrap()
                .fetch_and_execute(true, false, &mut self.addr_space, &mut self.io_space).unwrap();            
        }

        Ok(())
    }
}


#[cfg(test)]
mod tests {
    use super::*;
}