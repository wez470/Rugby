use sdl2::keyboard::Keycode;
use std::time::{Instant, Duration};
use std::thread;
use crate::cpu::Cpu;

const CYCLES_PER_FRAME: usize = 69905;
const NANOS_PER_FRAME: u32 = 16666667;

pub fn start_frontend(cpu: &mut Cpu, instruction_count: usize, step_mode: bool) {
    let sdl = sdl2::init().expect("Failed to initialize SDL");
    let video_subsystem = sdl.video().expect("Failed to access SDL video subsystem");
    let window = video_subsystem
        .window("Rustboy", 1024, 1024)
        .resizable()
        .build()
        .expect("Failed to create window");
    let mut renderer = window.renderer().build().expect("Failed to get window renderer");
    let mut event_pump = sdl.event_pump().expect("Failed to get SDL event pump");
    let start_time = Instant::now();
    'main: for _ in 0..instruction_count {
        let frame_start_time = Instant::now();

        const BYTES_PER_PIXEL: usize = 4;
        let mut image = [0u8; 160 * 144 * BYTES_PER_PIXEL];

        for tile_row in 0..144 {
            for tile_col in 0..160 {
                let tile_color = cpu.gpu.screen_buffer[tile_row][tile_col];
                image[(tile_row * 160 + tile_col) * 4 + 2] = GAMEBOY_COLORS[tile_color as usize].rgb().0;
                image[(tile_row * 160 + tile_col) * 4 + 1] = GAMEBOY_COLORS[tile_color as usize].rgb().1;
                image[(tile_row * 160 + tile_col) * 4 + 0] = GAMEBOY_COLORS[tile_color as usize].rgb().2;
            }
        }

        let surface = sdl2::surface::Surface::from_data(
            &mut image[..],
            160 as u32,
            144 as u32,
            (160 * BYTES_PER_PIXEL) as u32,
            sdl2::pixels::PixelFormatEnum::RGB888,
        ).unwrap();
        let texture = renderer.create_texture_from_surface(&surface).unwrap();

        renderer.copy(&texture, None, None).unwrap();
        renderer.present();

        'wait: while {
            for event in event_pump.poll_iter() {
                use sdl2::event::Event;
                match event {
                    Event::Quit { .. } => break 'main,
                    Event::KeyDown { keycode, .. } => {
                        if let Some(key) = keycode {
                            if key == Keycode::Space && step_mode {
                                break 'wait;
                            }
                        }
                    },
                    _ => ()
                }
            }
            step_mode
        } {}

        cpu.step_cycles(CYCLES_PER_FRAME);

        while (Instant::now() - frame_start_time).subsec_nanos() < NANOS_PER_FRAME {
            thread::sleep(Duration::new(0, 1 as u32));
        }
    }
    let end_time = Instant::now();
    let total_duration = end_time - start_time;
    let total_time = total_duration.as_secs() as f64 + (total_duration.subsec_nanos() as f64) / 1e9;
    println!("Total time: {}", total_time);
}

/// The four colors of the original Game Boy screen, from lightest to darkest, in RGB.
const GAMEBOY_COLORS: [sdl2::pixels::Color; 4] = [
    sdl2::pixels::Color::RGB(155, 188, 15),
    sdl2::pixels::Color::RGB(139, 172, 15),
    sdl2::pixels::Color::RGB(48, 98, 48),
    sdl2::pixels::Color::RGB(15, 56, 15),
];
