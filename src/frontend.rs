use crate::cpu::Cpu;
use crate::joypad::{ButtonKeys, DirKeys};
use log::info;
use sdl2::controller::Button;
use sdl2::event::Event;
use sdl2::keyboard::{Keycode, Mod};
use std::thread;
use std::time::{Instant, Duration};

const CYCLES_PER_FRAME: usize = 69905;
const NANOS_PER_FRAME: u32 = 16_666_667;

pub fn start_frontend(cpu: &mut Cpu, inst_limit: Option<usize>) {
    let sdl = sdl2::init().expect("Failed to initialize SDL");
    let video = sdl.video().expect("Failed to access SDL video subsystem");
    let window = video
        .window("Rustboy", 1024, 1024)
        .resizable()
        .opengl()
        .build()
        .expect("Failed to create window");

    let gl_context = window.gl_create_context().expect("Couldn't create GL context");
    gl::load_with(|s| video.gl_get_proc_address(s) as _);

    let mut imgui = imgui::ImGui::init();
    imgui.set_ini_filename(None);
    let mut imgui_sdl2 = imgui_sdl2::ImguiSdl2::new(&mut imgui);
    let imgui_renderer = imgui_opengl_renderer::Renderer::new(
        &mut imgui,
        |s| video.gl_get_proc_address(s) as _,
    );

    let mut canvas = window
        .into_canvas()
        .present_vsync()
        .build()
        .expect("Failed to get window canvas");

    let game_controller_subsytem = sdl.game_controller().expect("Failed to get game controllers");
    let mut controllers = vec![];

    let mut paused = false;
    let mut pause_next_frame = false;
    let mut event_pump = sdl.event_pump().expect("Failed to get SDL event pump");

    'main: for inst_count in 0.. {
        let frame_start_time = Instant::now();

        if let Some(max) = inst_limit {
            if inst_count >= max {
                 break;
            }
        }

        if pause_next_frame {
            pause_next_frame = false;
            paused = true;
        }

        for event in event_pump.poll_iter() {
            imgui_sdl2.handle_event(&mut imgui, &event);
            if imgui_sdl2.ignore_event(&event) { continue; }

            match event {
                Event::Quit { .. } => break 'main,

                Event::KeyDown { keycode: Some(keycode), keymod, repeat, .. } => {
                    let modifiers = Mod::LSHIFTMOD | Mod::RSHIFTMOD | Mod::LCTRLMOD |
                        Mod::RCTRLMOD | Mod::LALTMOD | Mod::RALTMOD | Mod::LGUIMOD |
                        Mod::RGUIMOD;
                    if !keymod.intersects(modifiers) {
                        match keycode {
                            Keycode::W if !repeat => cpu.joypad.dir_key_down(DirKeys::UP),
                            Keycode::A if !repeat => cpu.joypad.dir_key_down(DirKeys::LEFT),
                            Keycode::S if !repeat => cpu.joypad.dir_key_down(DirKeys::DOWN),
                            Keycode::D if !repeat => cpu.joypad.dir_key_down(DirKeys::RIGHT),
                            Keycode::Return if !repeat =>
                                cpu.joypad.button_key_down(ButtonKeys::START),
                            Keycode::Tab if !repeat =>
                                cpu.joypad.button_key_down(ButtonKeys::SELECT),
                            Keycode::K if !repeat => cpu.joypad.button_key_down(ButtonKeys::A),
                            Keycode::J if !repeat => cpu.joypad.button_key_down(ButtonKeys::B),
                            Keycode::P if !repeat => paused = !paused,
                            Keycode::Space => {
                                paused = false;
                                pause_next_frame = true;
                            }
                            _ => {}
                        }
                    }
                }

                Event::KeyUp { keycode: Some(keycode), keymod, .. } => {
                    let modifiers = Mod::LSHIFTMOD | Mod::RSHIFTMOD | Mod::LCTRLMOD |
                        Mod::RCTRLMOD | Mod::LALTMOD | Mod::RALTMOD | Mod::LGUIMOD |
                        Mod::RGUIMOD;
                    if !keymod.intersects(modifiers) {
                        match keycode {
                            Keycode::W => cpu.joypad.dir_key_up(DirKeys::UP),
                            Keycode::A => cpu.joypad.dir_key_up(DirKeys::LEFT),
                            Keycode::S => cpu.joypad.dir_key_up(DirKeys::DOWN),
                            Keycode::D => cpu.joypad.dir_key_up(DirKeys::RIGHT),
                            Keycode::Return => cpu.joypad.button_key_up(ButtonKeys::START),
                            Keycode::Tab => cpu.joypad.button_key_up(ButtonKeys::SELECT),
                            Keycode::K => cpu.joypad.button_key_up(ButtonKeys::A),
                            Keycode::J => cpu.joypad.button_key_up(ButtonKeys::B),
                            _ => {}
                        }
                    }
                }

                Event::ControllerDeviceAdded { which, .. } => {
                    if let Ok(controller) = game_controller_subsytem.open(which) {
                        info!("Successfully opened new controller with index {}", which);
                        controllers.push(controller);
                    } else {
                        info!("Failed to open new controller with index {}", which);
                    }
                }

                Event::ControllerDeviceRemoved { which, .. } => {
                    controllers.retain(|c| c.instance_id() != which);
                    info!("Removed controller with index {}", which);
                }

                Event::ControllerButtonDown { button, .. } => {
                    match button {
                        Button::A => cpu.joypad.button_key_down(ButtonKeys::A),
                        Button::X => cpu.joypad.button_key_down(ButtonKeys::B),
                        Button::Start => cpu.joypad.button_key_down(ButtonKeys::START),
                        Button::Back => cpu.joypad.button_key_down(ButtonKeys::SELECT),
                        Button::DPadLeft => cpu.joypad.dir_key_down(DirKeys::LEFT),
                        Button::DPadRight => cpu.joypad.dir_key_down(DirKeys::RIGHT),
                        Button::DPadUp => cpu.joypad.dir_key_down(DirKeys::UP),
                        Button::DPadDown => cpu.joypad.dir_key_down(DirKeys::DOWN),
                        _ => {}
                    }
                }

                Event::ControllerButtonUp { button, .. } => {
                    match button {
                        Button::A => cpu.joypad.button_key_up(ButtonKeys::A),
                        Button::X => cpu.joypad.button_key_up(ButtonKeys::B),
                        Button::Start => cpu.joypad.button_key_up(ButtonKeys::START),
                        Button::Back => cpu.joypad.button_key_up(ButtonKeys::SELECT),
                        Button::DPadLeft => cpu.joypad.dir_key_up(DirKeys::LEFT),
                        Button::DPadRight => cpu.joypad.dir_key_up(DirKeys::RIGHT),
                        Button::DPadUp => cpu.joypad.dir_key_up(DirKeys::UP),
                        Button::DPadDown => cpu.joypad.dir_key_up(DirKeys::DOWN),
                        _ => {}
                    }
                }

                _ => ()
            }
        }

        const BYTES_PER_PIXEL: usize = 4;
        let mut image = [0u8; 160 * 144 * BYTES_PER_PIXEL];

        for tile_row in 0..144 {
            for tile_col in 0..160 {
                let pixel_i = (tile_row * 160 + tile_col) * 4;
                let color_i = cpu.gpu.screen_buffer[tile_row][tile_col] as usize;
                let color = GAMEBOY_COLORS[color_i].rgb();
                image[pixel_i + 2] = color.0;
                image[pixel_i + 1] = color.1;
                image[pixel_i + 0] = color.2;
            }
        }

        let surface = sdl2::surface::Surface::from_data(
            &mut image[..],
            160 as u32,
            144 as u32,
            (160 * BYTES_PER_PIXEL) as u32,
            sdl2::pixels::PixelFormatEnum::RGB888,
        ).unwrap();
        let texture_creator = canvas.texture_creator();
        let texture = texture_creator.create_texture_from_surface(&surface).unwrap();

        canvas.copy(&texture, None, None).unwrap();

        {
            let ui = imgui_sdl2.frame(canvas.window(), &mut imgui, &event_pump.mouse_state());
            ui.show_demo_window(&mut true);

            // FIXME(solson): Avoid using SDL Canvas so we don't need this hack (which is from
            // https://github.com/michaelfairley/rust-imgui-sdl2/issues/2#issuecomment-390321178).
            canvas.window_mut().gl_make_current(&gl_context).unwrap();
            imgui_renderer.render(ui);
            unsafe { gl::Flush(); }
        }

        canvas.present();

        if !paused {
            cpu.step_cycles(CYCLES_PER_FRAME);
        }

        while (Instant::now() - frame_start_time).subsec_nanos() < NANOS_PER_FRAME {
            thread::sleep(Duration::new(0, 1 as u32));
        }
    }
}

/// The four colors of the original Game Boy screen, from lightest to darkest, in RGB.
const GAMEBOY_COLORS: [sdl2::pixels::Color; 4] = [
    sdl2::pixels::Color { r: 155, g: 188, b: 15, a: 0xFF },
    sdl2::pixels::Color { r: 139, g: 172, b: 15, a: 0xFF },
    sdl2::pixels::Color { r: 48,  g: 98,  b: 48, a: 0xFF },
    sdl2::pixels::Color { r: 15,  g: 56,  b: 15, a: 0xFF },
];
