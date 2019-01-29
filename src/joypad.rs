use sdl2::keyboard::Keycode;

#[derive(copy)]
pub enum Button {
    Start = 0b00001000,
    Down = 0b00001000,
    Select = 0b00000100,
    Up = 0b00000100,
    Left = 0b00000010,
    B = 0b00000010,
    Right = 0b00000001,
    A = 0b00000001,
}

impl std::convert::From<Keycode> for Button {
    fn from(key: Keycode) -> Button {
        match key {
            Keycode::Up => Button::Up,
            Keycode::Down => Button::Down,
            Keycode::Left => Button::Left,
            Keycode::Right => Button::Right,
            Keycode::Return => Button::Start,
            Keycode::Z => Button::A,
            Keycode::X => Button::B,
            Keycode::Backspace => Button::Select,
            _ => panic!("Invalid key"),
        }
    }
}