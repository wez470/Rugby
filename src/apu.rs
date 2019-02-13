pub struct Audio {
    channel3: Channel3
}

impl Audio {
    pub fn new() -> Audio {
        Audio {
            channel3: Channel3::new()
        }
    }
}

pub enum Volume {
    Zero = 0,
    Full = 1,
    Half = 2,
    Quarter = 3,
}

pub struct Channel3 {
    /// True if sound is on. Register FF1A
    on: bool,

    /// Sound Length. Register FF1B
    length: u8,

    /// Volume. Register FF1C
    volume: Volume,

    /// Frequency. Register FF1D and Bits 0-2 of Register FF1E
    frequency: u16,

    /// True if we are going to restart sound. TODO(wcarlson): What is this?
    restart: bool,

    /// True if we should stop after the current sound length
    stop: bool,

    /// Wave pattern RAM. Registers FF30-FF3F
    wave_ram: Box<[u8]>,
}

impl Channel3 {
    pub fn new() -> Channel3 {
        Channel3 {
            on: false,
            length: 0,
            volume: Volume::Zero,
            frequency: 0,
            restart: false,
            stop: false,
            wave_ram: vec![0; 16].into_boxed_slice(),
        }
    }
}