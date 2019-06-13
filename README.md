# Rugby: Rust Game Boy? Yes!
[![Build Status](https://travis-ci.org/wez470/Rugby.svg?branch=master)](https://travis-ci.org/wez470/Rugby)

# Building / Running
1. Install SDL2 dev and SDL2 graphics dev libraries.
2. `cargo run --release run <ROM>`

### Debug Mode
Rugby has an interactive CLI debugger that can be started with:
1. `cargo run --release debug <ROM>`


# Controls
```
start       = enter
select      = tab
a           = k
b           = j
up          = w
left        = a
down        = s
right       = d
speed x2    = right bracket (max x4)
speed x0.5  = left bracket  (min x0.25)
pause       = p
```

<img src="https://i.imgur.com/u30jZ22.png" alt="Rugby Gameplay" width="300"/>

<img src="https://i.imgur.com/iViGdsG.png" alt="Rugby Debugger" width="300">