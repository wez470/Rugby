# Rugby: Rust Game Boy? Yes!
[![Build Status](https://travis-ci.org/wez470/Rugby.svg?branch=master)](https://travis-ci.org/wez470/Rugby)

# Building / Running
1. Install SDL2 dev and SDL2 graphics dev libraries.
2. `cargo run --release run <ROM>`, `cargo run --release run <ROM> -s <SAVE>`

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
pause       = p
1           = save (this is a battery save, not a snapshot. save-file needs to be provided to use.)
```

<img src="https://media3.giphy.com/media/zsPv55feI0PdeIEDvs/giphy.gif?cid=790b76116f660dc27e73302163eb5d954264f4c3b07d9aaf&rid=giphy.gif&ct=g" alt="Rugby Gameplay" width="300"/>

<img src="https://i.imgur.com/iViGdsG.png" alt="Rugby Debugger" width="600">
