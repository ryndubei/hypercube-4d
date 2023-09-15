hypercube-4d
=========

Full-visibility 4D renderer written in Haskell with Gloss. Inspired by the 4D Maze Game: <https://www.urticator.net/maze/>

Build
-----

Debian/Ubuntu-based Linux distributions:

```
sudo apt install freeglut3-dev haskell-stack
cd hypercube-4d
stack setup
stack build
```
Then use `stack run` to run the program.

Controls
--------

### Miscellaneous
- 'g': Return to starting state
- 'b': Toggle debug view

### Movement
- 'w','s': forward/backward
- 'a','d': left/right
- 'r','f': up/down
- 'q','e': in/out

### Turning
- 'h','l': left/right
- 'k','j': up/down
- 'u','i': in/out

License
-------

GPL-3.0-or-later
