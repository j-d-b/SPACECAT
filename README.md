# SPACECAT

![spacecat_example](http://jdbrady.info/files/spacecat.gif)

SPACECAT is a precision platform game written in Haskell, which involves navigating our hero, the spacecat, to the exit portal.

## Setup
Setup and run with the `cabal` tool.

```
cabal sandbox init
cabal install
cabal build
```

Then start *SPACECAT* with

```
cabal run
```

## Background
Uses Haskell gloss package for animation and 2D rendering.

Written with [imcinerney](https://github.com/imcinerney) as our Functional Programming course final project, though I put more work into cleaning it up and adding features after the course for a v1.1 release.

## Instructions
SPACECAT gameplay requires both precisely timed keypresses and more restful thinking to master MAP II. Use MAP I to practice
and learn the controls.

The objective is to reach the gate, which appears as a purple,
rectangular portal.

Move the cat around with the arrow keys:
  * `LEFT` Move left
  * `RIGHT`: Move Right
  * `UP`: Jump
  * `DOWN`: Crouch

`SPACEBAR`: Paw swipe. Can use this to attack cannons when close.

Press `q` to return to the home screen at any time.

Press `r` to restart a given level.

*Note*: crouching while in the air allows the cat to lift its feet a bit higher, and reach platforms otherwise unreachable, if well timed.

There are three types of platforms: `hardbottom`, `softbottom`, and `spike`.

The cat can jump through `softbottom` platforms from below, but not `hardbottom`.

`spikes` cause you to lose a life for every frame you are touching them.

`cannons` will shoot at the cat from any distance when he enters their line of sight; from one projectile hit, you will lose one life; *remaining lives are displayed in the top left*.

Close the game/window with the escape key.

**Good luck out there!**
