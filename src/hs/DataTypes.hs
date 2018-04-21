module DataTypes where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Constants

-- Data for state of game
data State = State
    { start          :: Bool             -- true = start screen
    , end            :: Bool             -- true = gg screen
    , hoveringL      :: Bool             -- start screen left map
    , hoveringR      :: Bool             -- start screen right map
    , hero           :: Hero
    , leftState      :: Bool             -- is left arrow pressed down
    , rightState     :: Bool             -- is right arrow pressed down
    , upState        :: Bool             -- is up arrow pressed down
    , platforms      :: [Platform]       -- list of the current platforms
    , earth          :: Platform         -- earth line
    , spikes         :: [Platform]
    , goal           :: Goal
    , bullets        :: [Bullet]         -- list of the current bullets
    , cannons        :: [Cannon]         -- list of current cannons
    , scrollAmt      :: Float            -- starting position y of the glass box
    , wallPos        :: (Float, Float)   -- the left and right bounds
    , mapChoice      :: Int              -- 1 for map 1, 2 for map 2
    , totalTime      :: Float            -- keeps track of playtime
    , complete       :: Bool             -- true when won
    }

-- Data Structures
data Platform = Platform
    { ppos   :: (Float, Float)
    , psize  :: (Float, Float)
    , pcol   :: Color
    , ptype  :: PlatformType
    }

data Bullet = Bullet
    { bulletPos :: Position
    , bulletVel :: (Float, Float)
    }

data Cannon = Cannon
    { cannonPos       :: Position
    , csize           :: (Float, Float)
    , reloadTime      :: Seconds
    , percentHealth   :: Float
    , cannonHealth    :: Float
    , cannonMaxHealth :: Float
    , cannonColor     :: Color
    , angle           :: Direction
    , top             :: Platform
    }

data Direction = North
               | East
               | South
               | West
    deriving Eq

data Goal = Goal { goalPos :: Position }

data PlatformType = SoftBottom
                  | HardBottom
                  | Earth
                  | Spike
    deriving Eq


data Hero = Hero
    { heroPos    :: Position       -- hero (x, y) position
    , heroVel    :: (Float, Float) -- hero (vx, vy) velocity
    , lives      :: Float          -- hero number of lives
    , onGround   :: Bool           -- is our hero on the ground
    , inCrouch   :: Bool           -- is our hero crouched
    , currHeight :: Float          -- current eight of the hero
    , facingR    :: Bool           -- is our hero facing right
    , swiped     :: Bool             -- has he swiped
    }

-- make a standard height platform easily
pConstruct :: Position -> Float -> PlatformType -> Platform
pConstruct posit wid typ = Platform { ppos = posit
                                    , psize = (wid, stdPlatformThickness)
                                    , pcol = blue , ptype = typ }

-- make a standard size & health cannon easily
cConstruct :: Position -> Direction -> Cannon
cConstruct posit@(x, y) dir =
        Cannon { cannonPos       = posit
               , csize           = stdCannonSize
               , reloadTime      = 0
               , percentHealth   = 1
               , cannonHealth    = normalCannonHealth
               , cannonMaxHealth = normalCannonHealth
               , cannonColor     = black
               , angle           = dir
               , top             = pConstruct (x, y - 1) (fst stdCannonSize) HardBottom
               }
