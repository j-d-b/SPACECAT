module Map1 where

import DataTypes
import Constants
import Visuals
import Graphics.Gloss.Data.Color
import Graphics.Gloss

--Initial States - Useful to have
map1InitState :: State
map1InitState = State
    { start          = False -- begins at a start screen
    , end            = False
    , hoveringL      = False
    , hoveringR      = False
    , hero           = initHero
    , leftState      = False
    , rightState     = False
    , upState        = False
    , platforms      = initPlatforms
    , earth          = initEarth
    , spikes         = initSpikes
    , goal           = initGoal
    , bullets        = []
    , cannons        = initCannons
    , scrollAmt       = 0
    , wallPos        = defaultWall
    , mapChoice      = 1
    , totalTime      = 0
    , complete       = False
    }

--
initEarth :: Platform
initEarth = Platform { ppos=(0, -150), psize=(800, 100), pcol=blue, ptype = Earth }



--Useful when modifying new data types to have all initial guys in here
initHero :: Hero
initHero = Hero
    { heroPos       = (-190, (earthLineAbsPos + halfHeroHeight))
    , heroVel       = (0, 0)
    , lives         = stdHeroLives
    , onGround      = False
    , inCrouch      = False
    , currHeight    = heroHeight
    , facingR       = True
    , swiped        = False
    }



defaultTop :: Position -> (Float, Float) -> Platform --Top of cannon so the hero can walk normally on top
defaultTop (cx, cy) (sizeX, sizeY) = Platform
                                        {   ppos  = (cx, cy+sizeY)
                                          , psize = (sizeX, 0.5)
                                          , pcol  = green
                                          , ptype = HardBottom
                                        }

initCannons :: [Cannon]
initCannons = [
                cConstruct (0, -80) West
              , cConstruct (0, -40) West
              , cConstruct (0, 0) East

              , cConstruct (-380, 510) East
              , cConstruct (-380, 550) East
              , cConstruct (-380, 590) East
              , cConstruct (-380, 630) East
              , cConstruct (-380, 670) East
              , cConstruct (-380, 710) East
              , cConstruct (-380, 750) East

              --Third level spikes
              , cConstruct (300, 1210) West
              , cConstruct (300, 1240) West
              , cConstruct (300, 1270) West
              , cConstruct (300, 1300) West

              --Death Squad at the Top
              , cConstruct (250, 2760) West
              , cConstruct (250, 2820) West
              , cConstruct (250, 2880) West
              , cConstruct (250, 2940) West
              , cConstruct (250, 3000) West

              , cConstruct (350, 2760) East
              , cConstruct (350, 2820) East
              , cConstruct (350, 2880) East
              , cConstruct (350, 2940) East
              , cConstruct (350, 3000) East

              , cConstruct (-200, 2760) East
              , cConstruct (-200, 2820) East
              , cConstruct (-200, 2880) East
              , cConstruct (-200, 2940) East
              , cConstruct (-200, 3000) East

    ]
initSpikes :: [Platform]
initSpikes = [
                --Cage for cannons
                pConstruct (300, 1330) 100 Spike
              , pConstruct (250, 1210) 30  Spike
              , pConstruct (250, 1230) 30  Spike
              , pConstruct (250, 1250) 30  Spike
              , pConstruct (250, 1270) 30  Spike
              , pConstruct (250, 1290) 30  Spike
              , pConstruct (250, 1310) 30  Spike
              , pConstruct (250, 1330) 30  Spike


              --Don't fall!
               , pConstruct (150, 1550) 30 Spike
               , pConstruct (200, 1700) 30 Spike
               , pConstruct (100, 1800) 30 Spike
               , pConstruct (200, 1900) 30 Spike
               , pConstruct (300, 2000) 30 Spike


               , pConstruct (-150, 1550) 30 Spike
               , pConstruct (-200, 1700) 30 Spike
               , pConstruct (-100, 1800) 30 Spike
               , pConstruct (-200, 1900) 30 Spike
               , pConstruct (-300, 2000) 30 Spike
               , pConstruct (-300, 2200) 30 Spike
               , pConstruct (100, 2300) 30 Spike
                ]



initPlatforms :: [Platform]
initPlatforms = [


                     -- the great wall
                    Platform{ ppos  = (350, 1500)
                    , psize = (10, 3200)
                    , pcol  = green
                    , ptype = HardBottom
                    }


                --Initial platforms up
                , pConstruct (140, -10)   40 SoftBottom
                , pConstruct (10, 80)     40 SoftBottom
                , pConstruct (-110, 140)  40 SoftBottom
                , pConstruct (-210, 210)  40 SoftBottom
                , pConstruct (-310, 280)  40 SoftBottom
                , pConstruct (-410, 350)  40 SoftBottom

                --A fall preventer
                , pConstruct (10, 380)   670 SoftBottom
                , pConstruct (310, 450)   30 SoftBottom
                , pConstruct (-100, 500) 750 SoftBottom

                --Stairs from the first cannon
                , pConstruct (-100, 580)  30 SoftBottom
                , pConstruct (-100, 660)  30 SoftBottom
                , pConstruct (-100, 740)  30 SoftBottom
                , pConstruct (-100, 820)  30 SoftBottom
                , pConstruct (-200, 920)  30 SoftBottom
                , pConstruct (-300, 1020) 30 SoftBottom
                , pConstruct (-400, 1120) 30 SoftBottom

                --Second fall preventer
                , pConstruct (10, 1200)  670 SoftBottom

                --Long ride up to the top!
                , pConstruct (-300, 1280) 30 SoftBottom
                , pConstruct (-210, 1360) 30 SoftBottom
                , pConstruct (-110, 1460) 30 SoftBottom
                , pConstruct (0, 1550)    30 SoftBottom

                --Right Path
                , pConstruct (100, 1650) 30 SoftBottom
                , pConstruct (200, 1750) 30 SoftBottom
                , pConstruct (315, 1850) 30 SoftBottom
                , pConstruct (200, 1950) 30 SoftBottom
                , pConstruct (100, 2050) 30 SoftBottom

                --Left Path
                , pConstruct (-100, 1650) 30 SoftBottom
                , pConstruct (-200, 1750) 30 SoftBottom
                , pConstruct (-300, 1850) 30 SoftBottom
                , pConstruct (-400, 1950) 30 SoftBottom
                , pConstruct (-300, 2050) 30 SoftBottom
                , pConstruct (-200, 2150) 30 SoftBottom
                , pConstruct (-100, 2250) 30 SoftBottom
                , pConstruct (0, 2350)    30 SoftBottom
                , pConstruct (0, 2450)    30 SoftBottom
                , pConstruct (0, 2550)    30 SoftBottom
                , pConstruct (0, 2655)    30 SoftBottom

                --Area for cannons on either side
                , pConstruct (-250, 2730) 300 HardBottom
                , pConstruct (200, 2730)  300 HardBottom

                --Ride to the top
                , pConstruct (0, 2750)    30 SoftBottom
                , pConstruct (0, 2830)    30 SoftBottom
                , pConstruct (0, 2910)    30 SoftBottom
                , pConstruct (0, 2990)    30 SoftBottom

                --left side
                ,pConstruct (-100, 3070) 30 SoftBottom
                ,pConstruct (-350, 2800) 30 SoftBottom
                ,pConstruct (-350, 2890) 30 SoftBottom

                --right Side
                ,pConstruct (100, 3070) 30 SoftBottom
                ,pConstruct (300, 2800) 30 SoftBottom
                ,pConstruct (300, 2890) 30 SoftBottom
                ,pConstruct (200, 3070) 30 SoftBottom
                ,pConstruct (330, 3050) 30 SoftBottom --debugging
                  ]

initGoal :: Goal
initGoal = Goal { goalPos = (380, -82) }
