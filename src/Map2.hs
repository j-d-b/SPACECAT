module Map2 where

-- JACOB'S MAP --

import DataTypes
import Constants
import Visuals
import Graphics.Gloss.Data.Color
import Graphics.Gloss


map2InitState :: State
map2InitState = State
    { start          = False
    , end            = False
    , hoveringL      = False
    , hoveringR      = False
    , hero           = initHero2
    , leftState      = False
    , rightState     = False
    , upState        = False
    , platforms      = map2Platforms
    , earth          = initEarth2
    , spikes         = initSpikes2
    , goal           = initGoal2
    , bullets        = []
    , cannons        = initCannons2
    , scrollAmt      = 0
    , wallPos        = defaultWall
    , mapChoice      = 2
    , totalTime      = 0
    , complete       = False
    }

initEarth2 :: Platform
initEarth2 = Platform { ppos=(0, -150), psize=(800, 100), pcol=blue, ptype = Earth }

initSpikes2 :: [Platform]
initSpikes2 = [ pConstruct (-350, 35) 40 Spike
              , pConstruct (-320, 35) 40 Spike
              , pConstruct (-290, 35) 40 Spike
              , pConstruct (-260, 35) 40 Spike
              , pConstruct (-230, 35) 40 Spike
              , pConstruct (-140, 110) 40 Spike
              , pConstruct (-20, 310) 40 Spike
              , pConstruct (-60, 390) 40 Spike
              , pConstruct (-20, 470) 40 Spike
              , pConstruct (-60, 550) 40 Spike

              -- falling gates
              , pConstruct (110, 3850) 40 Spike
              , pConstruct (190, 3850) 40 Spike

              , pConstruct (180, 3550) 40 Spike
              , pConstruct (260, 3550) 40 Spike

              , pConstruct (230, 3250) 40 Spike
              , pConstruct (310, 3250) 40 Spike
              -- end

              ,pConstruct (390, 2970) 40 Spike
              ,pConstruct (390, 2870) 40 Spike
              ,pConstruct (390, 2770) 40 Spike
              ,pConstruct (390, 2670) 40 Spike
              ,pConstruct (390, 2570) 40 Spike
              ,pConstruct (390, 2470) 40 Spike
              ,pConstruct (390, 2370) 40 Spike
              ,pConstruct (390, 2270) 40 Spike


              -- duplicates on other side
              ,pConstruct (310, 2970) 40 Spike
              ,pConstruct (315, 2870) 40 Spike
              ,pConstruct (315, 2770) 40 Spike
              ,pConstruct (315, 2670) 40 Spike
              ,pConstruct (315, 2570) 40 Spike
              ,pConstruct (315, 2470) 40 Spike
              ,pConstruct (315, 2370) 40 Spike
              ,pConstruct (315, 2270) 40 Spike


                ]

-- note, these are ordered by where you have to jump to beat the map
map2Platforms :: [Platform]
map2Platforms = [ pConstruct (-350, 2) 40 HardBottom
                , pConstruct (-350, 80) 40 HardBottom
                , pConstruct (-190, 70) 20 SoftBottom
                , pConstruct (-120, 70) 20 SoftBottom
                , pConstruct (-40, 173) 40 SoftBottom
                , pConstruct (-40, 270) 40 SoftBottom
                , pConstruct (-40, 350) 40 SoftBottom
                , pConstruct (-40, 430) 40 SoftBottom
                , pConstruct (-40, 510) 40 SoftBottom
                , pConstruct (-40, 600) 40 SoftBottom
                , pConstruct (-200, 680) 80 HardBottom
                , pConstruct (-412, 720) 30 SoftBottom
                , pConstruct (-280, 790) 40 HardBottom
                , pConstruct (100, 790) 400 HardBottom

                -- secret stairs
                , pConstruct (400, 870) 5 SoftBottom
                , pConstruct (400, 970) 5 SoftBottom
                , pConstruct (400, 1070) 5 SoftBottom
                , pConstruct (400, 1170) 5 SoftBottom
                , pConstruct (400, 1270) 5 SoftBottom
                , pConstruct (400, 1370) 5 SoftBottom
                , pConstruct (400, 1470) 5 SoftBottom
                , pConstruct (400, 1570) 5 SoftBottom
                , pConstruct (400, 1670) 5 SoftBottom
                , pConstruct (400, 1770) 5 SoftBottom

                , pConstruct (150, 1080) 40 SoftBottom
                , pConstruct (-10, 1100) 6 SoftBottom
                , pConstruct (-150, 1120) 6 SoftBottom
                , pConstruct (-290, 1130) 6 SoftBottom
                , pConstruct (-400, 1180) 6 SoftBottom
                , pConstruct (-403, 1285) 6 SoftBottom
                , pConstruct (-403, 1390) 6 SoftBottom
                , pConstruct (-403, 1495) 6 SoftBottom
                , pConstruct (-320, 1600) 40 SoftBottom
                , pConstruct (-320, 1680) 40 HardBottom
                , pConstruct (-320, 1770) 40 HardBottom
                , pConstruct (-320, 1870) 40 HardBottom
                , pConstruct (-320, 1975) 40 HardBottom
                , pConstruct (-200, 2030) 1 SoftBottom
                , pConstruct (-320, 2085) 40 HardBottom
                , pConstruct (-200, 2190) 40 SoftBottom
                , pConstruct (-80, 2295) 40 SoftBottom
                , pConstruct (40, 2400) 40 SoftBottom
                , pConstruct (0, 2500) 2 HardBottom
                , pConstruct (-80, 2600) 2 HardBottom
                , pConstruct (-160, 2700) 2 HardBottom
                , pConstruct (-265, 2805) 40 SoftBottom
                , pConstruct (-265, 2905) 40 SoftBottom
                , pConstruct (-245, 3005) 40 SoftBottom
                , pConstruct (-225, 3105) 40 SoftBottom
                , pConstruct (-205, 3205) 40 SoftBottom
                , pConstruct (-185, 3305) 40 SoftBottom
                , pConstruct (-165, 3405) 40 SoftBottom
                , pConstruct (-145, 3505) 40 SoftBottom
                , pConstruct (-125, 3605) 40 SoftBottom
                , pConstruct (-105, 3705) 40 SoftBottom
                , pConstruct (0, 3805) 100 HardBottom
                , pConstruct (330, 3000) 80 SoftBottom
                , pConstruct (350, 2900) 20 SoftBottom
                , pConstruct (350, 2800) 20 SoftBottom
                , pConstruct (350, 2700) 20 SoftBottom
                , pConstruct (350, 2600) 20 SoftBottom
                , pConstruct (350, 2500) 20 SoftBottom
                , pConstruct (350, 2400) 20 SoftBottom
                , pConstruct (350, 2300) 20 SoftBottom


                , pConstruct (350, 2156) 40 SoftBottom -- for goal
                ]

initCannons2 :: [Cannon]
initCannons2 =  [ cConstruct (60, earthLineAbsPos + 15) West
                , cConstruct (100, earthLineAbsPos + 15) West

                , cConstruct (-350, 290) East
                , cConstruct (270, 370) West
                , cConstruct (-350, 450) East
                , cConstruct (270, 530) West
                , cConstruct (-350, 620) East

                , cConstruct (-150, 840) West
                , cConstruct (-50, 920) West
                , cConstruct (50, 990) West

                , cConstruct (350, 1880) North
                , cConstruct (310, 2917) North


                ]

initHero2 :: Hero
initHero2 = Hero
    { heroPos       = (-140, (earthLineAbsPos + halfHeroHeight))
    , heroVel       = (0, 0)
    , lives         = 5
    , onGround      = False
    , inCrouch      = False
    , currHeight    = heroHeight
    , facingR       = True
    , swiped        = False
    }

initGoal2 :: Goal
initGoal2 = Goal { goalPos = (350, 2178) }

initPlatforms2 :: [Platform]
initPlatforms2 = [ ]
