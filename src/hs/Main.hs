module Main where

import Data.Fixed
import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Interface.Pure.Game

import Constants
import Visuals
import Misc
import Hero
import DataTypes
import Map1
import Map2

-- Jacob Brady & Ian McInerney

-- convert state into a start screen picture
startScreen :: State -> Picture -> Picture -> (Picture, Picture) -> (Picture, Picture) -> Picture
startScreen state homeBgImg overlayImg (mapImgI, mapImgII) (map1InfoImg, map2InfoImg) = pictures [homeBgImg, overlayDim, map1, map2, title, ldim, rdim, map1Info, map2Info]
    where
            -- note map img should be 300x150
            map1 = translate (-160) (-65) $ mapImgI
            map2 = translate (160) (-65) $ mapImgII

            title = translate 0 12 $ overlayImg

            map1Info = translate (-160) (-65) $ map1InfoImg
            map2Info = translate (160) (-65) $ map2InfoImg

            ldim
                | hoveringL state  = translate (-160) (-65) $ color (makeColorI 120 120 120 100) $ rectangleSolid 292 142
                | otherwise        = blank
            rdim
                | hoveringR state  = translate (160) (-65) $ color (makeColorI 120 120 120 100) $ rectangleSolid 292 142
                | otherwise        = blank

            overlayDim = color (makeColorI 220 220 220 110) $ rectangleSolid 800 400

endGameScreen :: State -> Picture -> Picture -> (Picture, Picture) -> Picture
endGameScreen state failedImg successImg (homeImg, retryImg) = pictures [ backg, behindMsg, msg, ldim, rdim, hm, rty, time, livesRem, tip, tip2 ]
    where
          backg = color (makeColorI 169 169 169 180) $ rectangleSolid 800 400 -- overlay

          hm  = translate (160) (-65) $  homeImg
          rty = translate (-160) (-65) $  retryImg

          ldim
              | hoveringL state  = translate (-160) (-70) $ color (makeColorI 100 100 100 165) $ rectangleSolid 265 95
              | otherwise        = translate (-160) (-70) $ color (makeColorI 100 100 100 80) $ rectangleSolid 265 95
          rdim
              | hoveringR state  = translate (160) (-70) $ color (makeColorI 100 100 100 165) $ rectangleSolid 265 95
              | otherwise        = translate (160) (-70) $ color (makeColorI 100 100 100 80) $ rectangleSolid 265 95
          msg
              | complete state  = pictures  $ (translate (-24) 147 $ scale 0.13 0.13 $ Text ("MAP" ++ (show $ mapChoice state))) : [translate 0 100 $ successImg]
              | otherwise       = pictures  $ (translate (-24) 147 $ scale 0.13 0.13 $ Text ("MAP" ++ (show $ mapChoice state))) : [translate 0 100 $ failedImg]
          livesRem
              | complete state  = translate (-260) (-5) $ scale 0.2 0.2 $ Text ("Lives Rem: " ++ (show $ lives $ hero state))
              | otherwise       = Blank
          behindMsg
              | complete state  = translate 0 100 $ color (makeColorI 100 100 100 160) $ rectangleSolid 540 136
              | otherwise       = translate 0 100 $ color (makeColorI 100 100 100 160) $ rectangleSolid 440 136 -- failed
          time
              | complete state  = translate 55.5 (-5) $ scale 0.2 0.2 $ Text ("Time: " ++ (show $ totalTime state))
              | otherwise       = Blank
          tip  = translate (-208) (-104) $ scale 0.098 0.098 $ Text "press r to retry"
          tip2 = translate 93 (-104) $ scale 0.098 0.098 $ Text "press q to go home"

-- relevant to play function, keep in main
window :: Display
window = InWindow "-- spacecat --" (800, 400) (100, 100)

-- relevant to play function, keep in main
bgColor :: Color
bgColor = greyN 0.5

-- convert game state into Picture. first picture is of hero from file
-- (FacingRBMP, FacingLBMP) second tuple is platforms (soft, hard), third is
-- life image, fourth is goal image. Fifth is ground image, 7th is spike img,
-- 6th is cannon images, containing health bar ones too, and all cannon directions
-- 8th is bgImg
-- Probably end up making these lists, not tuples, unless the idea
-- of a fixed number (e.g. left and right).
render :: [Picture] -> (Picture, Picture) -> Picture -> Picture -> Picture -> [Picture] -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> (Picture, Picture) -> (Picture, Picture) -> (Picture, Picture) -> State -> Picture
render heroImg (platSoft, platHard) lifeImg goalImg earthImg cannonImgs spikeImg bgImg failedImg successImg homeBgImg startOverlayImg mapsTuple homeRetryTuple infoTuple state
    | start state = startScreen state homeBgImg startOverlayImg mapsTuple infoTuple
    | end state   = pictures [ atmosphere, currEarth, pictures currPlatforms
                             , pictures currSpikes, pictures currCannons, currGoal
                             , picHero, pictures currBullets, pictures lifes
                             , endGameScreen state failedImg successImg homeRetryTuple
                             ]
    | otherwise   = pictures [ atmosphere, currEarth, pictures currPlatforms
                             , pictures currSpikes, pictures currCannons, currGoal
                             , picHero, pictures currBullets, pictures lifes
                             ]
        where
            (hx, hy) = heroPos $ hero state
            state' = scroll state (scrollAmt state)

            picHero       = drawHero (hero state') heroImg
            currBullets   = drawBullets (bullets state')
            currCannons   = drawCannons cannonImgs (cannons state')
            lifes         = drawLives (lives (hero state)) lifeImg
            currGoal      = drawGoal (goal state') goalImg
            currPlatforms = platformGenerator (platforms state') (platSoft, platHard)
            currSpikes    = platformGenerator (spikes state') (spikeImg, spikeImg)
            currEarth     = platToPic (earthImg, earthImg) (earth state') -- snd not used, change function to use a list, also this might not change
            atmosphere    = bgImg -- color (greyN 0.2) $ rectangleSolid 800 400


-- takes a state and a float which is the amount to subtract from the y coord
-- of everything that composes the map, and
scroll :: State -> Float -> State
scroll state trans = state { hero = hero' , cannons = cannons', bullets = bullets'
                           , spikes = spikes' , platforms = platforms', goal = goal'
                           , earth = earth'
                           }
    where
        (hx, hy) = heroPos $ hero state
        hero' = (hero state) { heroPos = (hx, hy - trans) }

        cannons'   = map (\c -> c { cannonPos = scrollY (cannonPos c) trans }) (cannons state)
        bullets'   = map (\b -> b { bulletPos = scrollY (bulletPos b) trans }) (bullets state)
        platforms' = map (\p -> p { ppos = scrollY (ppos p) trans }) (platforms state)
        spikes'    = map (\s -> s { ppos = scrollY (ppos s) trans }) (spikes state)
        earth'     = (\e -> e { ppos = scrollY (ppos e) trans }) $ earth state
        goal'      = (\g -> g { goalPos = scrollY (goalPos g) trans }) $ goal state


-- takes a position and a float which is the amount to subtract from the
-- y coordinate of the position
scrollY :: Position -> Float -> Position
scrollY (x, y) z = (x, y - z)


------- KEY HANDLING -------

-- pass to play in main
getInput :: Event -> State -> State
getInput e state | start state = selectOption e state
                 | end state   = endOption e state
                 | otherwise   = heroMovement e state

-- respond to arrow keys and move hero based on this
-- if l/r key pressed, give hero a sideVel push
-- if up key pressed, initiate jump, if down, crouch
-- in general, sets key state which is later used to determine movement
heroMovement :: Event -> State -> State
heroMovement (EventKey (SpecialKey KeyRight) Down _ _) state =
    state { hero = currHero { heroVel = (sideVel, vy) , facingR = True } , rightState = True }
        where
            currHero = hero state
            (_, vy)  = heroVel currHero
heroMovement (EventKey (SpecialKey KeyLeft) Down _ _) state =
    state { hero = currHero { heroVel = (-sideVel, vy) , facingR = False } , leftState = True }
        where
            currHero = hero state
            (_, vy) = heroVel currHero
heroMovement (EventKey (SpecialKey KeyRight) Up _ _) state =
    state { rightState = False }
heroMovement (EventKey (SpecialKey KeyLeft) Up _ _) state =
    state { leftState = False }
heroMovement (EventKey (SpecialKey KeyUp) Down _ _) state =
    state { upState = True }
heroMovement (EventKey (SpecialKey KeyUp) Up _ _) state =
    state { upState = False }
heroMovement (EventKey (SpecialKey KeyDown) Down _ _) state =
    state { hero = currHero { heroPos = (x, y) , inCrouch = True , currHeight = halfHeroHeight } }
        where
            currHero  = hero state
            grounded  = onGround currHero
            crouched  = inCrouch currHero
            (x, y)    = heroPos currHero
            y'
                | crouched                   = y
                | grounded                   = y
                | otherwise                  = y + (halfHeroHeight / 2)
heroMovement (EventKey (SpecialKey KeyDown) Up _ _) state =
    state {hero = currHero {heroPos = (x, y), inCrouch = False, currHeight = heroHeight}}
        where
            currHero = hero state
            grounded = onGround currHero
            crouched = inCrouch currHero
            (x,y)    = heroPos currHero
            y'
                | grounded = y
                | otherwise = y - (halfHeroHeight / 2)


heroMovement (EventKey (Char 'q') Down _ _) state = -- q to return home
    state { start = True }
heroMovement (EventKey (Char 'r') Down _ _) state -- r to restart level
    | mapChoice state == 1   = map1InitState
    | otherwise              = map2InitState

heroMovement (EventKey (SpecialKey KeySpace) Down _ _) state =
    state { cannons = cannons', hero = hero' }
        where cannons' = heroAttack (hero state) (cannons state)
              hero'    = (hero state) {swiped = True}
heroMovement (EventKey (SpecialKey KeySpace) Up _ _) state =
        state {hero = (hero state) {swiped = False} }
heroMovement _ state = state

-- for start screen controls
selectOption :: Event -> State -> State
selectOption (EventKey (MouseButton LeftButton) Down _ (x, y)) state -- lauch the game
    | onLeftMap (x, y)  = map1InitState  -- add map choice here
    | onRightMap (x, y) = map2InitState
    | otherwise         = state
selectOption (EventMotion (x, y)) state -- combine this and te one above
    | onLeftMap (x, y)  = state { hoveringL = True , hoveringR = False }
    | onRightMap (x, y) = state { hoveringR = True , hoveringL = False }
    | otherwise         = state { hoveringR = False , hoveringL = False }
selectOption _ state = state

-- end screen controls
endOption :: Event -> State -> State
endOption (EventKey (MouseButton LeftButton) Down _ (x, y)) state -- lauch the game
    | onLeftMap (x, y)  = if (mapChoice state) == 1 then map1InitState else map2InitState  -- retry
    | onRightMap (x, y) = state { start = True }  -- go home
    | otherwise         = state
endOption (EventMotion (x, y)) state -- combine this and te one above
    | onLeftMap (x, y)  = state { hoveringL = True , hoveringR = False }
    | onRightMap (x, y) = state { hoveringR = True , hoveringL = False }
    | otherwise         = state { hoveringR = False , hoveringL = False }
endOption (EventKey (Char 'r') Down _ _) state = if (mapChoice state) == 1 then map1InitState else map2InitState  -- press r to retry
endOption (EventKey (Char 'q') Down _ _) state = state { start = True }
endOption _ state = state

-- remove these two below to generalize maps
onLeftMap :: Position -> Bool
onLeftMap (x, y) = x > (-310) && x < -10 && y > (-140) && y < 10

onRightMap :: Position -> Bool
onRightMap (x, y) = x < 310 && x > 10 && y > (-140) && y < 10


----- MOTION -----

-- takes current state and number of seconds and creates next state
motion :: Seconds -> State -> State
motion s state
    | end state || start state = state
    | otherwise = state { hero = hero', cannons = cannons' , bullets = bullets'
                        , scrollAmt = sa' , totalTime = (totalTime state) + s
                        }
        where
            hero'      = moveHero s state
            bullets'   = moveBullets s (bullets state)
            cannons'   = decreaseTime s (cannons state) -- reload the cannon

            (hx, hy) = heroPos $ hero state
            (hvx, hvy) = heroVel hero'
            (hx', hy') = heroPos hero'

            sa = scrollAmt state
            satemp
                | hy > thresholdTop + sa     = (hy - thresholdTop)
                | hy < thresholdBottom + sa  = (hy - thresholdBottom)
                | otherwise                  = sa
            sa'
                | satemp < 0                 = 0
                | otherwise                  = satemp


moveHero :: Seconds -> State -> Hero
moveHero s state = currHero { heroPos = (x', y') , heroVel = (vx'', vy'') , onGround = grounded' , lives = (lives currHero) - takeIf 1 isSpiked }
    where
        -- relevant current location, velocities, & key states
        currHero = hero state
        (x, y)   = heroPos currHero
        (vx, vy) = heroVel currHero
        (leftWall, rightWall) = wallPos state
        left     = leftState state
        right    = rightState state
        jump     = upState state
        crouched = inCrouch currHero
        grounded = onGround currHero
        hheight  = currHeight currHero
        cans     = cannons state -- great variable name, ian, haha

        -- new location
        xtemp     | x + (vx * s) <= leftWall = leftWall
                  | x + (vx * s) >= rightWall = rightWall
                  | otherwise                 = x + (vx * s)
        ytemp     = y + (vy * s) -- update w/ physics


        topColSpike  = fst (isGrounded currHero (xtemp, ytemp) (spikes state)) -- if landed on spikes, dies
        botColSpike  = fst $ fst (collision currHero (xtemp, ytemp) (spikes state))
        sideColSpike = snd $ fst (collision currHero (xtemp, ytemp) (spikes state))
        isSpiked  = topColSpike || botColSpike || sideColSpike

        posInfo   = isGrounded currHero (xtemp, ytemp) ((getPlats cans) ++ (earth state) : (platforms state) ++ (spikes state))
        grounded' = fst posInfo
        (xtemp', ytemp') = snd posInfo  -- update w/ possible collisions w/ ground
        colPosInfo  = collision currHero (xtemp', ytemp') ((getPlats cans) ++ (earth state) : (platforms state) ++ (spikes state)) -- hitting the bottom of hard platform
        (x', y')  = snd colPosInfo
        collided = fst colPosInfo

        -- canCollidedInfo = cannonCollide (heroPos currHero) (facingR currHero) (cannons state)
        -- (x', y') | fst canCollidedInfo = tag $ snd canCollidedInfo
        --          | otherwise           = (x'',y'')
        -- -- || cannonCollide currHero (cannons state)

        -- colCannon = cannonCollide (vx, vy) (x', y') (facingR currHero) (cannons state)
        -- (vxCan, vyCan)
        --     | colCannon == Nothing = (vx, vy)
        --     | otherwise            = tag colCannon
        --
        -- grounded' = grounded'' || (colCannon /= Nothing && vy' == 0)

        -- new velocities
        friction
            | crouched  = stdFriction * crouchedFricModifier
            | otherwise = stdFriction
        vxtemp -- update w/ physics   -> note: no friction in the air
            | vx >= friction    = vx - (takeIf friction grounded)
            | vx <= -friction   = vx + (takeIf friction grounded)
            | otherwise         = 0
        vx' -- update due to key states
            | snd collided = 0
            | vxtemp < maxCrouchedSideVel && vxtemp >= 0 && right && crouched   = vx + (sideAccel * crouchedAccelModifier)
            | vxtemp > -maxCrouchedSideVel && vxtemp <= 0 && left && crouched   = vx - (sideAccel * crouchedAccelModifier)
            | vxtemp < maxSideVel && vxtemp >= 0 && right && not crouched       = vx + sideAccel
            | vxtemp > -maxSideVel && vxtemp <= 0 && left && not crouched       = vx - sideAccel
            | otherwise                                                         = vxtemp

        vytemp
            | fst collided  = 0
            | otherwise = vy + gravity -- update w/ physics
        vy' -- update due to key states
            | grounded && jump                        = jumpVel
            | grounded && not jump                    = 0 -- doesn't accumulate neg vy from grav when standing on ground
            | not grounded && not jump && vytemp > 0  = min vytemp minJumpVel
            | otherwise                               = vytemp

        colCannon = cannonCollide (vx', vy') (x', y') (facingR currHero) (cannons state)
        (vx'', vy'')
            | colCannon == Nothing = (vx', vy')
            | otherwise            = tag colCannon
-- orCombine :: (Bool, Maybe Position) -> (Bool, Maybe Position) -> (Bool, Maybe Position)
-- orCombine (True, a) _  = (True, a)
-- orCombine _ (True, b)  = (True, b)
-- orCombine (False, _) _ = (False, Nothing)

--Extracts the list of platforms for a cannon
getPlats :: [Cannon] -> [Platform]
getPlats cs = map (\x -> top x) cs

--
cannonCollide' :: Velocity -> Position -> Bool -> Cannon -> (Maybe Position)
cannonCollide' v hpos r c | inRange stdCannonSize hpos cpos = newHeroPos v hpos r c
                          | otherwise                       = (Nothing)
            where
                cpos = cannonPos c

--Prevents the hero from going into the cannon from the side. Does not alter
--if the hero is on top of the cannon
newHeroPos :: Velocity -> Position -> Bool -> Cannon -> (Maybe Position)
newHeroPos (vx, vy) (hx, hy) r c | (hx > cx) && not r && (hy < cy) =  Just (0, vy)
                                 | (hx < cx) && r && (hy < cy)     = Just (0, vy)
                                 | otherwise           = Nothing
              where
                (cx, cy) = cannonPos c


--I couldn't get mappend working, so I ended up calculating the old fashioned
--way
addMaybes :: Maybe (Float, Float) -> Maybe (Float, Float) -> Maybe (Float, Float)
addMaybes Nothing x         = x
addMaybes x Nothing         = x
addMaybes (Just x) (Just y) = Just (x+y)
-- Jacob's Note about this: Ian, what the hell is this. Shouldn't this be functor


cannonCollide :: Velocity -> Position -> Bool -> [Cannon] -> (Maybe Position)
cannonCollide vel hpos r cs = foldr addMaybes Nothing (map (cannonCollide' vel hpos r) cs )


-- Returns true if hero is not in the air. Also returns hero's
-- new position, taking into account if he landed on a platforms
-- can be used to just find out just if he is grounded by ignoring the snd of
-- the tuple. Takes state, projected next position (by velocities),
-- and a list of platforms (the map).
isGrounded :: Hero -> Position -> [Platform] -> (Bool, Position)
isGrounded currHero (x', y') [] = (False, (x', y'))
isGrounded currHero (x', y') (p:ps)
    | onPlatform    = ( True, (x', platTop + halfHeroHeight) )
    | otherwise     = isGrounded currHero (x', y') ps
        where
            (x, y) = heroPos currHero -- hero pos before this iteration's physics

            crouched = inCrouch currHero
            grounded = onGround currHero
            y'cr
                | crouched && (not grounded) = y' + (halfHeroHeight / 2)
                | otherwise = y'
            ycr
                | crouched && (not grounded) = y + (halfHeroHeight / 2)
                | otherwise = y

            currPlatform = p

            (platPosX, platPosY) = ppos currPlatform
            (platWidth, platHeight) = psize currPlatform

            platTop    = platPosY + platHeight / 2
            platBottom = platPosY - platHeight / 2
            rightEdge  = platPosX + platWidth / 2
            leftEdge   = platPosX - platWidth / 2

            onPlatform  = ( ycr >= platTop + halfHeroHeight  -- comes from above
                          && y'cr <= platTop + halfHeroHeight -- ends at/below
                          && x' > leftEdge - (heroWidth / 6)
                          && x' < rightEdge + (heroWidth / 6) )
                    -- ^ falls or doesn't land if heros is more than 2/3 off

-- checks to see if hero has hit something he can't go through or land on
-- currently only checks bottom of HardBottom Platform
collision :: Hero -> Position -> [Platform] -> ( (Bool, Bool), Position)
collision currHero (x', y') [] = ((False, False), (x', y'))
collision currHero (x', y') ps
    | (fst $ fst platColl) || (snd $ fst platColl) = platColl
    | otherwise     = collision currHero (x', y') remainingPlatforms
        where
            (x, y) = heroPos currHero
            crouched = inCrouch currHero
            grounded = onGround currHero
            currHalfHeroHeight | grounded  = halfHeroHeight
                               | otherwise = takeIfElse (heroHeight/4) halfHeroHeight crouched

            currPlatform = head ps
            currType = ptype currPlatform
            remainingPlatforms = tail ps

            (platPosX, platPosY) = ppos currPlatform
            (platWidth, platHeight) = psize currPlatform

            platTop    = platPosY + platHeight / 2
            platBottom = platPosY - platHeight / 2
            rightEdge  = platPosX + platWidth / 2
            leftEdge   = platPosX - platWidth / 2

            platColl | y <= platBottom - currHalfHeroHeight -- comes from below
                       && y' >= platBottom - currHalfHeroHeight -- ends above
                       && x' > leftEdge - (heroWidth / 6)
                       && x' < rightEdge + (heroWidth / 6)
                       && (currType == HardBottom || currType == Spike) = ( (True, False), (x',y))

                     | x <= leftEdge - (halfHeroWidth / 2) --come from the left
                      && x' >= leftEdge - (halfHeroWidth /2) --ends in the platform
                      && y' > platBottom - currHalfHeroHeight
                      && y' < platTop + currHalfHeroHeight
                      && (currType == HardBottom || currType == Spike) = ((False, True), (x,y'))

                     | x >= rightEdge + (halfHeroWidth / 2)
                      && x' <= rightEdge + (halfHeroWidth / 2)
                      && y' > platBottom - currHalfHeroHeight
                      && y' < platTop + currHalfHeroHeight
                      && (currType == HardBottom || currType == Spike) = ((False,True), (x,y') )
                     | otherwise = ( (False, False), (x',y') )


--Moves one bullet based on physics (assumes no acceleration)
moveBullet :: Seconds -> Bullet -> Bullet
moveBullet s b = b {bulletPos=(x', y') , bulletVel=(vx', vy') }
    where
        (x, y)    = bulletPos b
        (vx, vy)  = bulletVel b

        x' = x + (vx * s)
        y' = y + (vy * s)

        vx' = vx
        vy' = vy

--Move all live bullets
moveBullets :: Seconds -> [Bullet] -> [Bullet]
moveBullets s bl = map (moveBullet s) bl


-- reset cannon reload and generate a bullet if a hero is in sight
generateBullets :: State -> State
generateBullets state = state { bullets = bullets' , cannons = cannons' }
    where
        currCannons = cannons state
        currHero = hero state
        cannons' = resetReloadTime currCannons (heroPos currHero)
        bullets' = (bullets state) ++ shootBullets currCannons (heroPos currHero)

--Get rid of cannons who health is below 0
updateCannons :: State -> State
updateCannons state = state { cannons = cannons' }
    where
        cannons' = filter (\x -> (cannonHealth x) > 0) (cannons state)

--Update the game state if the hero gets hit with a bullet or don't if he doesnt
updateBullets :: State -> State
updateBullets state
    | bulletHit (heroPos currHero) (inCrouch currHero) (bullets state) = state { hero = currHero { lives = lives' } , bullets = bullets'}
    | otherwise = state
        where
            currBullets = bullets state
            currHero = hero state
            lives'   = (lives currHero) - 1
            bullets' = inRangeBullet (inCrouch currHero) (heroPos currHero) (bullets state)

-- checks if the center of the here is is in the boundaries of the goal, and
-- returns to the start screen is he is TODO have him go to a good job screen
-- with total time (maybe) displayed
checkGoal :: State -> State
checkGoal state
    | start state = state
    | otherwise = state { complete = inGoal , end = inGoal }
    where
        currGoal              = goal state
        (hx, hy)              = heroPos (hero state)
        (gx, gy)              = goalPos currGoal
        (goalWid, goalHeight) = goalSize

        top        = gy + goalHeight / 2
        bottom     = gy - goalHeight / 2
        rightEdge  = gx + goalWid / 2
        leftEdge   = gx - goalWid / 2

        inGoal = hx >= leftEdge  &&  hx <= rightEdge  &&  hy >= bottom  && hy <= top

-- checks to see if hero has zero lives left, and goes to end screen if he does
checkLives :: State -> State
checkLives state
    | end state = state
    | otherwise = state { end = remainingLives == 0 }
        where
            currHero       = hero state
            remainingLives = lives currHero


-- collision and perhaps map scrolling this way as well, and bullets hitting things
update :: Float -> State -> State
update seconds = checkLives . checkGoal . updateBullets . updateCannons . generateBullets . motion seconds


main :: IO ()
main = do
    catR <- loadBMP "src/graphics/cat_r.bmp"
    catL <- loadBMP "src/graphics/cat_l.bmp"
    swipeR <- loadBMP "src/graphics/cat_swipe_r.bmp"
    swipeL <- loadBMP "src/graphics/cat_swipe_l.bmp"
    life <- loadBMP "src/graphics/life_img.bmp"
    platHard <- loadBMP "src/graphics/plat_hard_bot_sym.bmp"
    platSoft <- loadBMP "src/graphics/plat_soft_bot.bmp"
    gate <- loadBMP "src/graphics/gate.bmp"
    ground <- loadBMP "src/graphics/metal_floor.bmp"
    cannonL <- loadBMP "src/graphics/cannon_l.bmp"
    cannonR <- loadBMP "src/graphics/cannon_r.bmp"
    cannonU <- loadBMP "src/graphics/cannon_u.bmp"
    cannonD <- loadBMP "src/graphics/cannon_d.bmp"
    spikeImg <- loadBMP "src/graphics/spikes.bmp"
    bgImg <- loadBMP "src/graphics/m16_lr_crop.bmp"
    healthBarImg <- loadBMP "src/graphics/healthbar.bmp"
    healthImg <- loadBMP "src/graphics/health.bmp"
    failedImg <- loadBMP "src/graphics/failed.bmp"
    successImg <- loadBMP "src/graphics/complete.bmp"
    homeBgImg <- loadBMP "src/graphics/home_bg.bmp"
    titleImg <- loadBMP "src/graphics/title_v1.1.bmp"
    map1Img <- loadBMP "src/graphics/map1_img.bmp"
    map2Img <- loadBMP "src/graphics/map2_img.bmp"
    homeImg <- loadBMP "src/graphics/home_button.bmp"
    retryImg <- loadBMP "src/graphics/retry_button.bmp"
    oneInfo <- loadBMP "src/graphics/map1_label.bmp"
    twoInfo <- loadBMP "src/graphics/map2_label.bmp"


    let cannonImgs = cannonL : cannonR : cannonU : cannonD : healthBarImg : healthImg : []
    let heroImgs   = catR:catL:swipeR:swipeL:[]
    play window bgColor fps ( map1InitState { start = True } ) (render heroImgs (platSoft, platHard) life gate ground cannonImgs spikeImg bgImg failedImg successImg homeBgImg titleImg (map1Img, map2Img) (homeImg, retryImg) (oneInfo, twoInfo)) getInput update
