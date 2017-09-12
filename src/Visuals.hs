module Visuals where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Constants
import Misc
import DataTypes


--- Drawing Functions ---
atmosphere :: Picture -- currently not used
atmosphere = color (greyN 0.2) $ rectangleSolid 800 400

-- second parameter is BMP for goal. have to match to goalSize
drawGoal :: Goal -> Picture -> Picture
drawGoal g goalBMP = uncurry translate (goalPos g) $ goalBMP

-- converts a list of Platform into a list of pictures, takes in a tuple
-- currently of (softbottombmp, hardbottombmp) to use for the platform images
platformGenerator :: [Platform] -> (Picture, Picture) -> [Picture]
platformGenerator ps bmpTuple = map (platToPic bmpTuple) ps

platToPic :: (Picture, Picture) -> Platform -> Picture
platToPic (softBmp, hardBmp) p = uncurry translate (ppos p) $ platPic
    where
        (width, len)  = psize p
        scaleFactorW = width / stdPlatformWidth
        scaleFactorL = len / stdPlatformThickness
        platPic
            | (ptype p) == HardBottom = scale scaleFactorW scaleFactorL $ hardBmp
            | (ptype p) == SoftBottom = scale scaleFactorW scaleFactorL $ softBmp
            | (ptype p) == Earth      = softBmp -- can't scale
            | (ptype p) == Spike      = softBmp -- can't scale
            | otherwise               = color (pcol p) $ uncurry rectangleSolid (psize p)


-- picture is BMP for cannon
drawCannons :: [Picture] -> [Cannon] -> [Picture]
drawCannons cannonImgs cs = map (showCannon cannonImgs) cs

-- combines the cannon itself, health bar, and remaining health level into
-- a single picture. First picture argument is BMP image
showCannon :: [Picture] -> Cannon -> Picture
showCannon cannonImgs c = pictures (cannonBody : bar : [health])
    where
        (cx, cy)     = cannonPos c
        direction    = (angle c)
        healthBarImg = cannonImgs !! 4
        healthImg    = cannonImgs  !! 5
        cannonBody
            | direction == West   = uncurry translate (cx, cy) $ cannonImgs !! 0
            | direction == East   = uncurry translate (cx, cy) $ cannonImgs !! 1
            | direction == North  = uncurry translate (cx, cy) $ cannonImgs !! 2
            | otherwise           = uncurry translate (cx, cy) $ cannonImgs !! 3
        bar    = uncurry translate (cx, cy + barAboveHeight) $ healthBarImg
        health = uncurry translate (partialBar (percentHealth c) (cx, cy) c) $ scale (percentHealth c) 1 $ healthImg

-- this function has a strange name and probably shouldn't exist
partialBar :: Float -> Position -> Cannon -> Position
partialBar health (x, y) c = (x - 0.5 * ((cannonMaxHealth c) - (health * (cannonMaxHealth c) ) ) , y + barAboveHeight)

--go through the list of cannons
decreaseTime :: Seconds -> [Cannon] -> [Cannon]
decreaseTime s cl = map (reloadCannon s) cl

--get the cannons back reloaded
reloadCannon :: Seconds -> Cannon -> Cannon
reloadCannon s c = c {reloadTime = x'}
    where
        waitTime = reloadTime c
        x' | waitTime <= 0 = 0
           | otherwise     = waitTime - s

resetReloadTime :: [Cannon] -> Position -> [Cannon]
resetReloadTime cl p = map (justFired p) cl

--Given hero posit
damageCannon :: Hero -> Cannon -> Cannon
damageCannon h c | inRangePlus attackRange h (cannonPos c) = c {cannonHealth = cannonHealth', percentHealth = percentHealth'}
                 | otherwise   = c
        where
            cannonHealth' = (cannonHealth c) - attackDamage
            percentHealth' = cannonHealth' / (cannonMaxHealth c)


--If a cannon just fired, reset reload time
justFired ::  Position -> Cannon -> Cannon
justFired hpos c
    | onSight c hpos && (reloadTime c <= 0) = c {reloadTime = stdReload}
    | otherwise                             = c


-- using number of lives remaining, generates locations of the life images for
-- display. Uses concept of stdHeroLives. I don't know how I see lives fitting
-- into this game. Id prefer to have no lives, but if we are going to use an
-- idea of lives, makes sense to rewrite this if each level can have different
-- amount of lives.
genLifePositions :: Float -> [Position]
genLifePositions 0     = []
genLifePositions lives = (leftLifeX + lifeImgSpacing * (stdHeroLives - lives), leftLifeY) : genLifePositions (lives - 1)
    where
        leftLifeX = fst leftLifePos
        leftLifeY = snd leftLifePos

-- takes int of number of lives and the life bmp img and draws all lives
drawLives :: Float -> Picture -> [Picture]
drawLives 0 _           = []
drawLives lives lifeImg = map (\x -> uncurry translate x $ lifeImg) (genLifePositions lives)


drawBullets :: [Bullet] -> [Picture]
drawBullets bl = map (\b -> uncurry translate (bulletPos b) $ color blue $ uncurry rectangleSolid stdBulletSize) bl -- replace w/ bmp


--Looks and cannon direction and sees if a hero is in his line of sight.
onSight :: Cannon -> Position -> Bool
onSight c (hx, hy) | (angle c) == West  = hx <= cx && (not ((hy + (b/2)) < cy || (hy - (b/2)) > cy ))
                   | (angle c) == East = hx >= cx && (not ((hy + (b/2)) < cy || (hy - (b/2)) > cy ))
                   | (angle c) == North    = hy >= cy && (not ((hx + (b/2)) < cx || (hx - (b/2)) > cx))
                   | otherwise            = hy <= cy && (not ((hx + (b/2)) < cx || (hx - (b/2)) > cx))
    where
        (cx, cy) = cannonPos c
        (a, b)   = csize c

-- Bullets go in the same direction of the cannon
newBullet :: Cannon -> Bullet
newBullet c = Bullet { bulletPos = bulletPos', bulletVel = bulletVel'}
    where
        bulletPos' = bulletSpawnPoint (angle c) (cannonPos c)
        bulletVel' = bulletDirection (angle c)

bulletDirection :: Direction -> (Float, Float)
bulletDirection s | s == West  = (-stdBulletVel, 0)
                  | s == East = (stdBulletVel, 0)
                  | s == North   = (0, stdBulletVel)
                  | otherwise    = (0, -stdBulletVel)

bulletSpawnPoint :: Direction -> Position -> Position
bulletSpawnPoint s (x,y) | s == West  = (x - halfCannonWidth, y)
                         | s == East = (x + halfCannonWidth, y)
                         | s == North    = (x, y + halfCannonWidth)
                         | otherwise    = (x, y - halfCannonWidth)

--Filter should be able to implmenet this funciton, but not working ATM. Look
--back later and fix it up.
inRangeBullet :: Bool -> Position -> [Bullet] -> [Bullet]
inRangeBullet  _ _ [] = []
inRangeBullet crouched hpos (b:bl)
    | inRange hitBox hpos (bulletPos b)        = inRangeBullet crouched hpos bl
    | otherwise                                = b : inRangeBullet crouched hpos bl
        where
            hitBox | crouched  = (halfHeroWidth, halfHeroHeight / 2)
                   | otherwise = (halfHeroWidth, halfHeroHeight)


-- takes hero position, crouched boolean, list of current bullets,
-- and returns true if the bullet has hit the hero
-- perhaps generalize to allow bullets to hit other things in the future, if
-- hero ends up getting a gun at any time
bulletHit :: Position -> Bool -> [Bullet] -> Bool
bulletHit _ _ []               = False
bulletHit hpos crouched (b:bl) =
    inRange hitBox hpos (bulletPos b) || bulletHit hpos crouched bl
        where
            hitBox | crouched  = (halfHeroWidth, halfHeroHeight / 2)
                   | otherwise = (halfHeroWidth, halfHeroHeight)



-- generalize to interact/attack any object
heroAttack :: Hero -> [Cannon] -> [Cannon]
heroAttack h cl = map (damageCannon h) cl

-- Are two items within the range of eachother (checks x and y)
inRange :: (Float, Float) -> Position -> Position -> Bool
inRange (xrange,yrange) (x,y) (a,b) = (not ((x + xrange) < a || (x - xrange) > a)) && (not ((y + yrange) < b || (y - yrange) > b ))

--Modified version of inRange, which allows a check to make sure the hero faces
--the cannon to kill it rather than just being in the range
facingCannon :: Hero -> Position -> Bool
facingCannon h c | (facingR h) && (compareX c hPos)     = True
                 | not (facingR h) && (compareX hPos c) = True
                 | otherwise                            = False
    where
        hPos = heroPos h



--Better version of inRange.
--Takes the range, hero, and position and tells whether the hero is in range of
--the cannon
inRangePlus :: (Float, Float) -> Hero -> Position -> Bool
inRangePlus ar h z = inRange ar (heroPos h) z && facingCannon h z

compareX :: Position -> Position -> Bool
compareX (a,_) (x,_) = a >= x

--This can definitely be expedited to a more general application
shootBullets :: [Cannon] -> Position -> [Bullet]
shootBullets [] p                             = []
shootBullets (c:cl) p
    | onSight c p && (reloadTime c <= 0) = newBullet c : shootBullets cl p
    | otherwise                          = shootBullets cl p


heroInRange :: Float -> Position -> Position -> Bool
heroInRange range (x1, y1) (x2, y2) = (x1 + range) >= x2
