module Hero where

import Graphics.Gloss
import Constants
import Misc
import DataTypes

-- Second argument picture takes a picture of the hero (as a pair now for r, l)
drawHero :: Hero -> [Picture] -> Picture
drawHero currHero heroImg
    | crouched                   = uncurry translate hpos $ scale 1 0.5 $ crouchImgSelection currHero heroImg
    | facingR currHero && swiped currHero     = uncurry translate hpos $ heroImg !! 2
    | facingR currHero     = uncurry translate hpos $ heroImg !! 0
    | swiped currHero      = uncurry translate hpos $ heroImg !! 3
    | otherwise            = uncurry translate hpos $ heroImg !! 1
        where
            (x, y)   = heroPos currHero
            crouched = inCrouch currHero
            grounded = onGround currHero
            hpos     = heroPos currHero

crouchImgSelection :: Hero -> [Picture] -> Picture
crouchImgSelection hero heroImg | facingR hero && swiped hero = trans $heroImg !! 2
                                | facingR hero                = trans $ heroImg !! 0
                                | swiped hero                 = trans $ heroImg !! 3
                                | otherwise                   = trans $ heroImg !! 1
                                    where trans | onGround hero = translate 0 (-halfHeroHeight) -- make him look lower if on ground
                                                | otherwise = translate 0 0
-- Second argument picture takes a picture of the hero (as a pair now for r, l)
-- drawHero :: Hero -> (Picture, Picture) -> Picture
-- drawHero currHero heroImg
--     | crouched                   = uncurry translate hpos $ if facingR currHero then scale 1 0.5 (fst heroImg) else scale 1 0.5 (snd heroImg)
--     | facingR currHero           = uncurry translate hpos $ fst heroImg
--     | otherwise                  = uncurry translate hpos $ snd heroImg
--         where
--             hpos     = heroPos currHero
--             crouched = inCrouch currHero
--             grounded = onGround currHero
