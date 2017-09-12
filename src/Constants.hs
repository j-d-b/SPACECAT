module Constants where

--Type aliases
type Seconds = Float
type Position = (Float, Float)      -- (x, y)
type Velocity = (Float, Float)
type PlatformSize = (Float, Float)  -- x dimension and y dimension

--- Constants of the game world ---
gravity :: Float
gravity = -8

jumpVel :: Float
jumpVel = 300

stdFriction :: Float
stdFriction = 7

crouchedFricModifier :: Float -- how much more friction when crouched
crouchedFricModifier = 1.5

crouchedAccelModifier :: Float -- how much slower side accel when crouched
crouchedAccelModifier = 0.5

sideVel :: Float -- velocity to give when side arrow pressed
sideVel = 10

minJumpVel :: Float
minJumpVel = 170

maxSideVel :: Float
maxSideVel = 100

sideAccel :: Float
sideAccel = 5

maxCrouchedSideVel :: Float
maxCrouchedSideVel = 20


--- Properties of the hero ---
heroImgSize :: (Float, Float) -- dimensions of the hero bmp
heroImgSize = (44, 22)

heroHeight :: Float
heroHeight = snd heroImgSize

halfHeroHeight :: Float
halfHeroHeight = heroHeight / 2

heroWidth :: Float
heroWidth = fst heroImgSize

halfHeroWidth :: Float
halfHeroWidth = heroWidth / 2

attackRange :: (Float, Float) --How far is the paw reach?
attackRange = (40, 40)

attackDamage :: Float
attackDamage = 10

heroSpace :: (Float, Float) -- how muc room does a hero take up?
heroSpace = (30, 20)

stdHeroLives :: Float
stdHeroLives = 5


--- Properties of the physical objects ---
stdPlatformThickness :: Float -- platform BMP thickness
stdPlatformThickness = 8

stdPlatformWidth :: Float -- platform BMP width
stdPlatformWidth = 40

earthLineAbsPos :: Float -- make this connect to the code for drawing earth
earthLineAbsPos = -100

healthBarLength :: Float --length of health bars
healthBarLength = 40

healthBarWidth :: Float --width of health bars
healthBarWidth = 5

barAboveHeight :: Float --how high do the health bars float above our cannons heads
barAboveHeight = 15.5

normalCannonHealth :: Float --Health of a cannon
normalCannonHealth = 30

lifeImgSize :: (Float, Float) --Size of the lives to display
lifeImgSize = (10, 10)

lifeImgSpacing :: Float -- Space in between the lifes to Display
lifeImgSpacing = 20

leftLifePos :: Position --First place (most left) to spawn a life icon
leftLifePos = (-380, 180)

stdBulletSize :: (Float, Float) --std buller size
stdBulletSize = (3,3)

--Standard speed of bullet. Dr. Efron would probably chastisize Jacob and I for
--conflating velocity with speed.
stdBulletVel :: Float
stdBulletVel = 150

stdCannonSize :: (Float, Float) --std cannon size
stdCannonSize = (26, 26)

halfCannonWidth :: Float
halfCannonWidth = 26

halfCannonHeight :: Float
halfCannonHeight = 26

stdReload :: Float --standard reload time for a cannon
stdReload = 1

goalSize :: (Float, Float)
goalSize = (24, 36)


--- Misc ---
fps :: Int
fps = 60

initUpThresh :: Float
initUpThresh = 90

initLowThresh :: Float
initLowThresh = -190

usableRange :: Float
usableRange = initUpThresh - initLowThresh

defaultWall :: (Float, Float)
defaultWall = (-400,400)
-- for testing

thresholdTop :: Float
thresholdTop = 90

thresholdBottom :: Float
thresholdBottom = -90
