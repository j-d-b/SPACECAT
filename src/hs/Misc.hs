module Misc where

--takes a float and boolean and returns it as a float if True o/w returns 0
takeIf :: Float -> Bool -> Float
takeIf x b  | b         = x
            | otherwise = 0

-- takes two floats and a boolean, returns the first if True, o/w the second
takeIfElse :: Float -> Float -> Bool -> Float
takeIfElse x y b  | b         = x
                  | otherwise = y

tag :: Maybe a -> a
tag (Just x) = x
--
-- instance Monoid (Maybe Float)
-- mempty  = Nothing
-- mappend = (+)
