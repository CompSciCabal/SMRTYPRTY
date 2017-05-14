import Control.Exception
import Numeric

data Interval = Interval Float Float deriving (Eq, Show)

instance Num Interval where
  Interval l u + Interval l' u' = Interval (l + l') (u + u')
  Interval l u * Interval l' u' = let a = l * l'
                                      b = l * u'
                                      c = u * l'
                                      d = u * u'
                                      lst = [a, b, c, d]
                                  in Interval (minimum lst) (maximum lst)
  Interval l u - Interval l' u' = Interval (l - u') (u - l')
  fromInteger a = Interval (realToFrac a) (realToFrac a)
  abs = id
  signum _ = 1

instance Fractional Interval where
  a / Interval l u = assert (not $ and[l <= 0, u >= 0]) a * Interval (1.0 / u) (1.0 / l)
  fromRational a = Interval r r
    where r = fromRat a

intervalWidth :: Interval -> Float
intervalWidth (Interval l u) = (u - l) / 2.0

makeCenterWidth :: Float -> Float -> Interval
makeCenterWidth center width = Interval (center - width) (center + width)

makeCenterPercent :: Float -> Float -> Interval
makeCenterPercent center percent = 
  makeCenterWidth center $ center * (percent / 100)