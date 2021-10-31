module Engine.Location where

import qualified Linear

data Location = Location
    {-# UNPACK #-} !Float
    {-# UNPACK #-} !Float
    {-# UNPACK #-} !Float
    {-# UNPACK #-} !Float
    {-# UNPACK #-} !Float
    {-# UNPACK #-} !Float

instance Semigroup Location where
    Location axx axy axw ayx ayy ayw <> Location bxx bxy bxw byx byy byw =
        Location
            (axx * bxx + axy * byx)
            (axx * bxy + axy * byy)
            (axx * bxw + axy * byw + axw)
            (ayx * bxx + ayy * byx)
            (ayx * bxy + ayy * byy)
            (ayx * bxw + ayy * byw + ayw)

instance Monoid Location where
    mempty = Location 1 0 0 0 1 0

inverseLocation :: Location -> Location
inverseLocation (Location xx xy xw yx yy yw) = do
    let det = xx * yy - xy * yx
    let invDet = 1 / det
    Location
        (yy * invDet)
        (- xy * invDet)
        ((xy * yw - yy * xw) * invDet)
        (- yx * invDet)
        (xx * invDet)
        ((yx * xw - xx * yw) * invDet)

locIdentity :: Location
locIdentity = mempty

locTranslate :: Linear.V2 Float -> Location
locTranslate (Linear.V2 x y) = Location 1 0 x 0 1 y

locScale :: Float -> Location
locScale k = Location k 0 0 0 k 0

locRotate :: Float -> Location
locRotate rad = Location a (-b) 0 b a 0
  where
    a = cos rad
    b = sin rad
