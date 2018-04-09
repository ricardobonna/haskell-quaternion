module Quaternion(
  Quaternion, conjQ, realQ, imagQ, normQ, divQR, invQ
) where

data Quaternion = Quaternion Float Float Float Float deriving (Eq)

instance Num Quaternion where
  Quaternion r1 i1 j1 k1 + Quaternion r2 i2 j2 k2 = Quaternion (r1+r2) (i1+i2) (j1+j2) (k1+k2)
  Quaternion r1 i1 j1 k1 - Quaternion r2 i2 j2 k2 = Quaternion (r1-r2) (i1-i2) (j1-j2) (k1-k2)
  Quaternion r1 i1 j1 k1 * Quaternion r2 i2 j2 k2 = Quaternion r3 i3 j3 k3
    where r3 = r1*r2 - i1*i2 - j1*j2 - k1*k2
          i3 = r1*i2 + i1*r2 + j1*k2 - k1*j2
          j3 = r1*j2 - i1*k2 + j1*r2 + k1*i2
          k3 = r1*k2 + i1*j2 - j1*i2 + k1*r2
  abs (Quaternion r i j k) = Quaternion (sqrt (r^2 + i^2 + j^2 + k^2)) 0 0 0
  signum (Quaternion r i j k) = Quaternion (r/qmod) (i/qmod) (j/qmod) (k/qmod)
    where qmod = realQ $ abs (Quaternion r i j k)
  fromInteger a = Quaternion (fromInteger a :: Float) 0 0 0
  negate (Quaternion r i j k) = Quaternion (-r) (-i) (-j) (-k)

instance Fractional Quaternion where
  fromRational a = Quaternion (fromRational a :: Float) 0 0 0
  recip q = divQR (conjQ q) ((normQ q)^2)
  q1 / q2 = q1 * (recip q2)

instance Show Quaternion where
  showsPrec p (Quaternion r i j k) = showParen (p > 9) (showString quatString)
    where quatString = "{" ++ (show r) ++ ", " ++ (show i) ++ "i, "
                          ++ (show j) ++ "j, " ++ (show k) ++ "k}"


conjQ :: Quaternion -> Quaternion
conjQ (Quaternion r i j k) = Quaternion r (-i) (-j) (-k)

realQ :: Quaternion -> Float
realQ (Quaternion r _ _ _) = r

imagQ :: Quaternion -> (Float, Float, Float)
imagQ (Quaternion _ i j k) = (i, j, k)

normQ :: Quaternion -> Float
normQ q = realQ $ abs q

divQR :: Quaternion -> Float -> Quaternion
divQR (Quaternion r i j k) d = Quaternion (r/d) (i/d) (j/d) (k/d)

invQ :: Quaternion -> Quaternion
invQ = recip

fromListQ :: [Float] -> Quaternion
fromListQ [i, j, k] = Quaternion 0 i j k
fromListQ [r, i, j, k] = Quaternion r i j k
fromListQ _ = error "fromListQ: List with wrong number of elements"

fromAngleAxisQ :: Float -> (Float, Float, Float) -> Quaternion
fromAngleAxisQ theta (i,j,k) = Quaternion (cos $ theta/2) (sin a * i/absV)
                                 (sin a * j/absV) (sin a * k/absV)
  where a = theta/2
        absV = sqrt (i^2 + j^2 + k^2)

rotQ :: Quaternion -> Quaternion -> Quaternion
rotQ q1 q2 = q1 * q2 * (conjQ q1)
