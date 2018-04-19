module Quaternion(
  Quaternion, conjQ, realQ, imagQ, normQ, divQR, invQ,
  fromListQ, fromRealImagQ, fromAngleAxisQ, rotQ
) where

data Quaternion a = Quaternion a a a a deriving (Eq)

instance (Num a) => Num (Quaternion a) where
  Quaternion r1 i1 j1 k1 + Quaternion r2 i2 j2 k2 = Quaternion (r1+r2) (i1+i2) (j1+j2) (k1+k2)
  Quaternion r1 i1 j1 k1 - Quaternion r2 i2 j2 k2 = Quaternion (r1-r2) (i1-i2) (j1-j2) (k1-k2)
  Quaternion r1 i1 j1 k1 * Quaternion r2 i2 j2 k2 = Quaternion r3 i3 j3 k3
    where r3 = r1*r2 - i1*i2 - j1*j2 - k1*k2
          i3 = r1*i2 + i1*r2 + j1*k2 - k1*j2
          j3 = r1*j2 - i1*k2 + j1*r2 + k1*i2
          k3 = r1*k2 + i1*j2 - j1*i2 + k1*r2
  abs (Quaternion r i j k) = Quaternion (abs r) (abs i) (abs j) (abs k)
  signum (Quaternion r i j k) = Quaternion (signum r) (signum i) (signum j) (signum k)
    where qmod = realQ $ abs (Quaternion r i j k)
  fromInteger a = Quaternion (fromInteger a) 0 0 0
  negate (Quaternion r i j k) = Quaternion (-r) (-i) (-j) (-k)

instance (Fractional a) => Fractional (Quaternion a) where
  fromRational a = Quaternion (fromRational a) 0 0 0
  recip (Quaternion r i j k) = divQR (Quaternion r (-i) (-j) (-k)) (r^2 + i^2 + j^2 + k^2)
  q1 / q2 = q1 * recip q2

instance (Show a) => Show (Quaternion a) where
  showsPrec p (Quaternion r i j k) = showParen (p > 9) (showString quatString)
    where quatString = "{" ++ (show r) ++ ", " ++ (show i) ++ "i, "
                          ++ (show j) ++ "j, " ++ (show k) ++ "k}"


conjQ :: (Num a) => Quaternion a -> Quaternion a
conjQ (Quaternion r i j k) = Quaternion r (-i) (-j) (-k)

realQ :: Quaternion a -> a
realQ (Quaternion r _ _ _) = r

imagQ :: Quaternion a -> (a, a, a)
imagQ (Quaternion _ i j k) = (i, j, k)

normQ :: (Floating a) => Quaternion a -> a
normQ (Quaternion r i j k) = sqrt (r^2 + i^2 + j^2 + k^2)

divQR :: (Fractional a) => Quaternion a -> a -> Quaternion a
divQR (Quaternion r i j k) d = Quaternion (r/d) (i/d) (j/d) (k/d)

mulQR :: (Num a) => Quaternion a -> a -> Quaternion a
mulQR (Quaternion r i j k) d = Quaternion (r*d) (i*d) (j*d) (k*d)

invQ :: (Fractional a) => Quaternion a -> Quaternion a
invQ = recip

rotQ :: (Num a) => Quaternion a -> Quaternion a -> Quaternion a
rotQ q1 q2 = q1 * q2 * conjQ q1

unitQ :: (Floating a) => Quaternion a -> Quaternion a
unitQ q = divQR q $ normQ q

-- Builders

fromListQ :: (Num a) => [a] -> Quaternion a
fromListQ [i, j, k] = Quaternion 0 i j k
fromListQ [r, i, j, k] = Quaternion r i j k
fromListQ _ = error "fromListQ: List with wrong number of elements"

fromRealImagQ :: a -> (a, a, a) -> Quaternion a
fromRealImagQ r (i,j,k) = Quaternion r i j k

fromAngleAxisQ :: (Floating a) => a -> (a, a, a) -> Quaternion a
fromAngleAxisQ theta (i,j,k) = Quaternion (cos a) (sin a * i/absV)
                                 (sin a * j/absV) (sin a * k/absV)
  where a = theta/2
        absV = sqrt (i^2 + j^2 + k^2)
