-----------------------------------------------------------------------------
-- |
-- Module  :  Quaternion
-- Copyright   :  (c) Ricardo Bonna
-- License     :  MIT
--
-- Maintainer  :  ricardobonna@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
--
-----------------------------------------------------------------------------

module Quaternion(
  -- * Quaternion Type
  Quaternion,
  -- * General Quaternion functions
  conjQ, realQ, imagQ, normQ, divQR, mulQR, invQ, rotQ, rotVQ, unitQ, expQ,
  quat2list, quat2euler,
  -- * Builders
  fromListQ, fromRealImagQ, fromAngleAxisQ, from3tupleQ, euler2quat
) where


--------------------
-- Quaternion type
--------------------

-- | Quaternion definition
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


----------------------------------
-- General Quaternion functions
----------------------------------

-- | Quaternion conjugate function. Calculates \(\bar{q}\) of a Quaternion \(q\)
conjQ :: (Num a) => Quaternion a -> Quaternion a
conjQ (Quaternion r i j k) = Quaternion r (-i) (-j) (-k)

-- | Get the real part of a Quaternion
realQ :: Quaternion a -> a
realQ (Quaternion r _ _ _) = r

-- | Get imaginary part of a Quaternion as a 3-tuple
imagQ :: Quaternion a -> (a, a, a)
imagQ (Quaternion _ i j k) = (i, j, k)

-- | Calculates the norm \(\|q\|\) of a Quaternion \(q\)
normQ :: (Floating a) => Quaternion a -> a
normQ (Quaternion r i j k) = sqrt (r^2 + i^2 + j^2 + k^2)

-- | Quaternion scalar division
divQR :: (Fractional a) => Quaternion a -> a -> Quaternion a
divQR (Quaternion r i j k) d = Quaternion (r/d) (i/d) (j/d) (k/d)

-- | Quaternion scalar multiplication
mulQR :: (Num a) => Quaternion a -> a -> Quaternion a
mulQR (Quaternion r i j k) d = Quaternion (r*d) (i*d) (j*d) (k*d)

-- | Calculates the inverse Quaternion of a Quaternion
invQ :: (Fractional a) => Quaternion a -> Quaternion a
invQ = recip

-- | Rotate a Quaternion q2 using a rotation quaternion q1
rotQ :: (Num a) => Quaternion a -> Quaternion a -> Quaternion a
rotQ q1 q2 = q1 * q2 * conjQ q1

-- | Rotate a vector v represented by a 3-tuple using a rotation Quaternion q
rotVQ :: (Num a) => (a, a, a) -> Quaternion a -> Quaternion a
rotVQ v q = rotQ q (from3tupleQ v)

-- | Normalize a Quaternion
unitQ :: (Floating a) => Quaternion a -> Quaternion a
unitQ q = divQR q $ normQ q

-- | Quaternion exponential function \(\exp(q)\)
expQ :: (Floating a) => Quaternion a -> Quaternion a
expQ (Quaternion r i j k) = Quaternion yr yi yj yk
  where v = sqrt (i^2 + j^2 + k^2)
        yr = exp r * cos v
        yi = exp r * (i/v) * sin v
        yj = exp r * (j/v) * sin v
        yk = exp r * (k/v) * sin v

-- | Gives a list from the elements of a Quaternion
quat2list :: Quaternion a -> [a]
quat2list (Quaternion r i j k) = [r, i, j, k]

-- | Converts a Quaternion into a ZYX rotation Euler angles
-- \((\phi, \theta, \psi)\)
quat2euler :: (RealFloat a) => Quaternion a -> (a, a, a)
quat2euler (Quaternion r i j k) = (phi, theta, psi)
  where phi = atan2 (2*(r*i+j*k)) (1-2*(i^2+j^2))
        theta = asin (2*(r*j-k*i))
        psi = atan2 (2*(r*k+i*j)) (1-2*(j^2+k^2))


---------------
-- Builders
---------------

-- | Generates a Quaternion from a list of either 3 or 4 numerical elements
fromListQ :: (Num a) => [a] -> Quaternion a
fromListQ [i, j, k] = Quaternion 0 i j k
fromListQ [r, i, j, k] = Quaternion r i j k
fromListQ _ = error "fromListQ: List with wrong number of elements"

-- | Generates a Quaternion from real and imaginary parts
fromRealImagQ :: a -> (a, a, a) -> Quaternion a
fromRealImagQ r (i,j,k) = Quaternion r i j k

-- | Generates a Quaternion from a rotation angle and a rotation axis
fromAngleAxisQ :: (Floating a) => a -> (a, a, a) -> Quaternion a
fromAngleAxisQ theta (i,j,k) = Quaternion (cos a) (sin a * i/absV)
                                 (sin a * j/absV) (sin a * k/absV)
  where a = theta/2
        absV = sqrt (i^2 + j^2 + k^2)

-- | Generates a Quaternion with zero real part from a 3-tuple (i, j, k)
from3tupleQ :: (Num a) => (a, a, a) -> Quaternion a
from3tupleQ (i, j, k) = Quaternion 0 i j k

-- | Converts the ZYX rotation Euler angles \((\phi, \theta, \psi)\) into a Quaternion
euler2quat :: (Floating a) => (a, a, a) -> Quaternion a
euler2quat (phi, theta, psi) = Quaternion r i j k
  where r = cos (phi/2) * cos (theta/2) * cos (psi/2) + sin (phi/2) * sin (theta/2) * sin (psi/2)
        i = sin (phi/2) * cos (theta/2) * cos (psi/2) - cos (phi/2) * sin (theta/2) * sin (psi/2)
        j = cos (phi/2) * sin (theta/2) * cos (psi/2) + sin (phi/2) * cos (theta/2) * sin (psi/2)
        k = cos (phi/2) * cos (theta/2) * sin (psi/2) - sin (phi/2) * sin (theta/2) * cos (psi/2)
