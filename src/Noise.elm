module Noise (PermutationTable, permutationTable, noise3d) where

{-| This is a library to generate simplex noise in Elm.

The code is a port of the [simplex noise JavaScript version](https://github.com/jwagner/simplex-noise.js) by Jonas Wagner.

## Example usage

    (perm, newSeed) = permutationTable (initialSeed 42) -- generate the permutation table
    noiseValue = noise3d perm 1 1 1

# Permutation tables
@docs permutationTable, PermutationTable

# Genarate noise
@docs noise3d

-}
import Array exposing (Array)
import Random.Array exposing (shuffle)
import Random
import Bitwise exposing (and)

f2 : Float
f2 = 0.5 * ((sqrt 3) - 1)

g2 : Float
g2 = (3 - sqrt 3) / 6

f3 : Float
f3 = 1 / 3

g3 : Float
g3 = 1 / 6

f4 : Float
f4 = ((sqrt 5) - 1) / 4

g4 : Float
g4 = (5 - (sqrt 5)) / 20

get : Array a -> Int -> a
get  arr i =
  case (Array.get i arr) of
    Just x -> x
    Nothing -> Debug.crash "Error getting item"


reverseArray : Array a -> Array a
reverseArray array =
  Array.toList array |> List.reverse |> Array.fromList

{-|
Permutation table that is needed to generate the noise value.
-}
type alias PermutationTable = {perm: Array Int, permMod12: Array Int}


{-|
Genrate the permutation tables that are needed to calculate the noise value.
The function takes a seed and returns the table and  a new seed.
-}
permutationTable : Random.Seed -> (PermutationTable, Random.Seed)
permutationTable seed =
  let (perm, seed') =
    [0..255]
    |> Array.fromList
    |> Random.Array.shuffle seed
    |> \ (list, seed) ->(Array.append list  (reverseArray list), seed)
  in
    ({perm= perm, permMod12 = generatePermMod12 perm}, seed)


generatePermMod12 : Array Int -> Array Int
generatePermMod12 perm =
      Array.map (\i -> i % 12) perm


grad3 : Array Float
grad3 =
  Array.fromList [
  1, 1, 0, -1, 1, 0, 1, -1, 0,
  -1, -1, 0, 1, 0, 1, -1, 0, 1,
  1, 0, -1, -1, 0, -1, 0, 1, 1,
  0, -1, 1, 0, 1, -1, 0, -1, -1]


getCornerOffset : Float -> Float -> Float -> (Int, Int, Int, Int, Int, Int)
getCornerOffset x y z =
  if (x >= y) then
    if (y >= z) then
      (1, 0, 0, 1, 1, 0)
    else if (x >= z) then
      (1, 0, 0, 1, 0, 1)
    else
      (0, 0, 1, 1, 0, 1)
  else
    if (y < z) then
      (0, 0, 1, 0, 1, 1)
    else if (x < z) then
      (0, 1, 0, 0, 1, 1)
    else
      (0, 1, 0, 1, 1, 0)


getN : Float -> Float -> Float -> Int -> Int -> Int -> Array Int -> Array Int -> Float
getN x y z i j k  perm permMod12 =
  let
    t = 0.6 - x * x - y * y - z * z
  in
    if (t < 0) then
      0
    else
      let
        gi = (get permMod12 (i + get perm (j + get perm k))) * 3
        t'= t * t
      in
        t' * t' * ((get grad3 gi) * x + (get grad3 (gi + 1)) * y + (get grad3 (gi + 2)) * z)


{-|
Generates a noise value between `-1` and `1` based on the given x, y and z value and a seeded permutation table.
Using the same permutation table will always return the same result for the same coordinate.
-}
noise3d : PermutationTable -> Float -> Float -> Float -> Float
noise3d {perm, permMod12} xin yin zin =
  let
    s = (xin + yin + zin) * f3 --Very nice and simple skew factor for 3D
    i = floor (xin + s)
    j = floor (yin + s)
    k = floor (zin + s)
    t =  toFloat (i + j + k) * g3
    x0' = (toFloat i) - t  --Unskew the cell origin back to (x,y,z) space
    y0' = (toFloat j) - t
    z0' = (toFloat k) - t
    x0 = xin - x0'  --The x,y,z distances from the cell origin
    y0 = yin - y0'
    z0 = zin - z0'
    (i1, j1, k1, i2, j2,  k2) = getCornerOffset x0 y0 z0
    x1 = x0 - (toFloat i1) + g3 --Offsets for second corner in (x,y,z) coords
    y1 = y0 - (toFloat j1) + g3
    z1 = z0 - (toFloat k1) + g3
    x2 = x0 - (toFloat i2) + 2 * g3 --Offsets for third corner in (x,y,z) coords
    y2 = y0 - (toFloat j2) + 2 * g3
    z2 = z0 - (toFloat k2) + 2 * g3
    x3 = x0 - 1 + 3 * g3 --Offsets for last corner in (x,y,z) coords
    y3 = y0 - 1 + 3 * g3
    z3 = z0 - 1 + 3 * g3
    --Work out the hashed gradient indices of the four simplex corners
    ii = and i 255
    jj = and j 255
    kk = and k 255
    n0 = getN x0 y0 z0 ii jj kk perm permMod12
    n1 = getN x1 y1 z1 (ii + i1) (jj + j1) (kk + k1) perm permMod12
    n2 = getN x2 y2 z2 (ii + i2) (jj + j2) (kk + k2)  perm permMod12
    n3 = getN x3 y3 z3 (ii + 1) (jj + 1) (kk + 1)  perm permMod12
  in
   32 * (n0 + n1 + n2 + n3)
