module P15 () where
import Data.Ratio ( (%), numerator )
latticePaths side = grids (side,side)
    where
        grids (0,_) = 1
        grids (_,0) = 1
        grids (x,y) = grids(x,y-1) + grids (x-1,y)

latticePaths2 side = dp !!side !! side
  where
    dp = [[lattice x y | y <- [0..side]] | x <- [0..side]]
    lattice 0 _ = 1
    lattice _ 0 = 1
    lattice x y = dp !! (x - 1) !! y + dp !! x !! (y - 1)

latticePaths3 :: Integral a => a -> a
latticePaths3 side = numerator $ product [(side+i) % i|i<-[1..side]]

main = print $ latticePaths3 20