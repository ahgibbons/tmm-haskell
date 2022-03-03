module Main where

import qualified Data.Matrix as M
import Data.Matrix ((!))
import Data.Complex
import Control.Applicative
import Data.List (foldl')

data SellmeierMat = SellmeierMat Double Double Double

fixmat :: Complex Double -> a -> Complex Double
fixmat n = const n

sellmat :: SellmeierMat -> Double -> Complex Double
sellmat (SellmeierMat a b c) w = sqrt(a + ((b * w^2) / (w^2 - c))) :+ 0

optlayer :: (Double -> Complex Double) -> Double -> Double -> M.Matrix (Complex Double)
optlayer nf t w = M.fromLists [[cos theta, (0 :+ 1) * sin theta / n],
                             [(0 :+ 1) *  sin theta * n, cos theta]]
        where
           n = nf w
           theta = (2 * pi * t / w :+ 0) * n  

tmm :: (RealFloat c, Foldable t, Applicative t) => Complex c -> Complex c -> t (a -> M.Matrix (Complex c)) -> a -> c
tmm nL nR layers = realPart . (<*>) (*) conjugate . r . flip M.multStd bR . foldl' M.multStd bLinv . (<*>) layers . pure
    where
        bLinv = fmap (0.5*) $ M.fromLists [[1,(-1/nL)],[1,1/nL]]
        bR    = M.fromLists [[1,1],[(-1*nR), nR]]
        r m   = m ! (1,2) / m ! (1,1)

main :: IO ()
main = putStrLn "Hello, Haskell!"
