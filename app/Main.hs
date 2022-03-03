module Main where

import qualified Data.Matrix as M
import Data.Matrix ((!))
import Data.Complex
import Control.Applicative

data SellmeierMat = SellmeierMat Double Double Double

-- SimpleLayer n thickness
data SimpleLayer = SimpleLayer (Complex Double) Double deriving (Show)
-- SellmeierLayer a b c thickness
data SellmeierLayer = SellmeierLayer Double Double Double Double  deriving (Show)

class Layer a where
    ri :: a -> Double -> Complex Double
    thickness  :: a -> Double

instance Layer SimpleLayer where
    ri (SimpleLayer n _) _ = n
    thickness (SimpleLayer _ t) = t

instance Layer SellmeierLayer where
    ri (SellmeierLayer a b c _) w = sqrt(a + ((b * w^2) / (w^2 - c))) :+ 0
    thickness (SellmeierLayer _ _ _ t) = t
   
tm :: Layer a => Double -> a -> M.Matrix (Complex Double)
tm w layer = M.fromLists [[cos theta, ((0 :+ 1)*sin theta) / n],
                         [((0 :+ 1) * sin theta) * n, cos theta]]
  where
      theta = k * (thickness layer :+ 0)
      k     = 2 * pi * n / (w :+ 0)
      n     = ri layer w

simplelayer_tm :: Double -> SimpleLayer -> M.Matrix (Complex Double)
simplelayer_tm w (SimpleLayer n t) = M.fromLists [[cos theta, ((0 :+ 1)*sin theta) / n],
                                      [((0 :+ 1) * sin theta) * n, cos theta]]
                    where
                        theta = k * (t :+ 0)
                        k     = 2 * pi * n / (w :+ 0)

sellmeier_tm :: Double -> SellmeierLayer -> M.Matrix (Complex Double)
sellmeier_tm w (SellmeierLayer a b c t) = M.fromLists [[cos theta :+ 0, 0 :+ sin theta / n],
                                      [0 :+ sin theta * n, cos theta :+ 0]]
                    where
                        theta = k * t
                        k     = 2 * pi * n / w
                        n     = sellmeiereqn a b c w



sellmeierRI :: SellmeierMat -> Double -> Double
sellmeierRI (SellmeierMat a b c) w = sqrt(a + ((b * w^2) / (w^2 - c)))

sellmeiereqn :: Double -> Double -> Double -> Double -> Double
sellmeiereqn a b c w = sqrt(a + ((b * w^2) / (w^2 - c)))

--ri2sell:  Convert a refractive index into Sellmeier parameters
--          that return a constant refractive index value
ri2sell :: Double -> SellmeierMat
ri2sell n = SellmeierMat 1 (n*n-1) 0

ps_sell :: SellmeierMat
ps_sell = SellmeierMat 1 1.4435 20216

si_sell = SellmeierMat 1 10.668 90912 

sell2layer :: SellmeierMat -> Double -> SellmeierLayer
sell2layer (SellmeierMat a b c) t = SellmeierLayer a b c t

-- tmm layers nL nR w = r
tmm_simple :: [SimpleLayer] -> Double -> Double -> Double -> Double
tmm_simple layers nL nR w = realPart $ r * conjugate r
  where
    bLinv = fmap (0.5*) $ M.fromLists [[1:+0,(-1/nL):+0],[1:+0,1/nL:+0]]
    bR    = M.fromLists [[1:+0,1:+0],[(-1*nR):+0, nR:+0]]
    tms   = map (simplelayer_tm w) layers
    m     = M.multStd (foldl M.multStd bLinv tms) bR
    r     = m ! (1,2) / m ! (1,1)

tmm :: Layer a => [a] -> (Complex Double) -> (Complex Double) -> Double -> Double
tmm layers nL nR w = realPart $ r * conjugate r
  where
    bLinv = fmap (0.5*) $ M.fromLists [[1:+0,(-1/nL)],[1:+0,1/nL]]
    bR    = M.fromLists [[1:+0,1:+0],[(-1*nR), nR]]
    tms   = map (tm w) layers
    m     = M.multStd (foldl M.multStd bLinv tms) bR
    r     = m ! (1,2) / m ! (1,1)

--------

simple_layer :: Complex Double -> Double -> Double -> M.Matrix (Complex Double)
simple_layer n t w = M.fromLists [[cos theta, (0 :+ 1) * sin theta / n],
                            [(0 :+ 1) *  sin theta * n, cos theta]]
        where
            theta = (2 * pi * t / w :+ 0) * n

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

sellmeier_layer :: SellmeierMat -> Double -> Double -> M.Matrix (Complex Double)
sellmeier_layer (SellmeierMat a b c) t w = M.fromLists [[cos theta, (0 :+ 1) * sin theta / n],
                                                        [(0 :+ 1) *  sin theta * n, cos theta]]
        where
            theta = (2 * pi * t / w :+ 0) * n
            n     = sqrt(a + ((b * w^2) / (w^2 - c))) :+ 0

transfermatrixmethod nL nR layers = magnitudeSq . r . flip M.multStd bR . foldl M.multStd bLinv . (<*>) layers . pure
    where
        bLinv = fmap (0.5*) $ M.fromLists [[1,(-1/nL)],[1,1/nL]]
        bR    = M.fromLists [[1,1],[(-1*nR), nR]]
        r m   = m ! (1,2) / m ! (1,1)
        magnitudeSq c = realPart $ c * conjugate c



main :: IO ()
main = putStrLn "Hello, Haskell!"
