{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Image.Transform where

import Image.RepaImage

import Codec.Picture
import Data.Word
import Control.Monad.ST
import Data.STRef
import Data.Array.Repa hiding ((++),map,zipWith)
import qualified Data.Array.Repa as R (map)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as M
import Control.Monad

repaMap :: (Shape sh, Source r a) => (a -> b) -> Array r sh a -> Array D sh b
repaMap = R.map

data ImageStats = ImageStats {
    count :: Int
  , intensitySum :: Int
  , histogram :: V.Vector Int
} deriving (Show)

computeImageStats :: (Source a Pixel8) => RepaImage a Pixel8 -> ImageStats
computeImageStats img = ImageStats {
      count        = width * height
    , intensitySum = intensity
    , histogram    = hist
  }
  where Z :. width :. height = extent img
        hist                 = getHist img
        intensity            = V.foldl1' (+) $ V.imap (\i v -> i * v) hist

getHist :: (Source a Pixel8) => RepaImage a Pixel8 -> V.Vector Int
getHist img = runST $ do
  vec <- V.unsafeThaw $ V.replicate 256 0

  let Z :. width :. height = extent img
      size = width * height

  forM_ [0..size] $ \i -> do
    let elem = linearIndex img i
    M.modify vec (+1) (fromIntegral elem)

  V.unsafeFreeze vec

cond :: (Monad m) => Bool -> m () -> m ()
cond True  f = f
cond False _ = return ()

cond' :: Bool -> (a -> a) -> a -> a
cond' True  f v = f v
cond' False _ v = v

otsu :: ImageStats -> Word8
otsu ImageStats {..} = runST $ do
  bestThresh             <- newSTRef 0
  bestSigma              <- newSTRef 0
  firstClassPixelCount   <- newSTRef 0
  firstClassIntensitySum <- newSTRef 0
  forM_ (dropWhile (\i -> histogram V.! i == 0) [0..254]) $ \thresh -> do
    modifySTRef firstClassPixelCount   $ (+) . fromIntegral $ histogram V.! thresh
    modifySTRef firstClassIntensitySum $ (+) . fromIntegral $ thresh * histogram V.! thresh
    fcpc <- readSTRef firstClassPixelCount
    fcis <- readSTRef firstClassIntensitySum
    let firstClassProb  = fcpc / fromIntegral count
        secondClassProb = 1 - firstClassProb
        firstClassMean  = fcis / fcpc
        secondClassMean = (fromIntegral intensitySum - fcis) / (fromIntegral count - fcpc + 1)
        meanDelta = firstClassMean - secondClassMean
        sigma     = firstClassProb * secondClassProb * meanDelta * meanDelta
    bs <- readSTRef bestSigma
    cond (sigma > bs) $ do
      writeSTRef bestSigma sigma
      writeSTRef bestThresh thresh
  fromIntegral <$> readSTRef bestThresh

computeOtsu :: (Source a Pixel8) => RepaImage a Pixel8 -> Word8
computeOtsu = otsu . computeImageStats

toBinPixel :: Word8 -> Pixel8 -> Pixel8
toBinPixel thr pix = if thr <= pix then white else black

black :: Word8
black = 0

white :: Word8
white = 255

-- Эрозия и наращивание для 8 пикселей
modPixel8 :: (Source a Pixel8) => Pixel8 -> RepaImage a Pixel8 -> ImageShape -> Pixel8
modPixel8 _ img sh@(Z :. 0 :. _) = img ! sh
modPixel8 _ img sh@(Z :. _ :. 0) = img ! sh
modPixel8 tcol img sh@(Z :. x :. y)
  | x >= width-1 = img ! sh
  | y >= height-1 = img ! sh
  |otherwise = if cond then tcol else img ! sh
     where (Z :. width :. height) = extent img
           cond =  img ! (Z :. x   :. y-1) == tcol
                || img ! (Z :. x   :. y+1) == tcol
                || img ! (Z :. x-1 :. y)   == tcol
                || img ! (Z :. x-1 :. y-1) == tcol
                || img ! (Z :. x-1 :. y+1) == tcol
                || img ! (Z :. x+1 :. y)   == tcol
                || img ! (Z :. x+1 :. y-1) == tcol
                || img ! (Z :. x+1 :. y+1) == tcol
{-# INLINE modPixel8 #-}

-- Эрозия и наращивание для 4 пикселей
modPixel4 :: (Source a Pixel8) => Pixel8 -> RepaImage a Pixel8 -> ImageShape -> Pixel8
modPixel4 _    img sh@(Z :. 0 :. _) = img ! sh
modPixel4 _    img sh@(Z :. _ :. 0) = img ! sh
modPixel4 tcol img sh@(Z :. x :. y)
  | x >= width-1  = img ! sh
  | y >= height-1 = img ! sh
  |otherwise = if cond then tcol else img ! sh
     where (Z :. width :. height) = extent img
           cond =  img ! (Z :. x :. y-1) == tcol
                || img ! (Z :. x :. y+1) == tcol
                || img ! (Z :. x-1 :. y) == tcol
                || img ! (Z :. x+1 :. y) == tcol
{-# INLINE modPixel4 #-}

erasePixel4 :: (Source a Pixel8) => RepaImage a Pixel8 -> ImageShape -> Pixel8
erasePixel4 = modPixel4 white
{-# INLINE erasePixel4 #-}

erasePixel8 :: (Source a Pixel8) => RepaImage a Pixel8 -> ImageShape -> Pixel8
erasePixel8 = modPixel8 white
{-# INLINE erasePixel8 #-}

buildupPixel4 :: (Source a Pixel8) => RepaImage a Pixel8 -> ImageShape -> Pixel8
buildupPixel4 = modPixel4 black
{-# INLINE buildupPixel4 #-}

buildupPixel8 :: (Source a Pixel8) => RepaImage a Pixel8 -> ImageShape -> Pixel8
buildupPixel8 = modPixel8 black
{-# INLINE buildupPixel8 #-}
