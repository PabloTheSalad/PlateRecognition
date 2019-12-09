{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Image.Recognition where

import Image.RepaImage
import Image.IO

import Codec.Picture
import Data.Array.Repa hiding ((++),map,zipWith,transpose)
import qualified Data.Array.Repa as R (zipWith)
import Control.Monad

import System.FilePath
import System.Directory
import Data.List

diffRepaImage :: (Pixel p, Source r p, Num p, Ord p, Monad m) => RepaImage r p -> RepaImage r p -> m Int
diffRepaImage f s = sumAllP $ R.zipWith (\x y -> if x /= y then 0 else 1) f' s
  where f' = resizeRepaImage (extent s) f

resizeRepaImage :: (Pixel p, Source r p) => DIM2 -> RepaImage r p -> RepaImage D p
resizeRepaImage sh img = fromFunction sh (\sh' -> img ! mapCoord sh (extent img) sh')

mapCoord :: DIM2 -> DIM2 -> DIM2 -> DIM2
mapCoord (Z :. w :. h) (Z :. w' :. h') (Z :. x :. y) = (Z :. x' :. y')
  where x' = truncate $ fromIntegral x / fromIntegral w * fromIntegral w'
        y' = truncate $ fromIntegral y / fromIntegral h * fromIntegral h'

type SampleMap = [(Char, RepaImage D Pixel8)]

loadSamples :: FilePath -> IO SampleMap
loadSamples dir = do
  files <- listDirectory dir

  list <- forM files $ \file -> do
    pic <- loadPicture $ dir </> file
    case pic of
      Left str -> error str
      Right pic' -> return (head $ takeBaseName file, pic')

  return list

--Probability char (Сhar - expected symbol, Int - probability of symbol, less is better)
type ProbChar = [(Char, Int)]
type ProbString = [ProbChar]

findSample :: (Monad m) => SampleMap -> RepaImage D Pixel8 -> m ProbChar
findSample samples img = do
  x <- forM samples $ \(str, li) -> do
    x <- diffRepaImage li img
    return (str, x)
  return $ sortOn snd $ x
