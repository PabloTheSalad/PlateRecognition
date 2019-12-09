{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Path.IO where

import Image.RepaImage
import Image.IO
import Image.Recognition
import Path.Path

import Codec.Picture
import Data.Array.Repa hiding ((++),map,zipWith)
import Data.Array.Repa.Eval

import Control.Monad
import System.FilePath

saveObject :: (Load r DIM2 Pixel8) => FilePath -> RepaImage r Pixel8 -> Object -> IO ()
saveObject file img (Object m _) = saveRect file img . getRect $ m

saveRect :: (Pixel p, PngSavable p, Load r DIM2 p) => FilePath -> RepaImage r p -> Rect Int -> IO ()
saveRect file img rect = savePicture file . extractRect img $ rect

recognizePaths :: (Load r DIM2 Pixel8) => FilePath -> RepaImage r Pixel8 -> Object -> IO [[(Char, Int)]]
recognizePaths path img obj = do
  samples <- loadSamples path
  recognizePaths' samples img obj

recognizePaths' :: (Load r DIM2 Pixel8, Monad m) => SampleMap -> RepaImage r Pixel8 -> Object -> m [[(Char, Int)]]
recognizePaths' samples img (Object _ li) = mapM (\p -> findSample samples $ extractRect img $ getRect p) li

saveFullObject :: (Load r DIM2 Pixel8) => FilePath -> RepaImage r Pixel8 -> Object -> IO ()
saveFullObject path img obj@(Object _ i) = do
  let objName = path <.> "png"
      len = length i
  saveObject objName img obj
  forM_ (zip i [len,len-1..]) $ \(p, n) -> do
    let name = path ++ "_" ++ show n <.> "png"
    saveRect name img $ getRect p
