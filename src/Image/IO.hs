{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Image.IO where

import Image.RepaImage

import Codec.Picture
import Codec.Picture.Types
import Data.Array.Repa hiding ((++),map,zipWith)
import Data.Array.Repa.Eval

-- Загрузка/Сохранение изображения
loadPicture :: FilePath -> IO (Either String (RepaImage D Pixel8))
loadPicture path = do
  image <- readImage path
  return $ fmap (toRepaImage . extractLumaPlane . convertRGB8) image

loadRawPicture :: FilePath -> IO (Either String (RepaImage D PixelRGB8))
loadRawPicture path = do
  image <- readImage path
  return $ fmap (toRepaImage . convertRGB8) image

savePicture :: (Pixel p, PngSavable p, Load r DIM2 p) => FilePath -> RepaImage r p -> IO ()
savePicture path image = writePng path $ fromRepaImage image
