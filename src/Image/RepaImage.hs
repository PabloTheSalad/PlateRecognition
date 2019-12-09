{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Image.RepaImage where

import Codec.Picture
import Data.Array.Repa hiding ((++),map,zipWith)
import Data.Array.Repa.Eval

-- import Control.Monad.ST
-- import qualified Data.Vector.Unboxed as V
-- import qualified Data.Vector.Unboxed.Mutable as M
-- import Control.Monad

type RepaImage a b = Array a DIM2 b
type RepaImageU8 = RepaImage U Pixel8
type ImageShape = Z :. Int :. Int
type RImage = Array D DIM2 Pixel8

toRepaImage :: (Pixel p) => Image p -> RepaImage D p
toRepaImage img@Image {..} =  fromFunction (Z :. imageWidth :. imageHeight)
                                           (\(Z :. x :. y) -> pixelAt img x y)

fromRepaImage :: (Pixel p, Load r DIM2 p) => RepaImage r p -> Image p
fromRepaImage img = generateImage gen width height
  where
    Z :. width :. height = extent img
    gen x y = img ! (Z :. x :. y)
