{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Path.Path where

import Image.RepaImage
import Image.Transform

import Codec.Picture.Types
import Control.Monad.ST
import Data.Array.Repa hiding ((++),map,zipWith)
import Data.Array.Repa.Eval
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as M
import Control.Monad
import Control.Monad.Extra (findM)
import Data.Ratio
import Data.Function

class (Eq a, Enum a, Bounded a) => Cycled a where
  cyclePred :: a -> a
  cycleSucc :: a -> a
  cyclePred x = if x == minBound then maxBound else pred x
  cycleSucc x = if x == maxBound then minBound else succ x

data Direction = North | NorthEast
               | East  | SouthEast
               | South | SouthWest
               | West  | NorthWest
               deriving (Bounded, Enum, Eq, Show)

instance Cycled Direction

inverse :: Direction -> Direction
inverse x = iterate cycleSucc x !! 4

getAllDir :: Direction -> [Direction]
getAllDir dir = take 8 $ iterate cyclePred dir

type Point a = Z :. a :. a

(.+) :: (Num a) => Point a -> Point a -> Point a
(Z :. x1 :. y1) .+ (Z :. x2 :. y2) = Z :. x1+x2 :. y1+y2

inImage :: Point Int -> RepaImage U Pixel8 -> Bool
inImage (Z :. x :. y) img =  x >= 0
                          && x < width
                          && y >= 0
                          && y < height
  where Z :. width :. height = extent img
{-# INLINE inImage #-}

fromPair :: (a, a) -> Point a
fromPair (f,s) = Z :. f :. s

toPoint :: Direction -> Point Int
toPoint dir = fmap fromPair [(0,-1),(1,-1),(1,0),(1,1),(0,1),(-1,1),(-1,0),(-1,-1)] !! fromEnum dir

data Path p = Path
  { getPathInfo :: PathInfo p
  , getPath :: [Point p]
  } deriving (Show, Eq)

data PathInfo p = PathInfo
  { color :: Pixel8
  , closed :: Bool
  , limits :: Limits p
  } deriving (Show, Eq)

isClosed :: Path a -> Bool
isClosed (Path pi _) = closed pi

getColor :: Path a -> Pixel8
getColor (Path pi _) = color pi

findNextDir :: RepaImage U Pixel8 -> Pixel8 -> Point Int -> Direction -> Maybe Direction
findNextDir img col p dir = join $ findM pred $ getAllDir dir
  where pred dir
          | not $ point `inImage` img = Nothing
          | img ! point == col        = Just True
          | otherwise                 = Just False
          where point = p .+ toPoint dir

computePath :: RepaImage U Pixel8 -> Point Int -> Path Int
computePath img p = loop (emptyLimits `addPoint` p, [p]) p South
  where color            = img ! p
        loop (limits, list) p' dir = maybe (Path (PathInfo white False limits) list) loop' newDir
          where newDir    = findNextDir img color p' (cyclePred dir)
                loop' dir = let point = p' .+ toPoint dir
                            in if point == p
                               then Path (PathInfo (defineColor $ inverse dir) True limits) list
                               else loop (limits `addPoint` point, point:list) point (inverse dir)

defineColor :: Direction -> Pixel8
defineColor col
 | enum >= 0 && enum < 4 = white
 | otherwise           = black
 where enum = fromEnum col

nextPoint :: DIM2 -> Point Int -> Point Int
nextPoint size p = fromIndex size (toIndex size p + 1)
{-# INLINE nextPoint #-}

findFirstPath :: RepaImage U Pixel8 -> Pixel8 -> Point Int -> Point Int
findFirstPath img col p@(Z :. x :. _)
  | not $ p `inImage` img = p
  | x == x' && img ! p == col && img ! p' /= col = p
  | otherwise = findFirstPath img col p'
  where p'@(Z :. x' :. _) = nextPoint (extent img) p

findAllPathsWithMatrix :: RepaImage U Pixel8 -> Pixel8 -> ([Path Int], PathMatrix)
findAllPathsWithMatrix img col = loop (Z :. 0 :. 0) [] $ createPathMatrix size
  where size = extent img
        loop p list matrix
          | not $ p' `inImage` img = (list, matrix)
          | not $ matrix ! p'      = loop (nextPoint size p') (path:list) (matrix `unsafeAddPath` path)
          | otherwise              = loop (findFirstPath img col (nextPoint size p')) list matrix
          where p'   = findFirstPath img col p
                path = computePath img p'

findAllPaths :: RepaImage U Pixel8 -> Pixel8 -> [Path Int]
findAllPaths img col = fst $ findAllPathsWithMatrix img col

findAllPathInfoWithMatrix :: RepaImage U Pixel8 -> Pixel8 -> ([PathInfo Int], PathMatrix)
findAllPathInfoWithMatrix img col = (map getPathInfo paths, matrix)
  where (paths, matrix) = findAllPathsWithMatrix img col

findAllPathInfo :: RepaImage U Pixel8 -> Pixel8 -> [PathInfo Int]
findAllPathInfo img col = fst $ findAllPathInfoWithMatrix img col

type PathMatrix = Array U DIM2 Bool

createPathMatrix :: DIM2 -> PathMatrix
createPathMatrix size = computeS $ fromFunction size (\_ -> False)

unsafeAddPath :: PathMatrix -> Path Int -> PathMatrix
unsafeAddPath m (Path _ l) = fromUnboxed (extent m) $ runST $ do
  matrix <- V.unsafeThaw $ toUnboxed m

  let size = (extent m)
  forM_ l $ \p -> do
    let index = toIndex size p
    M.write matrix index True

  V.unsafeFreeze matrix

unsafeAddPaths :: PathMatrix -> [Path Int] -> PathMatrix
unsafeAddPaths m []     = m
unsafeAddPaths m (x:xs) = unsafeAddPaths (m `unsafeAddPath` x) xs

data Limits a = Limits
  { minX :: Point a
  , minY :: Point a
  , maxX :: Point a
  , maxY :: Point a
  } deriving (Show, Eq)

emptyLimits :: (Bounded a) => Limits a
emptyLimits = Limits
  { minX = (Z :. maxBound :. maxBound)
  , minY = (Z :. maxBound :. maxBound)
  , maxX = (Z :. minBound :. minBound)
  , maxY = (Z :. minBound :. minBound)
  }

addPoint :: (Num a, Ord a) => Limits a -> Point a -> Limits a
addPoint (lim@Limits {..})  p@(Z :. x :. y) = cond' (x < minX') (\l -> l { minX = p })
                                            $ cond' (x > maxX') (\l -> l { maxX = p })
                                            $ cond' (y < minY') (\l -> l { minY = p })
                                            $ cond' (y > maxY') (\l -> l { maxY = p }) lim
  where (Z :. minX' :. _) = minX
        (Z :. _ :. minY') = minY
        (Z :. maxX' :. _) = maxX
        (Z :. _ :. maxY') = maxY

data Rect a = Rect (Point a) a a deriving (Show)

toRect :: Num a => Limits a -> Rect a
toRect Limits {..} = Rect (Z :. minX' :. minY') (maxX' - minX') (maxY' - minY')
  where (Z :. minX' :. _) = minX
        (Z :. _ :. minY') = minY
        (Z :. maxX' :. _) = maxX
        (Z :. _ :. maxY') = maxY

getRect :: Num a => PathInfo a -> Rect a
getRect PathInfo {..} = toRect limits

inRect :: (Num a, Ord a) => Point a -> Rect a -> Bool
inRect (Z :. x' :. y') (Rect (Z :. x :. y) w h) =  x' >= x
                                                && x' <= x+w
                                                && y' >= y
                                                && y' <= y+h

inside :: (Num a, Ord a) => Rect a -> Rect a -> Bool
inside (Rect p _ _) rect = p `inRect` rect

onRect :: (Num a, Ord a) => Point a -> Rect a -> Bool
onRect (Z :. x' :. y') (Rect (Z :. x :. y) w h)
  | (y' == y+h || y' == y) && x' <= x+w && x' >= x = True
  | (x' == x+w || x' == x) && y' <= y+h && y' >= y = True
  | otherwise                      = False

getRectArea :: (Num a) => PathInfo a -> a
getRectArea p = w*h
  where (Rect _ w h) = getRect p

getRectRate :: (Num a, Integral a) => PathInfo a -> Ratio a
getRectRate p
  | h == 0    = w % 1
  | otherwise = w % h
  where (Rect _ w h) = getRect p

extractRect :: (Pixel p, Load r DIM2 p) => RepaImage r p -> Rect Int -> RepaImage D p
extractRect img (Rect p w h) = extract p (Z :. w :. h) img

drawThing :: (Pixel p, Source a p) => (Point Int -> Bool) -> RepaImage a p -> p -> RepaImage D p
drawThing f img col = fromFunction (extent img) func
  where func sh
          | f sh      = col
          | otherwise = img ! sh

drawRect :: (Pixel p, Source a p) => Rect Int -> RepaImage a p -> p -> RepaImage D p
drawRect rect = drawThing (\x -> inRect x rect)

drawRectBorder :: (Pixel p, Source a p) => Rect Int -> RepaImage a p -> p -> RepaImage D p
drawRectBorder rect = drawThing (\x -> onRect x rect)

drawPathInfo :: (Pixel p, Source a p) => p -> PathInfo Int -> RepaImage a p -> RepaImage D p
drawPathInfo color pi img = drawRectBorder (toRect $ limits pi) img color

findInside :: [PathInfo Int] -> PathInfo Int -> [PathInfo Int]
findInside list p = filter (\x -> x /= p && (inside `on` getRect) x p) list

diffPerCent :: PathInfo Int -> PathInfo Int -> (Ratio Int, Ratio Int)
diffPerCent pi pi' = ((x' - x) % w, (y' - y) % h)
  where (Rect (Z :. x :. y) w h) = getRect pi
        (Rect (Z :. x' :. y') _ _) = getRect pi'

type Range a = (a, a)

matchRange :: (Ord a) => Range a -> a -> Bool
matchRange (min, max) v = v > min && v < max

data PISelector = PISelector
  { colorS :: Pixel8
  , closedS :: Bool
  , rateS :: Range (Ratio Int)
  , areaS :: Range Int
  }

matchPISelector :: PISelector -> PathInfo Int -> Bool
matchPISelector sel pi =  color       pi == colorS  sel
                       && closed      pi == closedS sel
                       && matchRange (rateS sel) (getRectRate pi)
                       && matchRange (areaS sel) (getRectArea pi)

selectPI :: PISelector -> [PathInfo Int] -> [PathInfo Int]
selectPI sel pil = filter (matchPISelector sel) pil

data Object = Object
  { mainPI :: PathInfo Int
  , innerPI :: [PathInfo Int]
  } deriving (Show)

data ObjectSelector = ObjectSelector
  { mainPISelector  :: PISelector
  , innerPISelector :: PISelector
  , innerPICount :: Range Int
  , innerPIDest :: (Range (Ratio Int), Range (Ratio Int))
  }

matchObject :: ObjectSelector -> Object -> Bool
matchObject sel (Object _ i) = matchRange (innerPICount sel) (length i)

filterObject :: ObjectSelector -> Object -> Object
filterObject sel (Object m i) = Object m $ filter filt i
  where filt pi = matchRange rangeX dx && matchRange rangeY dy
          where (rangeX, rangeY) = innerPIDest sel
                (dx, dy)         = diffPerCent m pi


selectObjects :: ObjectSelector -> [PathInfo Int] -> [Object]
selectObjects sel pil = filter (matchObject sel) objects
  where mainPI  = selectPI (mainPISelector sel)  pil
        innerPI = selectPI (innerPISelector sel) pil
        objects = map (filterObject sel) $ zipWith (\m i -> Object m i) mainPI $ map (findInside innerPI) mainPI

findObjectsInImage :: RepaImage U Pixel8 -> Pixel8 -> ObjectSelector -> [Object]
findObjectsInImage img col sel = selectObjects sel $ findAllPathInfo img col
