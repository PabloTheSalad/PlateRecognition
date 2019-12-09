module Lib where

import Image.IO
import Image.Transform
import Image.Recognition
import Path.Path
import Path.IO
import Control.Monad

import Data.Array.Repa hiding ((++),map,zipWith)
import System.FilePath
import System.Environment

import Data.Char
import Data.List
import System.IO

import Codec.Picture

red :: PixelRGB8
red = PixelRGB8 255 0 0

saveFullPictureRecognition :: IO ()
saveFullPictureRecognition = do
  [file] <- getArgs
  img    <- either error id <$> (loadPicture $ file)
  imgRaw <- either error id <$> (loadRawPicture $ file)
  let size   = extent img
      otsu   = computeOtsu img
      binImg = repaMap (toBinPixel otsu) img
  lastPic <- computeUnboxedP binImg
  let (pathInfo, matrix) = findAllPathInfoWithMatrix lastPic white
      objects     = findObjectsInImage lastPic white signSelector
      newFileName = "./imageTest" </> takeBaseName file
      dLastPic    = delay lastPic
  savePicture (newFileName ++ "PathColor" <.> "png") $ fromFunction size (\sh -> if matrix ! sh then red else imgRaw ! sh)
  savePicture (newFileName ++ "RectColor" <.> "png") $ foldl' (flip $ drawPathInfo red) imgRaw pathInfo
  let (Object outer inner) = head objects
  putStrLn $ show $ toRect $ limits outer
  savePicture (newFileName ++ "Sign" <.> "png") $ drawPathInfo red outer $ foldl' (flip $ drawPathInfo red) imgRaw inner
  savePicture (newFileName ++ "Matrix" <.> "png") $ fromFunction size (\sh -> if matrix ! sh then black else white)
  savePicture (newFileName ++ "Path" <.> "png") $ fromFunction size (\sh -> if matrix ! sh then 128 else lastPic ! sh)
  savePicture (newFileName ++ "Bin" <.> "png") dLastPic -- Сохранение бинаризованной картинки
  mapM_ (\(p, n) -> saveFullObject (newFileName ++ "_" ++ show n) dLastPic p) $ zip objects [1..] -- Сохранение всех найденных объектов

data Config = Config
  { outDir :: FilePath
  , samplesDir :: FilePath
  , dataDir :: FilePath
  , files :: [(FilePath, [String])]
  }

loadConfig :: FilePath -> IO Config
loadConfig path = do
  cont <- openFile path ReadMode >>= hGetContents
  let l = lines cont
      [out, sample, data'] = words $ head l
      files' = map (\s -> let (w:ws) = words s in (w, ws)) $ tail l

  return Config
    { outDir = out
    , samplesDir = sample
    , dataDir = data'
    , files = files'
    }

recognizeImages :: FilePath -> ObjectSelector -> (ProbString -> String) -> IO ()
recognizeImages cpath selector parser = do
  config <- loadConfig cpath
  samples <- loadSamples $ samplesDir config

  forM_ (files config) $ \(file, exs) -> do
    img <- either error id <$> (loadPicture $ dataDir config </> file)
    let otsu   = computeOtsu img
        binImg = repaMap (toBinPixel otsu) img
    lastPic <- computeUnboxedP binImg
    let objects     = findObjectsInImage lastPic white selector
        newFileName = outDir config </> takeBaseName file
        dLastPic    = delay lastPic
    signs <- mapM (recognizePaths' samples dLastPic) objects

    putStrLn $ "Otsu: " ++ show otsu

    savePicture (newFileName ++ "Bin" <.> "png") dLastPic
    mapM_ (\(p, n) -> saveFullObject (newFileName ++ "_" ++ show n <.> "png") dLastPic p) $ zip objects [1..]

    putStrLn $ concat [file, ": ", show exs, " -> ", (show $ map parser signs)]

parseSign :: ProbString -> String
parseSign chars = zipWith (\p n -> parseSignChar (take 5 p) n) chars' [0..]
  where chars' = reverse $ map (map fst) chars

parseSignChar :: [Char] -> Int -> Char
parseSignChar p n
  | any (==n) [1,2,3,6,7,8] = toDigit $ selectExpected (\x-> isDigit x || x == 'O') p
  | any (==n) [0,4,5]       = selectExpected isAlpha p
  | otherwise               = head p

selectExpected :: (Char -> Bool) -> [Char] -> Char
selectExpected f li = maybe (head li) id (find f li)

toDigit :: Char -> Char
toDigit ch
  | ch == 'O' = '0'
  | ch == 'B' = '8'
  | otherwise = ch

signSelector :: ObjectSelector
signSelector = ObjectSelector
  { mainPISelector = PISelector
    { colorS = white
    , closedS = True
    , rateS = (1, 10)
    , areaS = (200, maxBound)
    }
  , innerPISelector = PISelector
    { colorS = black
    , closedS = True
    , rateS = (0.3, 0.9)
    , areaS = (50, maxBound)
    }
  , innerPICount = (4, 10)
  , innerPIDest = ((0, 0.95), (0, 0.6))
  }
