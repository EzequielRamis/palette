{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Control.Exception (ArithException, evaluate, try)
import Control.Monad ((=<<))
import Data.Aeson
  ( ToJSON (toEncoding),
    defaultOptions,
    encode,
    genericToEncoding,
  )
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Either (rights)
import Data.Functor (void)
import Data.Maybe (fromMaybe, maybe)
import GHC.Generics (Generic)
import GHC.IO.Handle (hGetContents)
import GHC.Num.Natural (Natural)
import System.Process (createProcess_, shell)

type Coord = (Double, Double, Double)

type Coord' = (Natural, Natural, Natural)

type Config = (Double, Double, Double -> Double)

data Ease
  = Line
  | InQuad
  | InCubic
  | InQuart
  | InQuint
  | InSine
  | InExpo
  | InCirc
  | OutQuad
  | OutCubic
  | OutQuart
  | OutQuint
  | OutSine
  | OutExpo
  | OutCirc
  | InOutQuad
  | InOutCubic
  | InOutQuart
  | InOutQuint
  | InOutSine
  | InOutExpo
  | InOutCirc

data Hue = H
  { name :: String,
    colors :: [String]
  }
  deriving (Generic, Show)

data Huetone = HT
  { name :: String,
    hues :: [Hue],
    tones :: [String]
  }
  deriving (Generic, Show)

instance ToJSON Hue where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON Huetone where
  toEncoding = genericToEncoding defaultOptions

main :: IO ()
main = genHuetone "Palette" 16 [mono, red, orange, gold, green, turquoise, blue, lavender]

padr :: Int -> String -> String
padr n h = let l = length h in take (n - l) ['0' ..] ++ h

rgb2hex :: Coord' -> String
rgb2hex (r, g, b) = "#" ++ concatMap (padr 2 . hex) [r, g, b]

hex :: Natural -> String
hex n =
  if n < 16
    then hex' n
    else
      let d = div n 16
          r = mod n 16
       in hex d ++ hex' r

hex' 10 = "A"
hex' 11 = "B"
hex' 12 = "C"
hex' 13 = "D"
hex' 14 = "E"
hex' 15 = "F"
hex' n = show n

-- https://gist.github.com/jjrv/b27d0840b4438502f9cad2a0f9edeabc implementation

linearize :: Natural -> Double
linearize x =
  let x' = fromIntegral x / 255
   in if x' <= 0.04045
        then x' / 12.92
        else ((x' + 0.055) / 1.055) ** 2.4

delinearize :: Double -> Natural
delinearize x =
  let x' =
        if x <= 0.0031308
          then x * 12.92
          else (x ** (1 / 2.4)) * 1.055 - 0.055
   in round $ x' * 255

cube = (** 3)

cbrt = (** (1 / 3))

oklab2rgb :: Coord -> Coord'
oklab2rgb (l', a', b') =
  let a = a' * 0.27
      b = b' * 0.27

      m = cube $ l' - 0.1055613458 * a - 0.0638541728 * b
      s = cube $ l' - 0.0894841775 * a - 1.2914855480 * b
      l = cube $ l' + 0.3963377774 * a + 0.2158037573 * b

      o0 = delinearize $ 4.0767416621 * l - 3.3077115913 * m + 0.2309699292 * s
      o1 = delinearize $ -1.2684380046 * l + 2.6097574011 * m - 0.3413193965 * s
      o2 = delinearize $ -0.0041960863 * l - 0.7034186147 * m + 1.7076147010 * s
   in (o0, o1, o2)

rgb2oklab :: Coord' -> Coord
rgb2oklab (r', g', b') =
  let r = linearize r'
      g = linearize g'
      b = linearize b'

      l = cbrt $ 0.4122214708 * r + 0.5363325363 * g + 0.0514459929 * b
      m = cbrt $ 0.2119034982 * r + 0.6806995451 * g + 0.1073969566 * b
      s = cbrt $ 0.0883024619 * r + 0.2817188376 * g + 0.6299787005 * b

      o0 = 0.2104542553 * l + 0.793617785 * m - 0.0040720468 * s
      o1 = (1.9779984951 * l - 2.428592205 * m + 0.4505937099 * s) / 0.27
      o2 = (0.0259040371 * l + 0.7827717662 * m - 0.808675766 * s) / 0.27
   in (o0, o1, o2)

oklab2oklch :: Coord -> Coord
oklab2oklch (l, a, b) = (l, sqrt (a ** 2 + b ** 2), atan2 b a)

oklch2oklab :: Coord -> Coord
-- oklch2oklab (l, c, h) = (l, c * cos h, c * sin h)
oklch2oklab (l, c, h) = let d = h * pi / 180 in (l / 100, c * cos d, c * sin d)

oklch2rgb' :: Coord -> Coord'
oklch2rgb' = oklab2rgb . oklch2oklab

rgb2oklch :: Coord' -> Coord
rgb2oklch = oklab2oklch . rgb2oklab

oklch2rgb :: Coord -> IO (Maybe Coord')
oklch2rgb c =
  let (r', g', b') = oklch2rgb' c
      eval :: Natural -> IO (Either ArithException Natural)
      eval = try . evaluate
   in do
        r <- eval r'
        g <- eval g'
        b <- eval b'
        let valids = rights [r, g, b]
        if length valids == 3 && all (< 256) valids
          then
            let [o0, o1, o2] = valids
             in return $ Just (o0, o1, o2)
          else return Nothing

oklch2hex :: Coord -> IO (Maybe String)
oklch2hex c = (rgb2hex <$>) <$> oklch2rgb c

-- x \in [0,1]
ease :: Ease -> Double -> Double
ease Line x = x
ease InQuad x = x ** 2
ease InCubic x = x ** 3
ease InQuart x = x ** 4
ease InQuint x = x ** 5
ease InSine x = cos (x * pi / 2)
ease InExpo 0 = 0
ease InExpo x = 2 ** (10 * x - 10)
ease InCirc x = 1 - sqrt (1 - x ** 2)
--
ease OutQuad x = 1 - (1 - x) ** 2
ease OutCubic x = 1 - (1 - x) ** 3
ease OutQuart x = 1 - (1 - x) ** 4
ease OutQuint x = 1 - (1 - x) ** 5
ease OutSine x = sin (x * pi / 2)
ease OutExpo 1 = 1
ease OutExpo x = 1 - 2 ** (-10 * x)
ease OutCirc x = sqrt (1 - (x - 1) ** 2)
--
ease InOutQuad x = if x < 0.5 then 2 * x ** 2 else 1 - (-2 * x + 2) ** 2 / 2
ease InOutCubic x = if x < 0.5 then 4 * x ** 3 else 1 - (-2 * x + 2) ** 3 / 2
ease InOutQuart x = if x < 0.5 then 8 * x ** 4 else 1 - (-2 * x + 2) ** 4 / 2
ease InOutQuint x = if x < 0.5 then 16 * x ** 5 else 1 - (-2 * x + 2) ** 5 / 2
ease InOutSine x = -(cos (pi * x) - 1) / 2
ease InOutExpo 0 = 0
ease InOutExpo 1 = 1
ease InOutExpo x = if x < 0.5 then 2 ** (20 * x - 10) / 2 else (2 - 2 ** (-20 * x + 10)) / 2
ease InOutCirc x = if x < 0.5 then (1 - sqrt (1 - (2 * x) ** 2)) / 2 else (sqrt (1 - (-2 * x + 2) ** 2) + 1) / 2

sample :: Double -> Double -> Natural -> [Double]
sample _ _ 0 = []
sample from _ 1 = [from]
sample from to resolution
  | from == to = replicate (fromIntegral resolution) from
  | otherwise = enumFromThenTo from (from + (to - from) / fromIntegral (resolution - 1)) to

-- lightness \in [0,1]
-- chroma \in [0,?]
-- hue \in [0,2pi]

curve :: Natural -> Config -> [Double]
curve r (f, t, e)
  | f == t = sample f t r
  | otherwise = map (\x -> e ((x - f) / (t - f)) * (t - f) + f) $ sample f t r

genHue :: Natural -> (Config, Config, Config) -> IO [Maybe String]
genHue r (l, c, h) = mapM oklch2hex $ zip3 (curve r l) (curve r c) (curve r h)

genHuetone' :: String -> [(Config, Config, Config)] -> Natural -> IO Huetone
genHuetone' n hs ts =
  let lh = length hs
      ts' = fromIntegral ts
   in do
        hs' <- mapM (genHue ts) hs
        return
          HT
            { name = n,
              hues =
                zipWith
                  ( \i h ->
                      H
                        { name = (padr (length $ hex (fromIntegral lh - 1)) . hex) i,
                          colors = map (fromMaybe "#000000") h
                        }
                  )
                  [0 .. fromIntegral lh - 1]
                  hs',
              tones = map (padr (length $ hex (ts' - 1)) . hex) [0 .. ts - 1]
            }

toClipboard :: B.ByteString -> IO ()
toClipboard s =
  void $
    createProcess_ "toClipboard" $
      shell $ "echo " ++ show s ++ " | xclip"

genHuetone :: String -> Natural -> [(Config, Config, Config)] -> IO ()
genHuetone n ts ht =
  (toClipboard . encode) =<< genHuetone' n ht ts

mono :: (Config, Config, Config)
mono = ((99, 15, ease InOutSine), (0.02, 0.04, id), (250, 250, id))

red :: (Config, Config, Config)
red = ((97, 25, id), (0.09, 0.85, \x -> -0.5 * cos (1.6 * pi * x + 0.4) + 0.4), (25, 25, id))

orange :: (Config, Config, Config)
orange = ((95, 30, id), (0.02, 0.7, \x -> -0.35 * cos (1.8 * pi * x + 0.5) + 0.18 * x + 0.4), (45, 45, id))

gold :: (Config, Config, Config)
gold = ((98, 25, id), (0.02, 0.5, \x -> -0.7 * cos (1.5 * pi * x + 1.7) + x), (75, 75, id))

green :: (Config, Config, Config)
green = ((96, 30, id), (0.0, 0.35, \x -> -cos (1.4 * pi * x + 1.8) + 1.2 * x + 0.3), (150, 150, id))

turquoise :: (Config, Config, Config)
turquoise = ((95, 30, id), (0.0, 0.45, \x -> -0.6 * cos (1.5 * pi * x + 1.9) + 0.7 * x + 0.3), (170, 170, id))

blue :: (Config, Config, Config)
blue = ((95, 30, id), (0.05, 0.3, \x -> -cos (1.4 * pi * x + 1.1) + 0.9 * x + 0.6), (250, 250, id))

lavender :: (Config, Config, Config)
lavender = ((96, 25, id), (0.08, 0.4, \x -> -1.1 * cos (1.3 * pi * x + 0.7) + 0.8), (285, 285, id))
