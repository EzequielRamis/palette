module Main where

import Control.Exception (ArithException, evaluate, try)
import Data.Either (rights)
import Data.Maybe (maybe)
import GHC.Num.Natural (Natural)

type Coord = (Double, Double, Double)

type Coord' = (Natural, Natural, Natural)

main :: IO ()
main = print $ rgb2oklab (255, 255, 255)

padr :: Int -> String -> String
padr n h = let l = length h in take (n - l) ['0' ..] ++ h

rgb2hex :: Coord' -> String
rgb2hex (r, g, b) = concatMap (padr 2 . hex) [r, g, b]

hex :: Natural -> String
hex n =
  if n < 16
    then hex' n
    else
      let d = div n 16
          r = mod n 16
       in hex d ++ hex' r

hex' 10 = "a"
hex' 11 = "b"
hex' 12 = "c"
hex' 13 = "d"
hex' 14 = "e"
hex' 15 = "f"
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
oklch2oklab (l, c, h) = (l, c * cos h, c * sin h)

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
             in pure $ Just (o0, o1, o2)
          else pure Nothing

oklch2hex :: Coord -> IO (Maybe String)
oklch2hex c = (rgb2hex <$>) <$> oklch2rgb c
