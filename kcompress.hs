{-# LANGUAGE FlexibleInstances #-}

{-
Copyright 2015 Louis-Guillaume Gagnon

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

import Codec.Picture
import Codec.Picture.Types
import Data.Set
import System.Environment
import Data.Word
import System.IO
import System.Exit
import System.Random

data Assignement a = Assignement
    { protoPixel :: a
    , meanPixel  :: a
    , nPixel :: Int
    , indices :: Set (Int,Int)
    }

instance Eq a => Eq (Assignement a) where
  a == b = protoPixel a == protoPixel b

instance Ord a => Ord (Assignement a) where
  a <= b = protoPixel a <= protoPixel b

newtype IntPair = IntPair { unpack :: (Int,Int) }

instance Random IntPair where
  random g =
    let (i,g') = random g
        (j,g'') = random g'
    in (IntPair (i,j),g'')
  randomR (IntPair (i0,j0), IntPair (i1,j1)) g =
    let (i,g') = randomR (i0,i1) g
        (j,g'') = randomR (j0,j1) g'
    in (IntPair (i,j),g'')

main :: IO ()
main = do
  [nstr,inp,outp] <- getArgs
  eimg <- readImage inp
  case eimg of
   Left msg  -> hPutStrLn stderr msg >> exitFailure
   Right img -> do
     let img'  = extractImage img
     let proto = pickProto (read nstr) 1241241 img'
     let img'' = kmeans proto img'
     savePngImage outp $ ImageRGB16 img''

extractImage :: DynamicImage -> Image PixelRGB16
extractImage (ImageY16 p)    = promoteImage p
extractImage (ImageYA8 p)    = promoteImage (promoteImage p :: Image PixelRGB8)
extractImage (ImageYA16 p)   = promoteImage $ dropAlphaLayer p
extractImage (ImageRGB8 p)   = promoteImage p
extractImage (ImageRGB16 p)  = p
extractImage (ImageRGBA8 p)  = promoteImage $ dropAlphaLayer p
extractImage (ImageRGBA16 p) = dropAlphaLayer p
extractImage _ = error "unsuported pixel format"


pickProto :: Int -> Int -> Image PixelRGB16 -> [PixelRGB16]
pickProto n s img =
  let w = imageWidth img
      h = imageHeight img
      idx = take n $ randomRs (IntPair (0,0), IntPair (w,h)) (mkStdGen s)
  in Prelude.map (uncurry (pixelAt img) . unpack) idx

-- TODO: add convergence test
kmeans :: [PixelRGB16] -> Image PixelRGB16 -> Image PixelRGB16
kmeans = kmeans' (0 :: Int)
  where kmeans' n proto img =
          let as = assign img proto
          in  if n >= 100
              then toImage as img
              else kmeans' (n + 1) (toList (Data.Set.map meanPixel as)) img

assign :: Image PixelRGB16 -> [PixelRGB16] -> Set (Assignement PixelRGB16)
assign img proto =
  let update' as x y px = insert (update (closest as px) x y px) as
  in  pixelFold update' (initialize proto) img


update :: Assignement PixelRGB16 -> Int -> Int -> PixelRGB16 ->
          Assignement PixelRGB16
update a x y px =
  let mean = meanPixel a
      n = fromIntegral $ nPixel a `mod` maxBound
      mean' = updateMean mean px n
      indices' = insert (x,y) $ indices a
      nPixel' = nPixel a + 1
  in Assignement (protoPixel a) mean' nPixel' indices'

updateMean :: PixelRGB16 -> PixelRGB16 -> Word16 -> PixelRGB16
updateMean _ p 0 = p
updateMean m p n = m `pixelAdd` ((p `pixelDiff` m) `pixelDiv` PixelRGB16 n n n)

pixelAdd :: PixelRGB16 -> PixelRGB16 -> PixelRGB16
pixelAdd  (PixelRGB16 r0 g0 b0)  (PixelRGB16 r1 g1 b1) =
  PixelRGB16 (r0 + r1) (g0 + g1) (b0 + b1)

pixelDiff :: PixelRGB16 -> PixelRGB16 -> PixelRGB16
pixelDiff (PixelRGB16 r0 g0 b0)  (PixelRGB16 r1 g1 b1) =
  PixelRGB16 (r0 - r1) (g0 - g1) (b0 - b1)

pixelDiv :: PixelRGB16 -> PixelRGB16 -> PixelRGB16
pixelDiv  (PixelRGB16 r0 g0 b0)  (PixelRGB16 r1 g1 b1) =
  PixelRGB16 (r0 `div` r1) (g0 `div` g1) (b0 `div` b1)

closest :: Set (Assignement PixelRGB16) -> PixelRGB16 -> Assignement PixelRGB16
closest as px = fst $ foldl' step start as
  where start = (undefined, maxBound :: Int)
        step (as',da) as'' =
          let d = pixelNorm $ px `pixelDiff` protoPixel as'
          in  if d < da
              then (as'',d)
              else (as',da)

pixelNorm :: PixelRGB16 -> Int
pixelNorm (PixelRGB16 r g b) =
  let r' = fromIntegral r
      g' = fromIntegral g
      b' = fromIntegral b
  in truncate (sqrt $ r'*r' + g'*g' + b'*g' :: Double)

initialize :: [PixelRGB16] -> Set (Assignement PixelRGB16)
initialize pxs = fromList $ Prelude.map (\p -> Assignement p (PixelRGB16 0 0 0) 0 empty) pxs

toImage :: Set (Assignement PixelRGB16) -> Image PixelRGB16 -> Image PixelRGB16
toImage as img = generateImage indiceMap (imageWidth img) (imageHeight img)
  where indiceMap x y =
          let as' = toList as
              (p:_) = dropWhile (\a -> not ((x,y) `member` indices a)) as'
          in protoPixel p
