{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module HaskQR.Display(displayCode) where

import SDL
import Control.Monad (unless, when)
import Data.Bit
import Data.Bits (xor)
import Data.Vector.Unboxed as V hiding (any)

import HaskQR.Data
import HaskQR.ErrorCorrection (performPolynomialDivision)

displayCode :: [Codeword] -> Version -> IO ()
displayCode code version = do
  SDL.initialize flags 
  window <- SDL.createWindow "My SDL Application" updatedWindow 
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  -- convert the data from a bytestring
  -- let qrDataEx = V.reverse $ castFromWords8 $ V.reverse code -- TODO: understand why this works
  -- print qrDataEx
  _ <- drawCode code version renderer
  waitForUser renderer
  SDL.destroyWindow window
  where
    updatedWindow = SDL.defaultWindow { SDL.windowInitialSize = windowSizeV2 }
        where
          v = getVersionNumber version
          moduleLength = (v - 1) * 4 + 21
          windowSideLength = fromIntegral $ moduleLength * pixelMultiplier
          windowSizeV2 = V2 windowSideLength windowSideLength -- TODO: size should be taken from QRCode Version Number * pixel Multiplier (default is 10)

drawCode :: [Codeword] -> Version -> Renderer -> IO ()
drawCode code version renderer = do
  SDL.rendererDrawColor renderer $= V4 255 255 255 0
  _ <- drawFinderPattern renderer v
  _ <- drawSeparators renderer v
  _ <- drawTimingPattern renderer v
  _ <- drawDataBits renderer matrixPlacementPattern (mconcat code) (\i j c -> if even (i + j) then c `xor` 1 else c) -- mask 0
  let formatString = getFormatString
  _ <- drawDataBits renderer formatStringPattern1 formatString (\_ _ c -> c)
  _ <- drawDataBits renderer formatStringPattern2 formatString (\_ _ c -> c)
  present renderer
  where
    v :: Int
    v = getVersionNumber version
    
    matrixPlacementPattern :: [(Int, Int)]
    matrixPlacementPattern = getMatrixPlacementPattern v
    
    formatStringPattern1 :: [(Int, Int)]
    formatStringPattern1 = getFormatStringPattern1

    formatStringPattern2 :: [(Int, Int)]
    formatStringPattern2 = getFormatStringPattern2

getFormatString :: Vector Bit
getFormatString = V.zipWith xor (databits <> errorbits) [1,0,1,0,1,0,0,0,0,0,1,0,0,1,0] -- spec says to xor the final result with that list
  where
    databits  = [0,1,0,0,0] -- Error Correction L and Mask 0
    errorbits = [1,1,1,1,0,1,0,1,1,0] -- hardcoded for now; performPolynomialDivision [1,0,1,0,0,1,1,0,1,1,1] -- [, ]

drawDataBits :: Renderer -> [(Int, Int)] -> Vector Bit -> (Int -> Int -> Bit -> Bit) -> IO ()
drawDataBits _ [] _ _ = pure ()
drawDataBits renderer ((i,j):ps) code mask = 
  if V.null code then pure ()
  else
   drawPixel i j maskedC >> drawDataBits renderer ps cs mask
   where
    c  = V.head code
    cs = V.tail code
    drawPixel :: Int -> Int -> Bit -> IO ()
    drawPixel x y val = if val == 0 then printPxl renderer x y else pure ()

    -- mask pattern 1
    maskedC = mask i j c

    -- startDrawPattern :: Int -> Int -> Vector Bit -> IO ()
    -- startDrawPattern i j  vector = 
    --   if V.null vector then pure ()
    --   else
    --     drawPixel i j c >> case (newDirection, isLeft) of
    --       (True, True) -> startDrawPattern (i - 1) j (newDirection, False) cs 
    --       (True, False) -> startDrawPattern (i + 1) (j - 1) (newDirection, True) cs
    --       (False, True) -> startDrawPattern (i + 1) (j - 1) (newDirection, True) cs
    --       where
    --         c  = V.head vector
    --         cs = V.tail vector
    --         newDirection = if isUpwards && j == 7 then False else True

waitForUser :: Renderer -> IO ()
waitForUser renderer = do
  events <- SDL.pollEvents
  let eventIsQPress event =
        case SDL.eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
          _ -> False
      qPressed = any eventIsQPress events
  unless qPressed (waitForUser renderer)

flags :: [InitFlag]
flags = [InitVideo, InitEvents] 

pixelMultiplier :: Num a => a
pixelMultiplier = 10

drawFinderPattern :: Renderer -> Int -> IO ()
drawFinderPattern renderer v = do
  _ <- drawTopLeftPattern
  _ <- drawTopRightPattern
  _ <- drawBottomLeftPattern
  pure ()
  where
    topLeftVal :: Int
    topLeftVal = (v - 1) * 4 + 21

    drawTopLeftPattern :: IO ()
    drawTopLeftPattern = drawPattern 0 0

    drawTopRightPattern :: IO ()
    drawTopRightPattern = drawPattern (topLeftVal - 7) 0

    drawBottomLeftPattern :: IO ()
    drawBottomLeftPattern = drawPattern 0 (topLeftVal - 7) 
    
    drawPattern :: Int -> Int -> IO ()
    drawPattern i j = goLeft (i+1) (j+1) >> goDown (i + 5) (j + 1) >> goLeft (i+1) (j+5) >> goDown (i+1) (j+1)
      where
        goLeft :: Int -> Int -> IO () -- should be called goRight
        goLeft x y = if x == (i + 5) then pure () else printPxl renderer x y >> goLeft (x+1) y

        goDown :: Int -> Int -> IO ()
        goDown x y = if y == (j+6) then pure () else printPxl renderer x y >> goDown x (y+1)

drawSeparators :: Renderer -> Int -> IO ()
drawSeparators renderer v = do
  _ <- drawTopLeftSeparator
  _ <- drawTopRightSeparator
  _ <- drawBottomLeftSeparator
  pure ()
  where
    topLeftVal :: Int
    topLeftVal = (v - 1) * 4 + 21

    drawTopLeftSeparator :: IO ()
    drawTopLeftSeparator = goLeft 7 7 >> goDown 7 7

    drawTopRightSeparator :: IO ()
    drawTopRightSeparator = goRight (topLeftVal - 7) 7 >> goDown (topLeftVal - 8) 7

    drawBottomLeftSeparator :: IO ()
    drawBottomLeftSeparator = goLeft 7 (topLeftVal - 8) >> goUp 7 (topLeftVal - 8) 

    goLeft :: Int -> Int -> IO ()
    goLeft x y = if x < 0 then pure () else printPxl renderer x y >> goLeft (x-1) y

    goDown :: Int -> Int -> IO ()
    goDown x y = if y < 0 then pure () else printPxl renderer x y >> goDown x (y-1)

    goRight :: Int -> Int -> IO ()
    goRight x y = if x > topLeftVal then pure () else printPxl renderer x y >> goRight (x+1) y

    goUp :: Int -> Int -> IO ()
    goUp x y = if y > topLeftVal then pure () else printPxl renderer x y >> goUp x (y + 1)

drawTimingPattern :: Renderer -> Int -> IO ()
drawTimingPattern renderer v = do
  _ <- drawTopSeparator
  _ <- drawLeftSeparator
  pure ()
  where
    topLeftVal :: Int
    topLeftVal = (v - 1) * 4 + 21

    drawTopSeparator :: IO ()
    drawTopSeparator = goRight 9 6

    drawLeftSeparator :: IO ()
    drawLeftSeparator = goDown 6 9

    goRight :: Int -> Int -> IO ()
    goRight x y = if x > (topLeftVal - 8) then pure () else printPxl renderer x y >> goRight (x + 2) y

    goDown :: Int -> Int -> IO () 
    goDown x y = if y > (topLeftVal - 8) then pure () else printPxl renderer x y >> goDown x (y + 2)

    
printPxl :: Renderer -> Int -> Int -> IO ()
printPxl renderer i j =
  let 
    valI :: Num a => a
    valI = fromIntegral i * pixelMultiplier

    valJ :: Num a => a
    valJ = fromIntegral j * pixelMultiplier

    pixel :: Num a => Maybe (Rectangle a)
    pixel = Just $ Rectangle (P $ V2 valI valJ) (V2 pixelMultiplier pixelMultiplier) 
  in
    SDL.fillRect renderer pixel

getMatrixPlacementPattern :: Int -> [(Int, Int)]
getMatrixPlacementPattern v = 
  goUpwards (topLeftVal - 1) (topLeftVal - 1) True (==8)
  <> goDownWards 18 9 True (==topLeftVal)
  <> goUpwards (topLeftVal - 5) (topLeftVal - 1) True (==8)
  <> goDownWards 14 9 True (==topLeftVal)
  <> goUpwards (topLeftVal - 9) (topLeftVal - 1) True (<0)
  <> goDownWards 10 0 True (==topLeftVal) 
  <> goUpwards 8 (topLeftVal - 9) True (==8)
  <> goDownWards 5 9 True (==topLeftVal - 8)
  <> goUpwards 3 (topLeftVal - 9) True (==8)
  <> goDownWards 1 9 True (==topLeftVal - 8)
  where
    topLeftVal :: Int
    topLeftVal = (v - 1) * 4 + 21

    goUpwards :: Int -> Int -> Bool -> (Int -> Bool) -> [(Int, Int)]
    goUpwards i j isLeft cond =
      if cond j then []
      else
        case isLeft of
          True -> (i,j):goUpwards (i - 1) j False cond
          False -> if j == 7 then (i,j):goUpwards (i + 1) (j - 2) True cond else (i,j):goUpwards (i + 1) (j - 1) True cond

    goDownWards :: Int -> Int -> Bool -> (Int -> Bool) -> [(Int, Int)]
    goDownWards i j isLeft cond =
      if cond j then []
      else
        case isLeft of
          True -> (i,j):goDownWards (i - 1) j False cond
          False -> if j == 5 then (i,j):goDownWards (i + 1) (j+2) True cond else (i,j):goDownWards (i + 1) (j+1) True cond

getFormatStringPattern1 :: [(Int, Int)]
getFormatStringPattern1 = goRight 0 8 (>8) <> goUp 8 7 (<0)
  where
    goRight :: Int -> Int -> (Int -> Bool) -> [(Int, Int)]
    goRight i j cond =
      if cond i then []
      else if i == 6 then goRight (i+1) j cond else (i,j): goRight (i+1) j cond
    
    goUp :: Int -> Int -> (Int -> Bool) -> [(Int, Int)]
    goUp i j cond =
      if cond j then []
      else if j == 6 then goUp i (j-1) cond else (i,j): goUp i (j-1) cond

getFormatStringPattern2 :: [(Int, Int)]
getFormatStringPattern2 = leftSide 8 20 (<14) <> rightSide 13 8 (>20)
  where
    leftSide :: Int -> Int -> (Int -> Bool) -> [(Int, Int)]
    leftSide i j cond =
      if cond j then []
      else (i,j):leftSide i (j-1) cond
    
    rightSide :: Int -> Int -> (Int -> Bool) -> [(Int, Int)]
    rightSide i j cond =
      if cond i then []
      else (i,j):rightSide (i + 1) j cond

