module HaskQR.ErrorCorrection where

import HaskQR.Data
import Data.Vector.Unboxed as V
import Data.Vector.Split as VS
import Data.Bit
import Data.Bits (shiftL, xor)
import Data.Word

getErrorCodeword :: Version -> [Codeword] -> [Codeword] -- consider using Word8
getErrorCodeword version dataCodeword = V.reverse <$> postProcess (fromIntegral <$> getCoefficients msgPolynomial genPolynomial)
   where
    msgPolynomial = convertToInt dataCodeword -- must be in decimal form
    genPolynomial = getGenPolynomial version
    postProcess = VS.chunksOf 8 . castFromWords8 . fromList

getCoefficients :: [Int] -> [Int] -> [Int] -- use [Word8]
getCoefficients message generator = performPolynomialDivision paddedMessage paddedGenerator (Prelude.length message)
    where
        -- don't pad for now
        paddedMessage = message -- message Prelude.++ Prelude.replicate (numberOfCorrectionCodewords version L) 0
        paddedGenerator = generator -- generator Prelude.++ Prelude.replicate (Prelude.length paddedMessage - Prelude.length generator) 0

performPolynomialDivision :: [Int] -> [Int] -> Int -> [Int]
performPolynomialDivision msg gen steps = go msg 0
    where
        go :: [Int] -> Int -> [Int]
        go msgPolynomial i
            | i < steps = go updatedMsg (i+1)
            | otherwise = msgPolynomial
            where
                updatedGen = fmap (fromGF256 . (`mod` 255) . (+ (toGF256 $ Prelude.head msgPolynomial))) gen
                updatedMsg = Prelude.tail $ Prelude.zipWith xor paddedMsg paddedGen
                    where
                        paddedMsg = msgPolynomial Prelude.++ Prelude.replicate (Prelude.length gen - Prelude.length msgPolynomial) 0
                        paddedGen = updatedGen Prelude.++ Prelude.replicate (Prelude.length msgPolynomial - Prelude.length gen) 0

convertToInt :: [Codeword] -> [Int] -- use Word8
convertToInt = fmap (bitToInt8 . V.indexed)
   where
    bitToInt8 = V.foldl' (\acc (i,b) -> (1 `shiftL` (7 - i)) * fromIntegral b + acc ) 0

-- these are the values of the a exponent (refer to table A1)
getGenPolynomial :: Version -> [Int] -- use Word8
getGenPolynomial version = case numberOfCorrectionCodewords version L of
    7  -> [0, 87, 229, 146, 149, 238, 102, 21] -- this is for 1-L ()
    10 -> [0, 251, 67, 46, 61, 118, 70, 64, 94, 32, 45]
    
-- convert a^n to an integer under GF(256) according to https://www.thonky.com/qr-code-tutorial/log-antilog-table
fromGF256 :: Int -> Int
fromGF256 n
    | n < 8 = 2^n
    | otherwise = doWhileLt255 $ 2 * fromGF256 (n - 1)
    where
        doWhileLt255 :: Int -> Int
        doWhileLt255 num = if num < 256 then num else doWhileLt255 $ num `xor` 285

-- convert an integer to a^n; it's the opposite of fromGF256 
toGF256 :: Int -> Int
toGF256 n
    | n == 0 = error "log of 0 is -Infinity"
    | n == 1 = 0
    | odd n = toGF256 (n `xor` 285)
    | otherwise = 1 + toGF256 (n `div` 2)

-- This is based on Table9
numberOfCorrectionCodewords :: Version -> ErrorCorrectionLevel -> Int
numberOfCorrectionCodewords version ecLevel = case (v, ecLevel) of
    (1, L) -> 7
    where
        v = getVersionNumber version