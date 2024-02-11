{-# LANGUAGE BinaryLiterals #-}
module HaskQR.Encoder.Internal where

import Data.ByteString as BS
import Data.List.Split as LS
import Data.Vector.Split as VS

import Data.Bit as Bit
import Data.Vector.Unboxed as V

import Data.Binary.Put
import Data.Word

import HaskQR.Data

getMessageBitStream :: Input -> Mode -> Version -> [Codeword]
getMessageBitStream input mode version =  VS.chunksOf 8 (dataBitStream <> paddingBits)
    where
        dataBitStream :: Vector Bit
        dataBitStream = getDataBitStream input mode version

        -- only single mode for now: assert segment length according to eq p.26
        paddingBits :: Vector Bit
        paddingBits = V.replicate paddingLength 0
            where
                paddingLength = (8 - V.length dataBitStream `mod` 8) `mod` 8 -- second mod to convert 8 to 0

getDataBitStream :: Input -> Mode -> Version -> Vector Bit
getDataBitStream input mode version = segment <> terminator
    where
        segment :: Vector Bit
        segment = getSegmentBits input mode version

        terminator :: Vector Bit
        terminator = V.replicate times 0
            where
                times = min (capacity - segmentLength) 4
                capacity = 8 * numOfDataCodewords version
                segmentLength = V.length segment

getSegmentBits :: Input -> Mode -> Version -> Vector Bit
getSegmentBits input mode version = modeIndicator <> charCountIndicator <> encodedInputData 
    where
        -- first four bits in the data segment - Table 2
        modeIndicator :: Vector Bit 
        modeIndicator = getNBitVector 4 modeVal
            where
                modeVal = case mode of
                    NumericMode  -> 0b0001
                    AlphaNumMode -> 0b0010

        -- character count indicator; length depends on Version number
        charCountIndicator :: Vector Bit
        charCountIndicator = let
            ln = fromIntegral $ Prelude.length input
            cciLength = charCountIndicatorLength mode version 
            in 
                getNBitVector cciLength ln

        encodedInputData :: Vector Bit
        encodedInputData = case mode of
            NumericMode  -> encodeNumericMode input 
            AlphaNumMode -> encodeAlphaNumMode input

encodeNumericMode :: Input -> Vector Bit
encodeNumericMode input = let
    chunkedInput :: [String] 
    chunkedInput = LS.chunksOf 3 input
    
    elementLength :: String -> Int
    elementLength str = 1 + 3 * Prelude.length str

    convertStrToWord32 :: String -> Word32
    convertStrToWord32 = read
    in
        -- convert each number string to a 4/7/10 bit binary number (see section 7.4.3)
        V.concat $ fmap (\el -> getNBitVector (elementLength el) (convertStrToWord32 el)) chunkedInput

encodeAlphaNumMode :: Input -> Vector Bit
encodeAlphaNumMode = V.concat . convertToBitValue . convertToCharacterValue
    where
        convertToCharacterValue :: Input -> [Int]
        convertToCharacterValue = fmap toCharValue
          where
            -- todo: don't use case statement
            toCharValue :: Char -> Int
            toCharValue c = case c of
                '0' -> 0
                '1' -> 1
                '2' -> 2
                '3' -> 3
                '4' -> 4
                '5' -> 5
                '6' -> 6
                '7' -> 7
                '8' -> 8
                '9' -> 9
                'A' -> 10
                'B' -> 11
                'C' -> 12
                '-' -> 41

        convertToBitValue :: [Int] -> [Vector Bit]
        convertToBitValue charValList = convertChunkToBits <$> LS.chunksOf 2 charValList
          where
            convertChunkToBits :: [Int] -> Vector Bit
            convertChunkToBits [dv1, dv2] = getNBitVector 11 $ convertToDecimal (dv1, dv2)
            convertChunkToBits [dv] = getNBitVector 6 $ convertToDecimal (0, dv)
            
            convertToDecimal :: (Int, Int) -> Word32
            convertToDecimal (v1,v2) = fromIntegral $ v1 * 45 + v2

charCountIndicatorLength :: Mode -> Version -> Int
charCountIndicatorLength mode version = 10 + versionAdditive - modeSubtractive
  where 
    versionAdditive :: Int
    versionAdditive
      | v >= 1  && v <= 9 = 0
      | v >= 10 && v <= 26 = 2
      | v >= 27 && v <= 40 = 4
      where
        v = getVersionNumber version

    modeSubtractive :: Int
    modeSubtractive = case mode of
        NumericMode  -> 0
        AlphaNumMode -> 1

-- utility function to convert a number (in word32) into a varied length binary vector
getNBitVector :: Int -> Word32 -> Vector Bit
getNBitVector n = V.drop (32 - n) . V.reverse . cloneFromByteString . BS.toStrict . runPut . putWord32le

-- from table 1
numOfDataCodewords :: Version -> Int
numOfDataCodewords version = case v of
    1 -> 19 -- That's for errer correction Level L
    where
        v = getVersionNumber version