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
getSegmentBits input mode version = getModeIndicator mode <> getCharCountIndicator input version <> encodeInputData input mode
    where
        -- first four bits in the data segment
        getModeIndicator :: Mode -> Vector Bit 
        getModeIndicator mode = getNBitVector 4 modeVal
            where
                modeVal = case mode of
                    NumericMode -> 0b0001
        -- character count indicator; length depends on Version number
        getCharCountIndicator :: Input -> Version -> Vector Bit
        getCharCountIndicator input version = let
            ln = fromIntegral $ Prelude.length input
            cciLength = charCountIndicatorLength version
            in 
                getNBitVector cciLength ln

        encodeInputData :: Input -> Mode -> Vector Bit
        encodeInputData input mode = case mode of
            NumericMode -> encodeNumericMode input 

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

charCountIndicatorLength :: Version -> Int
charCountIndicatorLength version 
    | v >= 1  && v <= 9 = 10
    | v >= 10 && v <= 26 = 12
    | v >= 27 && v <= 40 = 14
    where
        v = getVersionNumber version

-- utility function to convert a number (in word32) into a varied length binary vector
getNBitVector :: Int -> Word32 -> Vector Bit
getNBitVector n = V.drop (32 - n) . V.reverse . cloneFromByteString . BS.toStrict . runPut . putWord32le

-- from table 1
numOfDataCodewords :: Version -> Int
numOfDataCodewords version = case v of
    1 -> 19 -- That's for errer correction Level L
    where
        v = getVersionNumber version