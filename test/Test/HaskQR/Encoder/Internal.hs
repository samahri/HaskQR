{-# LANGUAGE OverloadedLists #-}
module Test.HaskQR.Encoder.Internal(qrEncoderInternalTest) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Bit

import HaskQR.Encoder.Internal
import HaskQR.Data

qrEncoderInternalTest :: TestTree
qrEncoderInternalTest = testGroup "QR Encoder Internal Test" [getSegmentBitsTest, getDataBitStreamTest]

getSegmentBitsTest :: TestTree
getSegmentBitsTest = testGroup "test getSegmentBits function" [encodeNumericMode1, encodeNumericMode2]

encodeNumericMode1 :: TestTree
encodeNumericMode1 = testCase "encode 01234567 using NumericMode" $ getSegmentBits "01234567" NumericMode v1 @?= zeroToSevenEncoded
    where
        zeroToSevenEncoded :: Vector Bit
        zeroToSevenEncoded = numericModeIndicator <> charCountIndicator <> encoded012 <> encoded345 <> encoded67 
            where
              charCountIndicator = [0,0,0,0,0,0,1,0,0,0]
              encoded012 = [0,0,0,0,0,0,1,1,0,0]
              encoded345 = [0,1,0,1,0,1,1,0,0,1]
              encoded67 = [1,0,0,0,0,1,1]

encodeNumericMode2 :: TestTree
encodeNumericMode2 = testCase "encode 111111 using NumericMode" $ getSegmentBits "111111" NumericMode v1 @?= allOnesEncoded
    where
        allOnesEncoded :: Vector Bit
        allOnesEncoded = numericModeIndicator <> charCountIndicator <> encoded111 <> encoded111
            where
                charCountIndicator = [0,0,0,0,0,0,0,1,1,0]
                encoded111 = [0,0,0,1,1,0,1,1,1,1] 

-- TODO: Tests with differnt version numbers

getDataBitStreamTest :: TestTree
getDataBitStreamTest = testGroup "Test getDataBitStream function" [terminatorPresentTest, terminatorPartiallyMaxTest]

terminatorPresentTest :: TestTree
terminatorPresentTest = testCase "encode 20241126 with terminator bits" $ getDataBitStream "20241126" NumericMode v1 @?= dataBitStream
    where
        dataBitStream :: Vector Bit
        dataBitStream = numericModeIndicator <> charCountIndicator <> encoded202 <> encoded411 <> encoded26 <> terminatorBits
            where
               charCountIndicator = [0,0,0,0,0,0,1,0,0,0]
               encoded202 = [0,0,1,1,0,0,1,0,1,0] 
               encoded411 = [0,1,1,0,0,1,1,0,1,1]
               encoded26 = [0,0,1,1,0,1,0]
               terminatorBits = [0,0,0,0]

terminatorPartiallyMaxTest :: TestTree -- 40 characters = 
terminatorPartiallyMaxTest = testCase "encode 999999999 999999999 999999999 999999999 999 9 with terminator bits" $ getDataBitStream "9999999999999999999999999999999999999999" NumericMode v1 @?= dataBitStream
    where
        dataBitStream :: Vector Bit
        dataBitStream = numericModeIndicator <> charCountIndicator <> encodedNine9s <> encodedNine9s <> encodedNine9s <> encodedNine9s <> encodedThree9s <> encoded9 <> terminatorBits
            where
               charCountIndicator = [0,0,0,0,1,0,1,0,0,0]
               encodedThree9s = [1,1,1,1,1,0,0,1,1,1] 
               encodedNine9s = encodedThree9s <> encodedThree9s <> encodedThree9s
               encoded9 = [1,0,0,1]
               terminatorBits = [0,0,0,0]

-- util functions
numericModeIndicator :: Vector Bit
numericModeIndicator = [0,0,0,1]

-- generalize this function with more versions
v1 :: Version
v1 = case mkVersion 1 of
    Just x -> x
    Nothing -> error "invalid verison number"