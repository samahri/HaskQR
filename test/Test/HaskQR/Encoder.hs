module Test.HaskQR.Encoder(qrEncoderTest) where

import Test.Tasty
import Test.Tasty.HUnit

import Test.HaskQR.Encoder.Internal(qrEncoderInternalTest)

qrEncoderTest :: TestTree
qrEncoderTest = testGroup "QR Encoder Test" [qrEncoderInternalTest]

