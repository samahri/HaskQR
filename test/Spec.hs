import Test.Tasty
import Test.HaskQR.Encoder(qrEncoderTest)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "QR Test" [qrEncoderTest]