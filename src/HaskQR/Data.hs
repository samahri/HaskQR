module HaskQR.Data(
    QRCode(..), 
    Mode(..), 
    Version,
    ErrorCorrectionLevel(..),
    Input,
    Codeword,
    getVersionNumber, 
    mkVersion
) where

import Data.Bit

data QRCode = QRCode {
    inputData :: String
    , getMode :: Mode
    , codeWord :: [Codeword] 
    , getVersion :: Version
} deriving Show

data Mode = NumericMode deriving Show
data ErrorCorrectionLevel = L deriving Show

newtype Version = Version { getVersionNumber :: Int } deriving (Show, Eq, Ord)

type Codeword = Vector Bit -- a Codeword is a vector of 8 bits
type Input = String -- Input data

mkVersion :: Int -> Maybe Version
mkVersion v
    | v >= 1 && v <= 40 = Just $ Version v
    | otherwise = Nothing