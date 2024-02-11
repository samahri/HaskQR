module HaskQR.Data(
    QRCode(..), 
    Mode(..), 
    Version,
    ErrorCorrectionLevel(..),
    Input,
    Codeword,
    getVersionNumber, 
    mkVersion,
    alphaNumEncodingMap 
) where

import Data.Bit
import qualified Data.Map.Strict as Map

data QRCode = QRCode {
    inputData :: String
    , getMode :: Mode
    , codeWord :: [Codeword] 
    , getVersion :: Version
} deriving Show

data Mode = NumericMode | AlphaNumMode deriving Show
data ErrorCorrectionLevel = L deriving Show

newtype Version = Version { getVersionNumber :: Int } deriving (Show, Eq, Ord)

type Codeword = Vector Bit -- a Codeword is a vector of 8 bits
type Input = String -- Input data

mkVersion :: Int -> Maybe Version
mkVersion v
    | v >= 1 && v <= 40 = Just $ Version v
    | otherwise = Nothing

alphaNumEncodingMap :: Map.Map Char Int
alphaNumEncodingMap = Map.fromList [
    ('0', 0),
    ('1' , 1),
    ('2' , 2),
    ('3' , 3),
    ('4' , 4),
    ('5' , 5),
    ('6' , 6),
    ('7' , 7),
    ('8' , 8),
    ('9' , 9),
    ('A' , 10),
    ('B' , 11),
    ('C' , 12),
    ('D' , 13),
    ('E' , 14),
    ('F' , 15),
    ('G' , 16),
    ('H' , 17),
    ('I' , 18),
    ('J' , 19),
    ('K' , 20),
    ('L' , 21),
    ('M' , 22),
    ('N' , 23),
    ('O' , 24),
    ('P' , 25),
    ('Q' , 26),
    ('R' , 27),
    ('S' , 28),
    ('T' , 29),
    ('U' , 30),
    ('V' , 31),
    ('W' , 32),
    ('X' , 33),
    ('Y' , 34),
    ('Z' , 35),
    (' ' , 36),
    ('$' , 37),
    ('%' , 38),
    ('*' , 39),
    ('+' , 40),
    ('-' , 41),
    ('.' , 42),
    ('/' , 43),
    (':' , 44)
    ]