{-# LANGUAGE OverloadedLists #-}
module HaskQR.Encoder (getCodeword) where

import HaskQR.Data
import HaskQR.Encoder.Internal
import qualified Data.Map.Strict as Map 

getCodeword :: Input -> Mode -> Version -> [Codeword]
getCodeword input mode version = messageBitStream <> padCodewords (length messageBitStream)
    where
        messageBitStream :: [Codeword]
        messageBitStream = getMessageBitStream input mode version

        padCodewords :: Int -> [Codeword]
        padCodewords msgLength = take (numOfDataCodewords - msgLength) $ cycle [[1,1,1,0,1,1,0,0], [0,0,0,1,0,0,0,1]]
          where
            numOfDataCodewords = dataCodewordsMap Map.! version
        

