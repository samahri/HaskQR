{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module Main where

import HaskQR.Display
import HaskQR.Encoder
import System.Environment 

import HaskQR.Data
import HaskQR.ErrorCorrection

main :: IO ()
main = do
    args <- getArgs 
    if length args /= 1 then
        putStrLn "invalid input"
    else do
        let inputStr = head args
        -- assert $ length inputStr <= 41
        let mode = NumericMode
        let version = case mkVersion 1 of -- version is decided by segment length
                Just v  -> v 
                Nothing -> error "invalid verison number"
        let codeword = getCodeword inputStr mode version
        let errorCodeWord = getErrorCodeword version codeword 
        let qrObject = QRCode { inputData = inputStr, getMode = NumericMode, codeWord = codeword ++ errorCodeWord, getVersion = version}
        print (mconcat $ codeword ++ errorCodeWord)
        displayCode (codeword ++ errorCodeWord) version
        
--testCode :: Vector Bit
-- testCode = [[0,1,1,0,0,1,1,0,0,1,1,0],[0,1,1,0,0,1,1,0,0,1,1,0]
--   ,[1,0,0,1,1,0,0,1,1,0,0,1],[1,0,0,1,1,0,0,1,1,0,0,1]
--   ,[0,1,1,0,0,1,1,0,0,1,1,0],[0,1,1,0,0,1,1,0,0,1,1,0]
--   ,[1,0,0,1,1,0,0,1,1,0,0,1],[1,0,0,1,1,0,0,1,1,0,0,1]
--   ,[0,1,1,0,0,1,1,0,0,1,1,0],[0,1,1,0,0,1,1,0,0,1,1,0], [0,1,1,0] ,[0,1,1,0, 0,1,1,0, 0,1,1,0]
--   ,[1,0,0,1, 1,0,0,1, 1,0,0,1], [1,0,0,1], [1,0,0,1,1,0,0,1,1,0,0,1]--,[1,0,0,1,1,0,0,1,1,0,0,1]
--   ,[0,1,1,0,0,1,1,0]
--   ,[0,0,0,0, 0,0,0,0]
--   ,[0,0,0,0, 0,0,0,0]
--   ,[0,0,0,0, 0,0,0,0]
--  ]