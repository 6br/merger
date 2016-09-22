{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Lib
import Data.Aeson
import Data.Aeson.TH
import Data.ByteString.Builder
--import qualified Data.ByteString as BS
import qualified Data.IntervalMap as IM

data PartialData = PartialData { dataId :: Int, blockId :: Int, arrayFrom :: Int, arrayTo :: Int, rawData :: String, mergeId :: Int } deriving Show

$(deriveJSON defaultOptions ''PartialData)

main :: IO ()
main = do
  inp <- getContents
  let ls = lines inp
  let ls2 = fmap (toLazyByteString . stringUtf8) ls
  let maybePartialData = mapM_ decode ls2
  mapM_ putStrLn maybePartialData

