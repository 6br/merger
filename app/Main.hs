{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeInType            #-}

module Main where

--import Lib
import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.ByteString                 as BS
--import qualified Data.Text        as T
--import qualified Data.Text.IO     as T
import           Blaze.ByteString.Builder        as B
import           Control.Applicative
import           Control.Monad
import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy.Internal   as BL
import qualified Data.IntervalMap                as IvMap
import           Data.IntervalMap.Generic.Strict
import qualified Data.IntervalMap.Interval       as IV
import qualified Data.Map                        as M
import qualified Data.Text                       as T
import qualified Data.Text.Encoding              as TE
import           Text.RawString.QQ
import Text.Printf

type Index = Int
--type ArraySpan = (Index, Index)
type ArraySpan = Interval Index
type RawData = BS.ByteString
--type Candidates = IntervalMap (IV.Interval Index) [RawData]
type Candidates = IntervalMap (IV.Interval Index) RawData

instance Ord e => Interval (e,e) e where
   lowerBound (a,_) = a
   upperBound (_,b) = b
   rightClosed _ = False

noCandidates :: Candidates
noCandidates = Data.IntervalMap.Generic.Strict.empty

mkArraySpan :: Index -> Index -> IV.Interval Index
mkArraySpan = IV.ClosedInterval

addCandidate :: Index -> Index -> RawData -> Candidates -> Candidates
addCandidate from to what = insertWith chooseLonger (mkArraySpan from to) what

addParitalData :: PartialData -> Candidates -> Candidates
addParitalData partial = insertWith chooseLonger (mkArraySpan (arrayFrom partial) (arrayTo partial)) (TE.encodeUtf8 $ rawData partial)

chooseLonger :: BS.ByteString -> BS.ByteString -> BS.ByteString
chooseLonger x y | lenx > leny = x
                 | otherwise = y
                 where
                   lenx = BS.length x
                   leny = BS.length y

--flattenWith :: (v -> v -> v) -> IntervalMap k v -> IntervalMap k v
--flattenWith combine m = fromList (combineSuccessive combine m)

--flattenWith :: (Ord k) => (v -> v -> v) -> IntervalMap k v -> IntervalMap k v
{-}flattenWith f = flattenWithMonotonic f'
  where
    f' (k1,v1) (k2,v2) = case IV.combine k1 k2 of
                           Nothing -> Nothing
                           Just k' -> let v' = f v1 v2 in v' `seq` Just (k', v')

--flattenWithMonotonic :: (Interval k e) => ((k,v) -> (k,v) -> Maybe (k,v)) -> IntervalMap k v -> IntervalMap k v
--flattenWithMonotonic combine m = fromDistinctAscList (combineSuccessive combine m)
-}
priolizedLonger :: Candidates -> Candidates
priolizedLonger x = IvMap.flattenWith chooseLonger x

--lambda :: Maybe (IvMap.Interval a) -> Maybe (IvMap.Interval a) -> Maybe (IvMap.Interval a)
--lambda :: Maybe (IvMap.Interval a) -> Maybe (IvMap.Interval a) -> Maybe (IvMap.Interval a)
--lambda :: [RawData] -> [RawData] -> [RawData]
--lambda :: [ByteString] -> [ByteString] -> [ByteString]
--lambda :: ByteString -> ByteString -> ByteString
--lambda = chooseLonger

--combineSuccessive :: ((k,v) -> (k,v) -> Maybe (k,v)) -> IntervalMap k v -> [(k,v)]
combineSuccessive combine m = go (toAscList m)
  where
    go (x : xs@(y:ys)) = case combine x y of
                           Nothing -> x : go xs
                           Just x' -> go (x' : ys)
    go xs = xs

sampleApps :: Candidates
sampleApps = addCandidate 1 2 "Dentist" $
              addCandidate 1 3 "Meeting2" $
              addCandidate 4 5 "Shopping" $
              noCandidates

data PartialData = PartialData { dataId :: Int, blockId :: Int, arrayFrom :: Index, arrayTo :: Index, rawData :: T.Text, mergeId :: Int } deriving Show

$(deriveJSON defaultOptions ''PartialData)

jsonEx2 :: String
jsonEx2 = [r|{"dataId": 0,"blockId": 15,"arrayFrom": 160003550,"arrayTo": 160010000,"mergeId": 16,"rawData": "# start gene chr1.g288\nchr1\tAUGUSTUS\tgene\t160003550\t160010000\t1\t-\t.\tchr1.g288\nchr1\tAUGUSTUS\ttranscript\t160003550\t160010000\t.\t-\t.\tchr1.g288.t1\nchr1\tAUGUSTUS\ttts\t160003550\t160003550\t.\t-\t.\ttranscript_id \"chr1.g288.t1\"; gene_id \"chr1.g288\";\nchr1\tAUGUSTUS\texon\t160003550\t160008630\t.\t-\t.\ttranscript_id \"chr1.g288.t1\"; gene_id \"chr1.g288\";\nchr1\tAUGUSTUS\tstop_codon\t160008574\t160008576\t.\t-\t0\ttranscript_id \"chr1.g288.t1\"; gene_id \"chr1.g288\";\nchr1\tAUGUSTUS\tCDS\t160008574\t160008630\t.\t-\t0\ttranscript_id \"chr1.g288.t1\"; gene_id \"chr1.g288\";\n# protein sequence = [RYDLMEENLPIDLTKMLS]\n# end gene"}
{"dataId": 0,"blockId": 15,"arrayFrom": 160003550,"arrayTo": 160010000,"mergeId": 16,"rawData": "# start gene chr1.g288\nchr1\tAUGUSTUS\tgene\t160003550\t160010000\t1\t-\t.\tchr1.g288\nchr1\tAUGUSTUS\ttranscript\t160003550\t160010000\t.\t-\t.\tchr1.g288.t1\nchr1\tAUGUSTUS\ttts\t160003550\t160003550\t.\t-\t.\ttranscript_id \"chr1.g288.t1\"; gene_id \"chr1.g288\";\nchr1\tAUGUSTUS\texon\t160003550\t160008630\t.\t-\t.\ttranscript_id \"chr1.g288.t1\"; gene_id \"chr1.g288\";\nchr1\tAUGUSTUS\tstop_codon\t160008574\t160008576\t.\t-\t0\ttranscript_id \"chr1.g288.t1\"; gene_id \"chr1.g288\";\nchr1\tAUGUSTUS\tCDS\t160008574\t160008630\t.\t-\t0\ttranscript_id \"chr1.g288.t1\"; gene_id \"chr1.g288\";\n# protein sequence = [RYDLMEENLPIDLTKMLS]\n# end gene"}
|]

{-
mainBS :: IO ()
mainBS = do
  inp <- BS.getContents
  let bind = (show . decodeJson) <$> Data.Text.lines inp
  mapM_ putStrLn bind
-}

main2 :: IO ()
main2 = do
  inp <- getLine
  print $ stringToPartialData inp

main3 :: IO ()
main3 = do
  inp <- readFile "result_hg38_chr1_error2_grepped.txt"
  let bind = (show . stringToPartialData) <$> lines inp
  mapM_ putStrLn bind

main4 :: IO ()
main4 = do
  inp <- getContents -- Use Data.Text
  --let ls = lines inp
  --let ls2 = fmap (toLazyByteString . stringUtf8) $ lines inp
  --let maybePartialData :: [Maybe PartialData] = fmap decodeJson ls2
  --let bind :: [String] = fmap show maybePartialData
  let bind = (show . stringToPartialData) <$> lines inp
  mapM_ putStrLn bind

stringToPartialData :: String -> Maybe PartialData
stringToPartialData =
  decodeJson . toLazyByteString . stringUtf8

--convertLazyBiteFromContents :: String -> [ByteString]
convertLazyBiteFromContents x =
  (toLazyByteString . stringUtf8) <$> lines x

--decodeJson :: Data.ByteString.Lazy.Internal.ByteString -> Maybe PartialData
decodeJson = decode

pDataToIvMap :: [PartialData] -> Candidates
pDataToIvMap = Prelude.foldr addParitalData noCandidates

liftPartial :: [Maybe PartialData] -> Maybe [PartialData]
liftPartial = sequence

exampleToCandidates :: Maybe Candidates
exampleToCandidates = fmap pDataToIvMap $ liftPartial $ stringToPartialData <$> lines jsonEx2

resultTest :: IO Candidates
resultTest = do
  inp <- readFile "result_hg38_chr1_error2_grepped.txt"
  let bind = liftPartial $ stringToPartialData <$> lines inp
  --let bind2 = liftPartial $ bind
  case bind of
    Nothing -> return noCandidates
    Just x  -> return $ priolizedLonger $ pDataToIvMap x

main = do
  inp <- resultTest
  putAsString inp

candidatesShow :: Candidates -> [RawData]
candidatesShow x = snd <$> toAscList x

candidatesFst :: Candidates -> [IvMap.Interval Index]
candidatesFst x = fst <$> toAscList x

--convertString x =  T.unlines $ TE.decodeUtf8 <$> candidatesShow x

printAsString :: Candidates -> IO ()
printAsString x = mapM_ print $ candidatesShow x

putAsString :: Candidates -> IO ()
putAsString x = mapM_ BS.putStrLn $ candidatesShow x

putRegion :: Candidates -> IO ()
putRegion x = mapM_ print $ candidatesFst x

pmap =  mapM_ printAsString exampleToCandidates

