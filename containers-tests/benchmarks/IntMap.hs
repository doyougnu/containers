{-# LANGUAGE BangPatterns #-}
module Main where

import Control.DeepSeq (rnf)
import Control.Exception (evaluate)
import Gauge (bench, defaultMain, whnf, nf)
import qualified System.Random as R
import Data.List (foldl',unfoldr)
import qualified Data.IntMap as M
import qualified Data.IntMap.Strict as MS
import Data.Maybe (fromMaybe)
import Prelude hiding (lookup)

main = do
    let m  = M.fromAscList elems :: M.IntMap Int
    keysR    <- R.randoms <$> R.newStdGen
    let rands  n = take n keysR
        elemsR n = zip (rands n) values
        mR       = M.fromAscList . elemsR
        [_100k,_1Mil,_10Mil,_1Bil] = [100000, 1000000, 10000000, 1000000000]
        [mR100k,mR1Mil,mR10Mil,mR1Bil] = mR <$> [_100k,_1Mil,_10Mil,_1Bil]
    evaluate $ rnf [m]
    mapM_ (evaluate . rnf) [mR100k,mR1Mil,mR10Mil,mR1Bil]
    defaultMain
        [ bench "lookup"                $ whnf (lookup keys) m
        , bench "lookupNat"             $ whnf (lookupNat keys) m
        , bench "lookup Random Keys 100k"     $ nf (lookup    (take _100k keysR)) mR100k
        , bench "lookupNat Random Keys 100k"  $ nf (lookupNat (take _100k keysR)) mR100k
        , bench "lookup Random Keys 1Mil"    $ nf (lookup    (take _1Mil keysR)) mR1Mil
        , bench "lookupNat Random Keys 1Mil" $ nf (lookupNat (take _1Mil keysR)) mR1Mil
        , bench "lookup Random Keys 10Mil"    $ nf (lookup    (take _10Mil keysR)) mR10Mil
        , bench "lookupNat Random Keys 10Mil" $ nf (lookupNat (take _10Mil keysR)) mR10Mil
        , bench "lookup Random Keys 1Bil"    $ nf (lookup    (take _1Bil keysR)) mR1Bil
        , bench "lookupNat Random Keys 1Bil" $ nf (lookupNat (take _1Bil keysR)) mR1Bil
        -- , bench "insert" $ whnf (ins elems) M.empty
        -- , bench "insertWith empty" $ whnf (insWith elems) M.empty
        -- , bench "insertWith update" $ whnf (insWith elems) m
        -- , bench "insertWith' empty" $ whnf (insWith' elems) M.empty
        -- , bench "insertWith' update" $ whnf (insWith' elems) m
        -- , bench "insertWithKey empty" $ whnf (insWithKey elems) M.empty
        -- , bench "insertWithKey update" $ whnf (insWithKey elems) m
        -- , bench "insertWithKey' empty" $ whnf (insWithKey' elems) M.empty
        -- , bench "insertWithKey' update" $ whnf (insWithKey' elems) m
        -- , bench "insertLookupWithKey empty" $ whnf (insLookupWithKey elems) M.empty
        -- , bench "insertLookupWithKey update" $ whnf (insLookupWithKey elems) m
        -- , bench "map" $ whnf (M.map (+ 1)) m
        -- , bench "mapWithKey" $ whnf (M.mapWithKey (+)) m
        -- , bench "foldlWithKey" $ whnf (ins elems) m
        -- , bench "foldlWithKey'" $ whnf (M.foldlWithKey' sum 0) m
        -- , bench "foldrWithKey" $ whnf (M.foldrWithKey consPair []) m
        -- , bench "delete" $ whnf (del keys) m
        -- , bench "update" $ whnf (upd keys) m
        -- , bench "updateLookupWithKey" $ whnf (upd' keys) m
        -- , bench "alter"  $ whnf (alt keys) m
        -- , bench "mapMaybe" $ whnf (M.mapMaybe maybeDel) m
        -- , bench "mapMaybeWithKey" $ whnf (M.mapMaybeWithKey (const maybeDel)) m
        -- , bench "fromList" $ whnf M.fromList elems
        -- , bench "fromAscList" $ whnf M.fromAscList elems
        -- , bench "fromDistinctAscList" $ whnf M.fromDistinctAscList elems
        -- , bench "minView" $ whnf (maybe 0 (\((k,v), m) -> k+v+M.size m) . M.minViewWithKey)
        --             (M.fromList $ zip [1..10] [1..10])
        ]
  where
    elems  = zip keys values
    keys   = [1..2^12]
    values = [1..2^12]
    sum k v1 v2     = k + v1 + v2
    consPair k v xs = (k, v) : xs


add3 :: Int -> Int -> Int -> Int
add3 x y z = x + y + z
{-# INLINE add3 #-}

lookup :: [Int] -> M.IntMap Int -> Int
lookup xs m = foldl' (\n k -> fromMaybe n (M.lookup k m)) 0 xs


lookupNat :: [Int] -> M.IntMap Int -> Int
lookupNat xs m = foldl' (\n k -> fromMaybe n (M.lookupNat k m)) 0 xs

ins :: [(Int, Int)] -> M.IntMap Int -> M.IntMap Int
ins xs m = foldl' (\m (k, v) -> M.insert k v m) m xs

insWith :: [(Int, Int)] -> M.IntMap Int -> M.IntMap Int
insWith xs m = foldl' (\m (k, v) -> M.insertWith (+) k v m) m xs

insWithKey :: [(Int, Int)] -> M.IntMap Int -> M.IntMap Int
insWithKey xs m = foldl' (\m (k, v) -> M.insertWithKey add3 k v m) m xs

insWith' :: [(Int, Int)] -> M.IntMap Int -> M.IntMap Int
insWith' xs m = foldl' (\m (k, v) -> MS.insertWith (+) k v m) m xs

insWithKey' :: [(Int, Int)] -> M.IntMap Int -> M.IntMap Int
insWithKey' xs m = foldl' (\m (k, v) -> MS.insertWithKey add3 k v m) m xs

data PairS a b = PS !a !b

insLookupWithKey :: [(Int, Int)] -> M.IntMap Int -> (Int, M.IntMap Int)
insLookupWithKey xs m = let !(PS a b) = foldl' f (PS 0 m) xs in (a, b)
  where
    f (PS n m) (k, v) = let !(n', m') = M.insertLookupWithKey add3 k v m
                        in PS (fromMaybe 0 n' + n) m'

del :: [Int] -> M.IntMap Int -> M.IntMap Int
del xs m = foldl' (\m k -> M.delete k m) m xs

upd :: [Int] -> M.IntMap Int -> M.IntMap Int
upd xs m = foldl' (\m k -> M.update Just k m) m xs

upd' :: [Int] -> M.IntMap Int -> M.IntMap Int
upd' xs m = foldl' (\m k -> snd $ M.updateLookupWithKey (\_ a -> Just a) k m) m xs

alt :: [Int] -> M.IntMap Int -> M.IntMap Int
alt xs m = foldl' (\m k -> M.alter id k m) m xs

maybeDel :: Int -> Maybe Int
maybeDel n | n `mod` 3 == 0 = Nothing
           | otherwise      = Just n
