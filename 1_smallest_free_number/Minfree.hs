{-# LANGUAGE NoImplicitPrelude, ScopedTypeVariables #-}

module Main where

import qualified Test.Tasty as Test
import qualified Test.Tasty.HUnit as Test
import qualified Test.Tasty.QuickCheck as Test

import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Array
import Data.Array.ST
import Data.Bool
import Data.Eq
import Data.Function
import Data.Int
import qualified Data.IntSet as IntSet
import Data.List
import Data.Maybe
import Data.Ord
import Data.Traversable
import Data.Tuple
import Prelude (Num (..), div, undefined)
import System.IO
import System.Random
import Text.Show
import GHC.IO.Encoding
import Debug.Trace

import qualified Criterion.Main as Crit

naiveMinfree :: [Int] -> Int
naiveMinfree xs = head ([0 ..] \\ xs)

arrayMinfree :: [Int] -> Int
arrayMinfree = search . checklist
  where
    search :: Array Int Bool -> Int
    search = length . takeWhile id . elems
    checklist :: [Int] -> Array Int Bool
    checklist xs = accumArray (||) False (0, n) (zip (filter (<= n) xs) (repeat True)) where n = length xs

countlistMinfree :: [Int] -> Int
countlistMinfree = search . countlist
  where
    search :: Array Int Int -> Int
    search = length . takeWhile (> 0) . elems
    countlist :: [Int] -> Array Int Int
    countlist [] = array (0, -1) []
    countlist xs = accumArray (+) 0 (0, n) (zip xs (repeat 1)) where n = maximum xs

arrayStMinfree :: [Int] -> Int
arrayStMinfree = search . checklist
  where
    search :: Array Int Bool -> Int
    search = length . takeWhile id . elems
    checklist :: [Int] -> Array Int Bool
    checklist xs = runSTArray $ do
                     a <- newArray (0, n) False
                     sequence_ [ writeArray a x True | x <- xs, x <= n ]
                     return a
                   where n = length xs
    
dacMinfree :: [Int] -> Int
dacMinfree xs' = minfrom 0 (length xs', xs')
  where
    minfrom a (n, xs) | n == 0     = a
                      | m == b - a = minfrom b (n - m, vs)
                      | otherwise  = minfrom a (m, us)
                      where (us, vs) = partition (< b) xs
                            b = a + 1 + (n `div` 2)
                            m = length us
    
mainTest :: IO ()
mainTest = Test.defaultMain $ Test.localOption (Test.QuickCheckTests 10000) tests

mainDac :: IO Int
mainDac = do
  let seq1 = take 500 $ nub $  randomRs (0, 700) (mkStdGen 1)
  return $ dacMinfree seq1

tests :: Test.TestTree
tests =
  Test.testGroup "all tests" [
  
    Test.testCase "naive naiveMinfree with testSeq" $ do
      Test.assertEqual "" 15 (naiveMinfree testSeq)
  
  , Test.testProperty "naive naiveMinfree by property"
      $ \seq' ->
        let seq = map Test.getNonNegative seq'
            foundMin = naiveMinfree seq
        in (`elem` seq) `all` [0 .. foundMin - 1] && (not $ elem foundMin seq)
  
  , Test.testProperty "arrayMinfree holds property"
      $ map Test.getNonNegative |> naiveMinfree === arrayMinfree
  
  , Test.testProperty "countlistMinfree holds property"
      $ map Test.getNonNegative |> naiveMinfree === countlistMinfree
  
  , Test.testProperty "arrayStMinfree holds property"
      $ map Test.getNonNegative |> naiveMinfree === arrayStMinfree
      
  , Test.testProperty "dacMinfree holds property"
      $ getNonRepeatingInt |> naiveMinfree === dacMinfree
  
  ]
  where
    (===) = liftA2 (==)
    infixr 8 |>
    a |> b = b . a
    
newtype NonRepeatingInt = NonRepeatingInt { getNonRepeatingInt :: [Int] } deriving (Show)

instance Test.Arbitrary NonRepeatingInt where
  arbitrary = Test.sized $ \n ->
    do k <- Test.choose (0, n)
       NonRepeatingInt . snd <$> mapAccumM (const . arbitraryNotIn) IntSet.empty [1 .. k]
    where 
      arbitraryNotIn st =
        Test.arbitrary >>= (\x ->
          if IntSet.member x st
          then arbitraryNotIn st
          else return (IntSet.insert x st, x)) . Test.getNonNegative
  
mapAccumM :: (Monad m, Traversable t) => (a -> b -> m (a, c)) -> a -> t b -> m (a, t c)
mapAccumM f s t = swap <$> runStateT (traverse (mapStateT (>>= g) . return) t) s
  where
    g = liftA swap . uncurry (flip f)
  
testSeq :: [Int]
testSeq = [ 8, 23, 09, 00, 12, 11, 01, 10, 13, 07, 41, 04, 14, 21, 05, 17, 03, 19, 02, 06 ]

main :: IO ()
main = do
  setLocaleEncoding utf8
  seq1 <- return $ force $ take 10 $ unfoldr (Just . splitAt 100) $ nub $ randomRs (0, 110) (mkStdGen 1)
  seq3 <- return $ force $ take 10 $ unfoldr (Just . splitAt 500) $ nub $ randomRs (0, 520) (mkStdGen 1)
  seq5 <- return $ force $ [[99, 98 .. 0], [499, 498 .. 0]]
  Crit.defaultMain [
      Crit.bgroup "" [
        Crit.bench "naive"   $ Crit.whnf (map naiveMinfree   ) seq1
      , Crit.bench "naive"   $ Crit.whnf (map naiveMinfree   ) seq3
      , Crit.bench "naive"   $ Crit.whnf (map naiveMinfree   ) seq5
      , Crit.bench "array"   $ Crit.whnf (map arrayMinfree   ) seq1
      , Crit.bench "array"   $ Crit.whnf (map arrayMinfree   ) seq3
      , Crit.bench "array"   $ Crit.whnf (map arrayMinfree   ) seq5
      , Crit.bench "arraySt" $ Crit.whnf (map arrayStMinfree ) seq1
      , Crit.bench "arraySt" $ Crit.whnf (map arrayStMinfree ) seq3
      , Crit.bench "arraySt" $ Crit.whnf (map arrayStMinfree ) seq5
      , Crit.bench "dac"     $ Crit.whnf (map dacMinfree     ) seq1
      , Crit.bench "dac"     $ Crit.whnf (map dacMinfree     ) seq3
      , Crit.bench "dac"     $ Crit.whnf (map dacMinfree     ) seq5
      ]
    ]