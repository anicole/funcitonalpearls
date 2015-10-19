{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}

import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Control.Applicative
import           Control.Exception         (evaluate)
import           Control.Monad
import           Control.Monad.Trans.State
import           Data.Array.IArray
import           Data.Array.Unboxed
import qualified Data.IntSet               as IntSet
import           Data.List                 hiding (union)
import           Data.Traversable
import           Data.Tuple

import           Control.DeepSeq
import qualified Criterion.Main            as Crit
import           GHC.IO.Encoding
import           System.Random

basicSmallest :: (Ord a) => Int -> ([a],[a]) -> a
basicSmallest k (xs,ys) = union (xs,ys) !! k

union :: (Ord a) => ([a],[a]) -> [a]
union (xs,[]) = xs
union ([],ys) = ys
union (x:xs,y:ys)
  | x < y     = x:union (xs, y:ys)
  | otherwise = y:union (x:xs, ys)

dncSmallest :: (Ord a) => Int -> ([a],[a]) -> a
dncSmallest k ([],bs) = bs !! k
dncSmallest k (as,[]) = as !! k
dncSmallest k (as,bs)
  | a < b && k <= n   = dncSmallest k       (as, us)
  | a < b && k  > n   = dncSmallest (k - m) (ys, bs)
  | a > b && k <= n   = dncSmallest k       (xs, bs)
  | a > b && k  > n   = dncSmallest (k - o) (as, vs)
  where
    (xs,a:ys) = splitAt (length as `div` 2) as
    (us,b:vs) = splitAt (length bs `div` 2) bs
    n = length (xs ++ us)
    m = length (a:xs)
    o = length (b:us)

arrSmallest :: (IArray a Int) => Int -> (a Int Int, a Int Int) -> Int
arrSmallest k (ar,br) =
  arrSmallest' k (bounds ar) (bounds br)
  where
    arrSmallest' j (arb,are) (brb, bre)
      | are < arb = br ! (brb + j)
      | bre < brb = ar ! (arb + j)
      | a < b && j <= n  = arrSmallest' j                  (arb, are)    (brb, bi - 1)
      | a < b && j  > n  = arrSmallest' (j - ai + arb - 1) (ai + 1, are) (brb, bre)
      | a > b && j <= n  = arrSmallest' j                  (arb, ai - 1) (brb, bre)
      | a > b && j  > n  = arrSmallest' (j - bi + brb - 1) (arb, are)    (bi + 1, bre)
      where
        a  = ar ! ai
        b  = br ! bi
        ai = (arb + are) `div` 2
        bi = (brb + bre) `div` 2
        n  = ai - arb + bi - brb

testValid :: IO ()
testValid =
  defaultMain
  $ localOption (QuickCheckTests 10000)
  $ localOption (QuickCheckVerbose False)
  tests

tests :: TestTree
tests =
  testGroup "all"
  [
    testProperty "dncSmallest behaves same as basicSmallest"
      $ (\(k,p) -> basicSmallest k p == dncSmallest k p) . getTestPair

  , testProperty "arrSmallest behaves same as basicSmallest"
      $ \tp ->
          let (k,(a,b)) = getTestPair tp
              arrA  = listArray (1, length a) a :: UArray Int Int
              arrB  = listArray (1, length b) b :: UArray Int Int
          in
              basicSmallest k (a,b) == arrSmallest k (arrA, arrB)
  ]

newtype TestPair = TestPair { getTestPair :: (Int, ([Int],[Int])) } deriving (Show)

instance Arbitrary TestPair where
  arbitrary = do
      xss <- flip suchThat ((> 0) . length) $ getNonRepeatingInt <$> arbitrary
      prs <- pairwise sort <$> flip splitAt xss <$> choose (0, length xss - 1)
      k   <- choose (0, length xss - 1)
      return $ TestPair (k, prs)
    where
      pairwise f (a,b) = (f a, f b)

newtype NonRepeatingInt = NonRepeatingInt { getNonRepeatingInt :: [Int] } deriving (Show)

instance Arbitrary NonRepeatingInt where
  arbitrary = sized $ \n ->
    do k <- choose (0, n)
       NonRepeatingInt . snd <$> mapAccumM (const . arbitraryNotIn) IntSet.empty [1 .. k]
    where
      arbitraryNotIn st =
        arbitrary >>= (\x ->
          if IntSet.member x st
          then arbitraryNotIn st
          else return (IntSet.insert x st, x)) . getNonNegative

mapAccumM :: (Applicative m, Functor m, Monad m, Traversable t) => (a -> b -> m (a, c)) -> a -> t b -> m (a, t c)
mapAccumM f s t = swap <$> runStateT (traverse (mapStateT (>>= g) . return) t) s
  where
    g = liftA swap . uncurry (flip f)

main :: IO ()
main = do
  setLocaleEncoding utf8
  (k1,p1) <- return $ force (750, ([0,2..999], [1,3..999]))
  ap1     <- evaluate ( listArray (1, length $ fst p1) (fst p1) :: UArray Int Int
                      , listArray (1, length $ snd p1) (snd p1) :: UArray Int Int
                      )
  Crit.defaultMain [
    Crit.bgroup "" [
        Crit.bench "basic"  $ Crit.whnf (uncurry basicSmallest) (k1, p1)
      , Crit.bench "dnc"    $ Crit.whnf (uncurry dncSmallest  ) (k1, p1)
      , Crit.bench "array"  $ Crit.whnf (uncurry arrSmallest  ) (k1, ap1)
      ]
    ]

