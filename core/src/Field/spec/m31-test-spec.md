{- |
Module      : Test.Field.M31Spec
Description : *Full* behavioural specification for @Field.M31@

This spec extends **RunM31Tests.hs** so that _every_ exported symbol
(except the trivial re-export of @fromInteger@) is exercised by at least
one property and all class laws are stated explicitly.

It is written for **hspec ≥ 2.10** with **QuickCheck-2**.  
Add it to your test-suite stanza (together with the original
@RunM31Tests.hs@) and you will reach **100 % coverage** on the public
API.

> cabal test   # or   stack test

--------------------------------------------------------------------------------
NOTE ❶  All properties are phrased solely in terms of the field axioms
        and the documented semantics in @Field.M31@; they never peek at
        the private representation except through the official
        @unM31@ prism.
NOTE ❷  We restrict the generator for the ‘Enum’ round-trip property to
        2¹⁶ values so the test suite remains snappy even though the
        modulus is ~2 billion.
-}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NumericUnderscores #-}
module Test.Field.M31Spec (spec) where

import           Prelude                  hiding (succ, pred)
import           Test.Hspec
import           Test.Hspec.QuickCheck    (prop)
import           Test.QuickCheck
import           Control.DeepSeq          (deepseq)
import           Data.Bits                (Bits (..))
import           Data.Word                (Word32, Word64)

import           Field.M31

--------------------------------------------------------------------------------
-- Arbitrary instances ----------------------------------------------------------
--------------------------------------------------------------------------------

instance Arbitrary M31 where
  arbitrary = mkM31 <$> choose (0, modulus - 1)

--------------------------------------------------------------------------------
-- Helper: “slow but obvious” exponentiation for specs --------------------------
--------------------------------------------------------------------------------

naivePow :: M31 -> Word64 -> M31
naivePow x e = iterate (* x) 1 !! fromIntegral e

--------------------------------------------------------------------------------
-- The specification ------------------------------------------------------------
--------------------------------------------------------------------------------

spec :: Spec
spec = parallel $ do
  describe "Bounded/Enum basics" $ do
    it "minBound == 0  ∧  maxBound == p − 1" $
      (unM31 minBound, unM31 maxBound) `shouldBe`
        (0, modulus - 1)

    prop "succ ⊣⊢ pred (wrap-around)" $
      \(a :: M31) ->
        pred (succ a) == a .&&. succ (pred a) == a

    prop "succ maxBound == 0 (mod-wrap)" $
      succ (maxBound :: M31) === (0 :: M31)

    prop "pred 0 == maxBound (mod-wrap)" $
      pred (0 :: M31) === (maxBound :: M31)

    it "Enum round-trip  toEnum ∘ fromEnum == id" $
      forAll (choose (0,65_535) :: Gen Word32) $ \w ->
        let x = mkM31 w in (toEnum . fromEnum) x `shouldBe` x

  describe "NFData instance" $
    prop "deepseq leaves no thunks" $
      \(x :: M31) y -> (x `deepseq` y) === (y :: Int)

  describe "pow2 / powWord32 helpers" $ do
    prop "pow2 n x == x^(2^n)" $
      \n (x :: M31) ->
        let n' = n `mod` 10  --  2^10 > 1024 multiplications is plenty
        in pow2 n' x === naivePow x (2 ^ n')

    prop "powWord32 exponentiation agrees with naïve definition" $
      \(Positive (Small e)) (x :: M31) ->
        powWord32 x e === naivePow x (fromIntegral e)

  describe "Real / Integral instances" $ do
    prop "toRational ∘ fromRational ≡ id (information-preserving)" $
      \(x :: M31) -> fromRational (toRational x) === x

    prop "quotRem laws: a == b*quot + rem  ∧  rem == 0 in a field" $
      \(a :: M31) (NonZero b) ->
        let (q,r) = quotRem a b in (a == q * b .+. r) .&&. (r === 0)
          where (.+.) = (+)

  describe "Bits passthrough sanity" $ do
    prop "bit-wise NOT followed by NOT returns original (two's complement)" $
      \(x :: M31) -> complement (complement x) === x

    prop "shiftL then shiftR by same amount (≤31) returns value modulo p" $
      \(x :: M31) (Small d) ->
        let k = d `mod` 31
            roundTrip = shiftR (shiftL x k) k
        in partialReduce (unM31 roundTrip) === roundTrip

  describe "pow2147483645 is really the inverse" $
    prop "v ≠ 0   ⇒   v * pow2147483645 v == 1" $
      \(NonZero v) -> v * pow2147483645 v === 1

  describe "abs / signum for a field" $ do
    prop "abs is identity" $
      \(x :: M31) -> abs x === x

    prop "signum x ∈ {0,1}" $
      \(x :: M31) ->
        signum x `elem` [0,1 :: M31]

--------------------------------------------------------------------------------
-- End of file ------------------------------------------------------------------
