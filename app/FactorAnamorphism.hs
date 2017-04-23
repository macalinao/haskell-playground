{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}

module Main where

import Text.Show.Deriving
import Data.Functor.Foldable
import Data.List

nextPrimeFactor :: Integer -> Maybe Integer
nextPrimeFactor 2 = Nothing
nextPrimeFactor n = find (\x -> n `mod` x == 0) [2..(floor $ sqrt $ fromIntegral n)]

data ExprF r = FactorF Integer | MultF r r deriving (Show, Functor)
type Expr = Fix ExprF

deriveShow1 ''ExprF

factor :: Integer -> Expr
factor = ana coAlg where
  coAlg fac = case (nextPrimeFactor fac) of
    Just prime -> MultF prime (fac `div` prime)
    Nothing    -> FactorF fac

main :: IO ()
main = print $ factor 159
