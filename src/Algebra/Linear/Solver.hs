{-# OPTIONS_GHC -fno-warn-tabs #-}
module Algebra.Linear.Solver
	( solveHom
	, gaussHom
	, solve
	, idMat
	) where

import Data.List (transpose)
import qualified Algebra.Linear.Matrix as M

matWidth :: [[k]] -> Int
matWidth = length . transpose

matHeight :: [[k]] -> Int
matHeight = length

infixl 6 .+.
infixl 6 .-.
infixl 7 *.

class VectorSpace v where
	(*.)  :: Fractional a => a -> v a -> v a
	(.+.) :: Fractional a => v a -> v a -> v a
	basis :: Fractional a => [v a]
	one   :: Fractional a => v a
	zero  :: Fractional a => v a

type Kn = []

instance VectorSpace [] where
	k *. (x:xs) = k * x : (k *. xs)
	_ *. _ = []
	(x:xs) .+. (y:ys) = (x + y) : (xs .+. ys)
	[] .+. [] = []
	x .+. [] = x
	[] .+. y = y
	zero = repeat 0
	one = repeat 1
	basis = (1 : zero) : ((0:) <$> basis)

(.-.) :: (VectorSpace v, Fractional k) => v k -> v k -> v k
x .-. y = x .+. (-1) *. y

isZero :: (Eq (v k), Fractional k, VectorSpace v) => v k -> Bool
isZero v = zero == v

eliminateHead :: (Eq k, Fractional k) => [k] -> [k] -> [k]
eliminateHead _ [] = []
eliminateHead v w = w .-. (head w *. v)

normalizeHead :: (Eq k, Fractional k) => [k] -> [k]
normalizeHead x@(0:_) = x
normalizeHead (v:vs) = recip v *. (v:vs)
normalizeHead _ = []

eliminateRem :: (Eq k, Fractional k) => [[k]] -> [k] -> [k]
eliminateRem (u:us) v = eliminateRem us $ v .-. (v !! rang) *. u
	where
		rang = length . takeWhile (== 0) $ u
eliminateRem _ v = v

gaussHom :: (Eq k, Fractional k) => [[k]] -> [[k]]
gaussHom [] = []
gaussHom ([]:_) = [[]]
gaussHom [(0:vn)] = (0:) <$> gaussHom [vn]
gaussHom ks@((0:_):_) = case break ((==0) . length . takeWhile (==0)) ks of
	(_, [])  -> []
	(vs, ws) -> gaussHom (ws ++ vs)
gaussHom (k:ks)
	| isZero k = gaussHom ks
	| otherwise = eliminateRem ks' k' : ks'
	where
		k' = normalizeHead k
		ks' = (0:) <$> gaussHom (tail . eliminateHead k' <$> ks)

solveHom :: (Eq k, Fractional k) => [[k]] -> [[k]]
solveHom eqs = zipWith (++)
		(drop rang . transpose $ res)
		(((-1) *.) <$> idMat (matWidth eqs - rang))
	where
		res = gaussHom eqs
		rang = length res

solve :: (Eq k, Fractional k) => [[k]] -> Maybe ([k], [[k]])
solve eqs = case solveHom eqs of
	[] -> Nothing
	res -> Just (take ((matWidth eqs) - 1) $ last res, solveHom . transpose . init . transpose $ eqs)

idMat :: Fractional k => Int -> [[k]]
idMat = fmap <$> take <*> flip take basis

