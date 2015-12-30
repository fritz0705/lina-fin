{-# OPTIONS_GHC -fno-warn-tabs #-}
module Algebra.Linear.Matrix
	( Matrix(Matrix)
	, getMatrix
	, width
	, height
	, columns
	, rows
	, transpose
	) where

import qualified Data.List
import Data.Maybe (fromJust)

newtype Matrix k = Matrix { getMatrix :: [[k]] }

instance Show k => Show (Matrix k) where
	show = show . getMatrix

width :: Matrix k -> Int
width = length . getMatrix

height :: Matrix k -> Int
height = width . transpose

columns :: Matrix k -> [[k]]
columns = Data.List.transpose . getMatrix

rows :: Matrix k -> [[k]]
rows = getMatrix

transpose :: Matrix k -> Matrix k
transpose = Matrix . Data.List.transpose . getMatrix

