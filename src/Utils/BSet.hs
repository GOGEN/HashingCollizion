module BSet where

	import GeneticAlgorithm
	import Control.DeepSeq
	import Helpers
	import System.Random
	import qualified Data.List as  L
	import qualified  Data.Array as  A

	data BSet =  Null | B ([Int], BParams) deriving (Eq)

	instance Show BSet where
		show (B (xs, param)) 	= "Ring: " ++ (show $ ring param) ++ ", Deep: " 
			++ (show $ length xs) ++ ", Set: " ++ (show xs)
		show Null 				= "set is empty"

	data BParams = BParams {
		ring		:: Int,
		cosList 	:: A.Array Int Float
	} deriving (Show, Eq)

	instance NFData BSet where
		rnf (B (xs, _)) = rnf xs `seq` ()
		rnf (Null) = ()

	instance Individual BSet where
		crossover g Null _ = ([Null], g)
		crossover g _ Null = ([Null], g)
		crossover g (B (xs1, param)) (B (xs2, _)) =
			let	(idx, g') = randomR (0, length xs1 - 1) g
				xs1' = take idx xs1
				xs2' = drop idx xs2
				xs   = xs1' ++ xs2'
			in ([B (L.sort xs, param)], g')

		mutation g Null = (Null, g)
		mutation g (B (xs, param)) =
			let	(idx, g') = randomR (0, length xs - 1) g
				(dx, g'') = randomR (-3, 3) g'
				t = xs !! idx
				xs' = take idx xs ++ [(t + dx)`mod`ring param] ++ drop (idx+1) xs
			in (B (xs', param), g'')

		fitness Null 	= 1.0
		fitness set 	= 
			let max_err = 1.0 in
			max_err - (min max_err (err set))

	err :: BSet -> Float
	err Null = 1.0
	err (B (xs, param)) = getMaxForAllX (cosList param) (ring param) 1.0 xs