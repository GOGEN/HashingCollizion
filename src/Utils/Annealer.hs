module Annealer where

	import Control.Concurrent.Annealer
	import System.Random
	import BSet
	import Generators
	import qualified Data.Array as A
	import qualified Data.List as L
	import Helpers

	getMaxForAllX' :: A.Array Int Float -> Int -> Float-> [Int] -> Float
	getMaxForAllX' cosList q prevDelta xs = 
		let	partSum 	= partialSum cosList q
			ring		= [1..q `div` 2]
		in L.foldl' (\f s -> if f < prevDelta then max f (abs $ partSum xs s) else 1.0) 0 ring

	annealerGen :: RandomGen g => BParams -> g -> (Int, Int) -> IO ()
	annealerGen param gen (lowD, _) =
		let	initPop 	= getBSet param (lowD, lowD) (chooseRandom gen)
		in do
			print $ length initPop
			initAnn <- initAnnealer initPop err 10000 perturbation
			best <- getBestState initAnn
			print best
			print $ err best
			-- offerState best initAnn
			set <- annealForTime 40 1000000 initAnn
			print set
			print $ err set

	energyFunc :: Float -> BSet -> Bool
	energyFunc delta set = delta > err set

	-- perturbation :: BSet -> IO BSet
	-- perturbation set = do
	-- 	randomRIO (0,)

	perturbation :: BSet -> IO BSet
	perturbation set = do
		gen <- newStdGen
		return ( fst (perturbation' gen set))

	perturbation' :: RandomGen g => g -> BSet -> (BSet, g)
	perturbation' g (B (xs, param)) =
			let	(idx, g') = randomR (0, length xs - 1) g
				(dx, g'') = randomR (-1, 1) g'
				t = xs !! idx
				xs' = take idx xs ++ [(t + dx)`mod`ring param] ++ drop (idx+1) xs
			in (B (xs', param), g'')