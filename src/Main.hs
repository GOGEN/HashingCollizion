module Main where

	import System.Random
	import Helpers
	import Annealer
	import BSet
	import Minimization
	import Generators
	import Genetic
	import Construct
	import Bruteforse

	main :: IO ()
	main = do
		gen <- newStdGen
		-- annealerGen (BParams (2^10) (calcCosList (2^10))) gen (68,68)
		print (minimize (2^10) 0.01 (chooseGenetic gen (stopf 0.01)))
	