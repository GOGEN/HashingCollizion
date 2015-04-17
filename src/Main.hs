module Main where

	import System.Random
	import Helpers
	import Annealer
	import BSet
	import Minimization
	import Generators
	import Genetic
	import Construct

	main :: IO ()
	main = do
		gen <- newStdGen
		--annealerGen (BParams (2^10) (calcCosList (2^10))) gen (68,68)
		print (minimize (2^5) 0.06 (chooseGenetic gen (stopf 0.06)))
		-- print $ calcConstruct (1523) 0.1