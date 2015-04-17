module Construct where

	import Helpers
	import Data.Numbers.Primes
	import Data.Maybe

	calcP :: Int -> Float -> [Int]
	calcP p e = 
		let	p' 			= (log $ convertInt p) ** (1+e)
			lowBound 	= round (p'/2)
			hightBound 	= floor p'
		in filter isPrime [lowBound..hightBound]

	calcS :: Int -> Float -> [Int]
	calcS p e = [1.. round $ (log $ convertInt p) ** (1+2.0*e)]

	calcTheoryBound :: Int -> Float -> Int -> Float
	calcTheoryBound p e len = (log $ convertInt p) ** (-e) * convertInt len

	calcConstruct :: Int -> Float -> (Float, Float, [Int])
	calcConstruct p e =
		let	pSet	= calcP p e
			sSet 	= calcS p e
			set = [ (s * fromJust (modInv r p)) `mod` p | r <- pSet, s <- sSet]
			cosList = calcCosList p
		in (calcTheoryBound p e (length set),
				getMaxForAllX cosList p 1.0 set,
				set)
