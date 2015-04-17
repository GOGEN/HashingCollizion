module Bruteforse(bruteforse) where

	import BSet

	bruteforse :: BParams -> [Int] -> Int -> [BSet]
	bruteforse param _ 0  = [B ([], param)]
	bruteforse param xs d = [ B (xs !! i : x, param) | i <- [0..(length xs)-1] 
	                                  , B (x, _) <- bruteforse param (drop i xs) (d-1) ]