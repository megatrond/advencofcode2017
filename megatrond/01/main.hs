--
-- Copyright (c) 2006 Don Stewart - http://www.cse.unsw.edu.au/~dons/
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)
--
import System.Environment

trim :: String -> String
trim = reverse . dropWhile(=='\n') . reverse

-- | 'main' runs the main program
main :: IO ()
main = do
	content <- readFile("input.txt")
	let ints = map (\x -> read [x]::Int) (trim content)
	print ints
