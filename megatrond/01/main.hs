--
-- Copyright (c) 2006 Don Stewart - http://www.cse.unsw.edu.au/~dons/
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)
--
import System.Environment

trim :: String -> String
trim = reverse . dropWhile(=='\n') . reverse

returnIfEqual :: Int -> Int -> Int
returnIfEqual a b 
	| a == b = a
	| otherwise = 0

mySum :: [Int] -> Int -> Int -> Int
mySum xs length index 
	| length == index = returnIfEqual (xs!!(index-1)) (xs!!0)
	| otherwise = (returnIfEqual (xs !! (index-1)) (xs !! index)) + (mySum xs length (index + 1))

mySum2 :: [Int] -> Int -> Int -> Int
mySum2 xs length index
	| length == index = returnIfEqual (xs!!(index-1)) (xs!!(div length 2))
	| otherwise = (returnIfEqual (xs !! (index-1)) (xs !! (((index - 1) + (length `div` 2)) `mod` length))) + (mySum2 xs length (index + 1))


-- | 'main' runs the main program
main :: IO ()
main = do
	content <- readFile("input.txt")
	let ints = map (\x -> read [x]::Int) (trim content)
	let l = mySum ints (length ints) 1
	print "First star answer:"
	print l
	let l2 = mySum2 ints (length ints) 1
	print "Second star answer:"
	print l2
