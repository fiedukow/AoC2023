import Data.List (find)
import Data.Char (isDigit)
import Data.Maybe (fromMaybe)

firstDigitIn :: String -> Char
firstDigitIn = fromMaybe (error "invalid input") . find isDigit

extractNumberOf :: String -> Int
extractNumberOf text = read [firstDigitIn text, (firstDigitIn . reverse) text]

solve :: String -> Int
solve = sum . map extractNumberOf . lines

main :: IO ()
main = readFile "input" >>= print . solve
