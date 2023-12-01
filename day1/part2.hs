import Data.List (find, isPrefixOf)
import Data.Char (isDigit)
import Data.Maybe (fromMaybe)

-- replaces prefix of text with other text, but only if the prefix is exactly the same as the from text
prefixReplace :: String -> String -> String -> String
prefixReplace from to text | from `isPrefixOf` text = to ++ (drop (length from) text)
                           | otherwise = text

replaceManyLeftToRightInternal :: [(String, String)] -> [(String, String)] -> String -> String
replaceManyLeftToRightInternal _ _ [] = []
replaceManyLeftToRightInternal originalPairs [] (s: xs) = s: replaceManyLeftToRightInternal originalPairs originalPairs xs
replaceManyLeftToRightInternal originalPairs ((from, to): xs) text = replaceManyLeftToRightInternal originalPairs xs (prefixReplace from to text)

-- replaces all occurences of items from the provided list with their counterparts.
-- the replacements are done from left to right, so if there are multiple items that can be replaced, the
-- order of appearing in text wins with the order of appearing in the list of replacement pairs
replaceManyLeftToRight :: [(String, String)] -> String -> String
replaceManyLeftToRight fromToPairs text = replaceManyLeftToRightInternal fromToPairs fromToPairs text

textDigits :: [(String, String)]
textDigits = [("one", "1"), ("two", "2"), ("three", "3"), ("four", "4"), ("five", "5"), ("six", "6"), ("seven", "7"), ("eight", "8"), ("nine", "9")]

digitize :: String -> String
digitize = replaceManyLeftToRight textDigits

reverseFirst :: (String, a) -> (String, a)
reverseFirst (f, s) = (reverse f, s)

reverseDigitize :: String -> String
reverseDigitize = replaceManyLeftToRight (map reverseFirst textDigits)

firstDigitIn :: String -> Char
firstDigitIn = fromMaybe (error "invalid input") . find isDigit

extractNumberOf :: String -> Int
extractNumberOf text = read [(firstDigitIn . digitize) text, (firstDigitIn . reverseDigitize . reverse) text]

solve :: String -> Int
solve = sum . map extractNumberOf . lines

main :: IO ()
main = readFile "input" >>= print . solve
