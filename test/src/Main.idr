module Main

import Data.DPair
import Data.So
import Data.Nat
import Literal

%default total

--------------------------------------------------------------------------------
-- Subset Literals
--------------------------------------------------------------------------------

0 Positive : Type
Positive = Subset Nat IsSucc

positive : Positive
positive = 12

lt : Subset Nat (`LT` 10)
lt = 8

0 IsPalindrome : String -> Type
IsPalindrome s = s === reverse s

0 Palindrome : Type
Palindrome = Subset String IsPalindrome

palindrome : Palindrome
palindrome = "foorabaroof"

0 IsAscii : Char -> Type
IsAscii c = So (ord c < 128)

0 Ascii : Type
Ascii = Subset Char IsAscii

ascii : Ascii
ascii = 'A'

0 IsPercentage : Double -> Type
IsPercentage d = So (0 <= d && d <= 100)

0 Percentage : Type
Percentage = Subset Double IsPercentage

percentage : Percentage
percentage = 12.1173

--------------------------------------------------------------------------------
-- main
--------------------------------------------------------------------------------

main : IO ()
main = putStrLn "All is well."
