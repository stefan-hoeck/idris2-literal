module Main

import Data.Maybe0
import Data.Refined.Bits32
import Data.Refined.String
import Data.DPair
import Data.So
import Data.Nat
import Literal

import Derive.Prelude
import Derive.Literal

%default total
%language ElabReflection

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
-- 
--------------------------------------------------------------------------------

0 IsPerc : Bits32 -> Type
IsPerc v = v <= 100

record Perc where
  constructor P
  value : Bits32
  {auto 0 prf : IsPerc value}

%runElab derive "Perc" [Show,Eq,IntegerLit]

perc : Perc
perc = 33

0 IsPlain : String -> Type
IsPlain = Str (All PrintableAscii)

record Plain where
  constructor PL
  value : String
  {auto 0 prf : IsPlain value}

%runElab derive "Plain" [Show,Eq,StringLit]

plain : Plain
plain = "The quick brown fox jumped over the lazy dog."

--------------------------------------------------------------------------------
-- main
--------------------------------------------------------------------------------

main : IO ()
main = putStrLn "All is well."
