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
-- IntegerLit deriving
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

record WrappedInt where
  constructor WI
  value : Nat

%runElab derive "WrappedInt" [Show,Eq,IntegerLit]

wrappedInt : WrappedInt
wrappedInt = 1_000_000

--------------------------------------------------------------------------------
-- StringLit deriving
--------------------------------------------------------------------------------

0 IsPlain : String -> Type
IsPlain = Str (All PrintableAscii)

record Plain where
  constructor PL
  value : String
  {auto 0 prf : IsPlain value}

%runElab derive "Plain" [Show,Eq,StringLit]

plain : Plain
plain = "The quick brown fox..."

record WrappedString where
  constructor WS
  value : String

%runElab derive "WrappedString" [Show,Eq,StringLit]

wrappedString : WrappedString
wrappedString = "The quick brown fox..."

--------------------------------------------------------------------------------
-- DoubleLit deriving
--------------------------------------------------------------------------------

Is01 : Double -> Bool
Is01 v = 0.0 <= v && v <= 1.0

record D01 where
  constructor D0
  value : Double
  {auto 0 prf : Holds Is01 value}

%runElab derive "D01" [Show,Eq,DoubleLit]

d01 : D01
d01 = 0.7765

d01_int : D01
d01_int = 0

record WrappedDouble where
  constructor WD
  value : Double

%runElab derive "WrappedDouble" [Show,Eq,DoubleLit]

wrappedDouble : WrappedDouble
wrappedDouble = 12.0e-1

wrappedDouble2 : WrappedDouble
wrappedDouble2 = 3

--------------------------------------------------------------------------------
-- CharLit deriving
--------------------------------------------------------------------------------

record AChar where
  constructor AC
  value : Char
  {auto 0 prf : Ascii value}

%runElab derive "AChar" [Show,Eq,CharLit]

achar : AChar
achar = '!'

record WrappedChar where
  constructor WC
  value : Char

%runElab derive "WrappedChar" [Show,Eq,CharLit]

wrappedChar : WrappedChar
wrappedChar = 'o'

--------------------------------------------------------------------------------
-- main
--------------------------------------------------------------------------------

main : IO ()
main = putStrLn "All is well."
