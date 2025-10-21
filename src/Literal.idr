module Literal

import Data.DPair

%default total

public export
interface CharLit (0 a : Type) where
  constructor CL
  0 CharPred : Char -> Type
  fromChar : (c : Char) -> (0 p : CharPred c) => a

public export
interface DoubleLit (0 a : Type) where
  constructor DL
  0 DoublePred : Double -> Type
  fromDouble : (d : Double) -> (0 p : DoublePred d) => a

public export
interface IntegerLit (0 a : Type) where
  constructor IL
  0 IntegerPred : Integer -> Type
  fromInteger : (i : Integer) -> (0 p : IntegerPred i) => a

public export
interface StringLit (0 a : Type) where
  constructor SL
  0 StringPred : String -> Type
  fromString : (s : String) -> (0 p : StringPred s) => a

public export
CharLit (Subset Char p) where
  CharPred = p
  fromChar c = Element c %search

public export
DoubleLit (Subset Double p) where
  DoublePred = p
  fromDouble d = Element d %search

public export
IntegerLit (Subset Integer p) where
  IntegerPred = p
  fromInteger i = Element i %search

public export
StringLit (Subset String p) where
  StringPred = p
  fromString s = Element s %search
