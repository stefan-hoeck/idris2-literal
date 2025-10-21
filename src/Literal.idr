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

public export %inline
Cast Char a => CharLit (Subset a p) where
  CharPred = p . cast
  fromChar c = Element (cast c) %search

public export %inline
Cast Double a => DoubleLit (Subset a p) where
  DoublePred = p . cast
  fromDouble d = Element (cast d) %search

public export %inline
Cast Integer a => IntegerLit (Subset a p) where
  IntegerPred = p . cast
  fromInteger i = Element (cast i) %search

public export %inline
Cast String a => StringLit (Subset a p) where
  StringPred = p . cast
  fromString s = Element (cast s) %search

--------------------------------------------------------------------------------
-- Utility constructors for elab deriving
--------------------------------------------------------------------------------

public export %inline
mkIL :
     (p : Integer -> Type)
  -> ((i : Integer) -> (0 prf : p i) -> a)
  -> IntegerLit a
mkIL p f = IL p (\v => f v %search)

public export %inline
mkSL :
     (p : String -> Type)
  -> ((i : String) -> (0 prf : p i) -> a)
  -> StringLit a
mkSL p f = SL p (\v => f v %search)

public export %inline
mkCL :
     (p : Char -> Type)
  -> ((i : Char) -> (0 prf : p i) -> a)
  -> CharLit a
mkCL p f = CL p (\v => f v %search)

public export %inline
mkDL :
     (p : Double -> Type)
  -> ((i : Double) -> (0 prf : p i) -> a)
  -> DoubleLit a
mkDL p f = DL p (\v => f v %search)
