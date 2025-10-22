||| This provides `Lit` implementations for the types from the
||| prelude. If you import this, make sure to add the following
||| pragmas to the top of your source files:
|||
||| ```idris
||| %hide Builtin.fromString
||| %hide Builtin.fromChar
||| %hide Builtin.fromInteger
||| %hide Builtin.fromDouble
||| ```
|||
||| Even then, you might experience some interference when using
||| other libraries or data types. Only use if you know what you
||| are doing.
module Literal.Prelude

import public Literal

%default total

public export %hint
stringLitImpl : StringLit String
stringLitImpl = slPlain id

public export %hint
charLitImpl : CharLit Char
charLitImpl = clPlain id

public export %hint
doubleLitImpl : DoubleLit Double
doubleLitImpl = dlPlain id

public export %hint
integerLitImpl : IntegerLit Integer
integerLitImpl = ilPlain id

public export %hint
intLitImpl : IntegerLit Int
intLitImpl = ilPlain prim__cast_IntegerInt

public export %hint
natLitImpl : IntegerLit Nat
natLitImpl = ilPlain integerToNat

public export %hint
bits8LitImpl : IntegerLit Bits8
bits8LitImpl = ilPlain prim__cast_IntegerBits8

public export %hint
bits16LitImpl : IntegerLit Bits16
bits16LitImpl = ilPlain prim__cast_IntegerBits16

public export %hint
bits32LitImpl : IntegerLit Bits32
bits32LitImpl = ilPlain prim__cast_IntegerBits32

public export %hint
bits64LitImpl : IntegerLit Bits64
bits64LitImpl = ilPlain prim__cast_IntegerBits64

public export %hint
int8LitImpl : IntegerLit Int8
int8LitImpl = ilPlain prim__cast_IntegerInt8

public export %hint
int16LitImpl : IntegerLit Int16
int16LitImpl = ilPlain prim__cast_IntegerInt16

public export %hint
int32LitImpl : IntegerLit Int32
int32LitImpl = ilPlain prim__cast_IntegerInt32

public export %hint
int64LitImpl : IntegerLit Int64
int64LitImpl = ilPlain prim__cast_IntegerInt64
