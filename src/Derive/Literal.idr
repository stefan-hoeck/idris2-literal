module Derive.Literal

import public Derive.Refined
import Language.Reflection.Util

%default total

export
litImplDef : (impl, fun, con : Name) -> (p : ParamTypeInfo) -> RefinedInfo p -> Decl
litImplDef impl fun con (MkParamTypeInfo ti q ns [c] s) (RI x) =
  let p    := MkParamTypeInfo ti q ns [c] s
      arg  := p.applied
      vfun := var fun
      res  := appCon (vfun `app` varStr "n") c x
      prf  := `(fromJust0 (hdec0 {p = ~(proofType ns c.args x)} (~(vfun) n)))
      pred := `(\v => IsJust0 (hdec0 {p = ~(proofType ns c.args x)} (~(vfun) v)))
      rhs  := `(~(var con) (~pred) (\n,_ => let 0 prf := ~(prf) in ~(res)))
      
   in def impl [ patClause (var impl) rhs ]

export
intLitImplClaim : (impl : Name) -> (p : ParamTypeInfo) -> Decl
intLitImplClaim impl p = implClaimVis Public impl (implType "IntegerLit" p)

export
charLitImplClaim : (impl : Name) -> (p : ParamTypeInfo) -> Decl
charLitImplClaim impl p = implClaimVis Public impl (implType "CharLit" p)

export
stringLitImplClaim : (impl : Name) -> (p : ParamTypeInfo) -> Decl
stringLitImplClaim impl p = implClaimVis Public impl (implType "StringLit" p)

export
doubleLitImplClaim : (impl : Name) -> (p : ParamTypeInfo) -> Decl
doubleLitImplClaim impl p = implClaimVis Public impl (implType "DoubleLit" p)

--------------------------------------------------------------------------------
--          Derive
--------------------------------------------------------------------------------

export
IntegerLitVis : Visibility -> List Name -> ParamTypeInfo -> Res (List TopLevel)
IntegerLitVis vis nms p = map decls $ refinedInfo p
  where
    decls : RefinedInfo p -> List TopLevel
    decls x =
      let fun  := refineName p.getName
          impl := implName p "IntegerLit"
       in [ refineTL fun p x
          , TL (intLitImplClaim impl p) (litImplDef impl "fromInteger" "mkIL" p x)
          ]

export %inline
IntegerLit : List Name -> ParamTypeInfo -> Res (List TopLevel)
IntegerLit = IntegerLitVis Export

export
StringLitVis : Visibility -> List Name -> ParamTypeInfo -> Res (List TopLevel)
StringLitVis vis nms p = map decls $ refinedInfo p
  where
    decls : RefinedInfo p -> List TopLevel
    decls x =
      let fun  := refineName p.getName
          impl := implName p "StringLit"
       in [ refineTL fun p x
          , TL (stringLitImplClaim impl p) (litImplDef impl "fromString" "mkSL" p x)
          ]

export %inline
StringLit : List Name -> ParamTypeInfo -> Res (List TopLevel)
StringLit = StringLitVis Export

export
CharLitVis : Visibility -> List Name -> ParamTypeInfo -> Res (List TopLevel)
CharLitVis vis nms p = map decls $ refinedInfo p
  where
    decls : RefinedInfo p -> List TopLevel
    decls x =
      let fun  := refineName p.getName
          impl := implName p "CharLit"
       in [ refineTL fun p x
          , TL (charLitImplClaim impl p) (litImplDef impl "fromChar" "mkCL" p x)
          ]

export %inline
CharLit : List Name -> ParamTypeInfo -> Res (List TopLevel)
CharLit = CharLitVis Export

export
DoubleLitVis : Visibility -> List Name -> ParamTypeInfo -> Res (List TopLevel)
DoubleLitVis vis nms p = map decls $ refinedInfo p
  where
    decls : RefinedInfo p -> List TopLevel
    decls x =
      let fun  := refineName p.getName
          impl := implName p "DoubleLit"
       in [ refineTL fun p x
          , TL (doubleLitImplClaim impl p) (litImplDef impl "fromDouble" "mkDL" p x)
          ]

export %inline
DoubleLit : List Name -> ParamTypeInfo -> Res (List TopLevel)
DoubleLit = DoubleLitVis Export
