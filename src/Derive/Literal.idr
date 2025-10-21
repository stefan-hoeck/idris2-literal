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
litDef : (impl, fun, con : Name) -> Con n vs -> Decl
litDef impl fun con c =
  let t   := `(~(var fun) n)
      rhs := `(~(var con) (\n => ~(injArgs explicit (const t) c)))
   in def impl [patClause (var impl) rhs]

--------------------------------------------------------------------------------
--          Derive
--------------------------------------------------------------------------------

litVis :
     (iface    : String)
  -> (smartCon : Name)
  -> (plainCon : Name)
  -> (conv     : Name)
  -> Visibility
  -> List Name
  -> ParamTypeInfo
  -> Res (List TopLevel)
litVis iface smartCon plainCon conv vis nms p =
  case refinedInfo p of
    Right r => Right $ decls r
    Left  x => case p.info.cons of
      [c] =>
        let impl := implName p iface
         in Right [TL implClm (litDef impl conv plainCon c)]
      _   => failRecord "IntegerLit"
  where
    implNm  : Name
    implNm = implName p iface

    implClm : Decl
    implClm = implClaimVis Public implNm (implType (fromString iface) p)

    decls : RefinedInfo p -> List TopLevel
    decls x =
      let fun  := refineName p.getName
       in [ refineTL fun p x
          , TL implClm (litImplDef implNm conv smartCon p x)
          ]

export
IntegerLitVis : Visibility -> List Name -> ParamTypeInfo -> Res (List TopLevel)
IntegerLitVis = litVis "IntegerLit" "mkIL" "ilPlain" "fromInteger"

export %inline
IntegerLit : List Name -> ParamTypeInfo -> Res (List TopLevel)
IntegerLit = IntegerLitVis Export

export
StringLitVis : Visibility -> List Name -> ParamTypeInfo -> Res (List TopLevel)
StringLitVis = litVis "StringLit" "mkSL" "slPlain" "fromString"

export %inline
StringLit : List Name -> ParamTypeInfo -> Res (List TopLevel)
StringLit = StringLitVis Export

export
CharLitVis : Visibility -> List Name -> ParamTypeInfo -> Res (List TopLevel)
CharLitVis = litVis "CharLit" "mkCL" "clPlain" "fromChar"

export %inline
CharLit : List Name -> ParamTypeInfo -> Res (List TopLevel)
CharLit = CharLitVis Export

export
DoubleLitVis : Visibility -> List Name -> ParamTypeInfo -> Res (List TopLevel)
DoubleLitVis = litVis "DoubleLit" "mkDL" "dlPlain" "fromDouble"

export %inline
DoubleLit : List Name -> ParamTypeInfo -> Res (List TopLevel)
DoubleLit = DoubleLitVis Export
