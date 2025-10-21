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
intLitImplDef : (impl, fun : Name) -> (p : ParamTypeInfo) -> RefinedInfo p -> Decl
intLitImplDef impl fun (MkParamTypeInfo ti q ns [c] s) (RI x) =
  let p    := MkParamTypeInfo ti q ns [c] s
      arg  := p.applied
      vfun := var fun
      res  := appCon (var "fromInteger" `app` varStr "n") c x
      prf  := `(fromJust0 (hdec0 {p = ~(proofType ns c.args x)} (fromInteger n)))
      pred := `(\v => IsJust0 (hdec0 {p = ~(proofType ns c.args x)} (fromInteger v)))
      rhs  := `(mkIL (~pred) (\n,_ => let 0 prf := ~(prf) in ~(res)))
      
   in def impl [ patClause (var impl) rhs ]

export
charLitImplClaim : (impl : Name) -> (p : ParamTypeInfo) -> Decl
charLitImplClaim impl p = implClaimVis Public impl (implType "CharLit" p)

export
stringLitImplClaim : (impl : Name) -> (p : ParamTypeInfo) -> Decl
stringLitImplClaim impl p = implClaimVis Public impl (implType "StringLit" p)

export
doubleLitImplClaim : (impl : Name) -> (p : ParamTypeInfo) -> Decl
doubleLitImplClaim impl p = implClaimVis Public impl (implType "DoubleLit" p)

-- export
-- fromDblTL : Visibility -> (p : ParamTypeInfo) -> RefinedInfo p -> TopLevel
-- fromDblTL vis = litTL vis "fromDouble" "FromDouble" `(Double)
-- 
-- export
-- fromStrTL : Visibility -> (p : ParamTypeInfo) -> RefinedInfo p -> TopLevel
-- fromStrTL vis = litTL vis "fromString" "FromString" `(String)

--------------------------------------------------------------------------------
--          Derive
--------------------------------------------------------------------------------

export
IntegerLitVis :
     Visibility
  -> List Name
  -> ParamTypeInfo
  -> Res (List TopLevel)
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
