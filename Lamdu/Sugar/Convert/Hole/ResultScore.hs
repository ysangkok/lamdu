{-# LANGUAGE NoImplicitPrelude #-}
module Lamdu.Sugar.Convert.Hole.ResultScore
    ( resultScore
    ) where

import qualified Control.Lens as Lens
import qualified Data.Map as Map
import           Lamdu.Calc.Type (Type(..), Composite(..))
import           Lamdu.Calc.Val.Annotated (Val(..))
import qualified Lamdu.Calc.Val as V
import qualified Lamdu.Expr.Lens as ExprLens
import qualified Lamdu.Infer as Infer

import           Lamdu.Prelude

resultTypeScore :: Type -> [Int]
resultTypeScore (TVar _) = [0]
resultTypeScore (TInst _ p) = 1 : maximum ([] : map resultTypeScore (Map.elems p))
resultTypeScore (TFun a r) = 2 : max (resultTypeScore a) (resultTypeScore r)
resultTypeScore (TSum c) = 2 : compositeTypeScore c
resultTypeScore (TRecord c) = 2 : compositeTypeScore c

compositeTypeScore :: Composite t -> [Int]
compositeTypeScore CEmpty = []
compositeTypeScore (CVar _) = [1]
compositeTypeScore (CExtend _ t r) =
    max (resultTypeScore t) (compositeTypeScore r)

resultScore :: Val Infer.Payload -> [Int]
resultScore val@(Val pl body) =
    bodyTopLevelScore body :
    resultTypeScore (pl ^. Infer.plType) ++
    [length (val ^.. ExprLens.payloadsOf ExprLens.valBodyHole)] ++
    (bodyParts body >>= resultScore)

bodyParts :: V.Body a -> [a]
bodyParts (V.BApp (V.Apply f a)) = [a, f]
bodyParts x = x ^.. Lens.traversed

bodyTopLevelScore :: V.Body (Val a) -> Int
bodyTopLevelScore body =
    case body of
    V.BApp (V.Apply (Val _ (V.BLeaf V.LHole)) _) -> 10
    V.BLeaf V.LHole -> 1
    _ -> 0
