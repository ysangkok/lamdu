{-# LANGUAGE NoImplicitPrelude #-}
module Lamdu.Sugar.Convert.Nominal
    ( convertFromNom, convertToNom
    ) where

import           Control.Monad.Trans.Either.Utils (runMatcherT, justToLeft)
import           Data.UUID.Types (UUID)
import qualified Lamdu.Calc.Val as V
import           Lamdu.Calc.Val.Annotated (Val(..))
import           Lamdu.Sugar.Convert.Expression.Actions (addActionsWithSetToInner)
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import qualified Lamdu.Sugar.Convert.TIdG as ConvertTIdG
import qualified Lamdu.Sugar.Convert.Text as ConvertText
import           Lamdu.Sugar.Internal
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

convertFromNom ::
    (Monad m, Monoid a) => V.Nom (Val (Input.Payload m a)) ->
    Input.Payload m a -> ConvertM m (ExpressionU m a)
convertFromNom = convert BodyFromNom

convert ::
    (Monad m, Monoid a) =>
    (Nominal UUID (ExpressionU m a) -> BodyU m b) ->
    V.Nom (Val (Input.Payload m a)) ->
    Input.Payload m b -> ConvertM m (ExpressionU m b)
convert f (V.Nom tid val) exprPl =
    Nominal
    { _nTId = ConvertTIdG.convert tid
    , _nVal = val
    }
    & traverse ConvertM.convertSubexpression
    <&> f
    >>= addActionsWithSetToInner exprPl val

convertToNom ::
    (Monad m, Monoid a) =>
    V.Nom (Val (Input.Payload m a)) -> Input.Payload m a ->
    ConvertM m (ExpressionU m a)
convertToNom nom exprPl =
    do
        ConvertText.text nom exprPl & justToLeft
        convert BodyToNom nom exprPl & lift
    & runMatcherT
