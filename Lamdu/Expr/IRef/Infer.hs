{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
-- | Infer expressions where GlobalId's are known to be DefI's
module Lamdu.Expr.IRef.Infer
    ( M
    , loadInferScope
    , loadInferRecursive
    , run
    ) where

import           Control.Monad.Trans.Class (MonadTrans)
import           Control.Monad.Trans.Either (EitherT(..), hoistEither)
import           Control.Monad.Trans.Reader (ReaderT(..))
import           Control.Monad.Trans.State (StateT(..), mapStateT)
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import           Lamdu.Calc.Val.Annotated (Val(..))
import qualified Lamdu.Calc.Val.Annotated as Val
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Load as Load
import           Lamdu.Infer (Infer)
import qualified Lamdu.Infer as Infer
import qualified Lamdu.Infer.Error as InferErr
import           Lamdu.Infer.Load (Loader(Loader))
import qualified Lamdu.Infer.Load as InferLoad
import           Lamdu.Infer.Unify (unify)
import qualified Lamdu.Infer.Update as Update

import           Lamdu.Prelude

type T = Transaction

loader :: (MonadTrans trans, Monad m, Monad (trans (T m))) => Loader (trans (T m))
loader =
    Loader
    { InferLoad.loadTypeOf =
        \globalId ->
        ExprIRef.defI globalId & Transaction.readIRef & lift
        <&> Definition.typeOfDefBody
    , InferLoad.loadNominal = lift . Load.nominal
    }

type E m = EitherT InferErr.Error (T m)
type M m = ReaderT (Loader (E m)) (StateT Infer.Context (E m))

liftInfer :: Monad m => Infer a -> M m a
liftInfer = lift . mapStateT hoistEither . Infer.run

loadInferScope ::
    Monad m => Infer.Scope -> Val a -> M m (Val (Infer.Payload, a))
loadInferScope scope val =
    InferLoad.loadInfer loader scope val & lift & lift >>= liftInfer

loadInferInto ::
    Monad m => Infer.Payload -> Val a -> M m (Val (Infer.Payload, a))
loadInferInto pl val =
    do
        inferredVal <- loadInferScope (pl ^. Infer.plScope) val
        let inferredType = inferredVal ^. Val.payload . _1 . Infer.plType
        liftInfer $
            do
                unify inferredType (pl ^. Infer.plType)
                Update.inferredVal inferredVal & Update.liftInfer

loadInferRecursive ::
    Monad m => ExprIRef.DefI m -> Val a -> M m (Val (Infer.Payload, a))
loadInferRecursive defI val =
    do
        defType <- Infer.freshInferredVar Infer.emptyScope "r" & liftInfer
        let scope =
                Infer.insertTypeOf (ExprIRef.globalId defI) defType Infer.emptyScope
        loadInferInto (Infer.Payload defType scope) val

run :: Monad m => M m a -> T m (Either InferErr.Error (a, Infer.Context))
run = runEitherT . (`runStateT` Infer.initialContext) . (`runReaderT` loader)
