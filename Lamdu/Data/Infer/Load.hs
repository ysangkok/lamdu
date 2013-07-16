module Lamdu.Data.Infer.Load
  ( Loader(..)
  , LoadError(..)
  , LoadedDef(..), ldDef, ldType -- re-export
  , load, newDefinition
  ) where

import Control.Lens.Operators
import Control.Monad (when)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Either (EitherT(..))
import Control.Monad.Trans.State (StateT)
import Control.MonadA (MonadA)
import Data.Maybe.Utils (unsafeUnjust)
import Data.Traversable (sequenceA)
import Data.UnionFind (Ref)
import Lamdu.Data.Infer.Internal
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.Either as Either
import qualified Data.Map as Map
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Infer.ExprRefs as ExprRefs

data Loader def m = Loader
  { loadDefType :: def -> m (Expr.Expression def ())
    -- TODO: For synonyms we'll need loadDefVal
  }

newtype LoadError def = LoadUntypedDef def

-- Error includes untyped def use
loadDefTypeIntoRef ::
  MonadA m =>
  Loader def m -> def ->
  StateT (Context def) (EitherT (LoadError def) m) Ref
loadDefTypeIntoRef (Loader loader) def = do
  loadedDefType <- lift . lift $ loader def
  when (Lens.has ExprLens.holePayloads loadedDefType) .
    lift . Either.left $ LoadUntypedDef def
  ExprRefs.exprIntoContext loadedDefType

newDefinition ::
  (MonadA m, Ord def) => def -> StateT (Context def) m Ref
newDefinition def = do
  ref <- ExprRefs.freshHole
  ctxDefRefs . Lens.at def %= setRef ref
  return ref
  where
    setRef ref Nothing = Just ref
    setRef _ (Just _) = error "newDefinition overrides existing def type"

load ::
  (Ord def, MonadA m) =>
  Loader def m -> Expr.Expression def a ->
  StateT (Context def) (EitherT (LoadError def) m)
    (Expr.Expression (LoadedDef def) a)
load loader expr = do
  existingDefRefs <- Lens.use ctxDefRefs <&> Lens.mapped %~ return
  -- Left wins in union
  allDefRefs <- sequenceA $ existingDefRefs `Map.union` defLoaders
  ctxDefRefs .= allDefRefs
  let
    getDefRef def =
      LoadedDef def .
      unsafeUnjust "We just added all defs!" $
      allDefRefs ^. Lens.at def
  expr & ExprLens.exprDef %~ getDefRef & return
  where
    defLoaders =
      Map.fromList
      [ (def, loadDefTypeIntoRef loader def)
      | def <- expr ^.. ExprLens.exprDef
      ]