{-# LANGUAGE NoImplicitPrelude, DeriveGeneric #-}
module Lamdu.Expr.Nominal
    ( Nominal(..)
    , apply
    ) where

import           Prelude.Compat

import           Control.DeepSeq (NFData(..))
import           Control.DeepSeq.Generics (genericRnf)
import           Data.Binary (Binary)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           GHC.Generics (Generic)
import           Lamdu.Expr.Scheme (Scheme)
import           Lamdu.Expr.Type (Type)
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Infer.Internal.Subst as Subst

data Nominal = Nominal
    { nParams :: Map T.ParamId T.TypeVar
    , nScheme :: Scheme
    } deriving (Generic, Show)
instance NFData Nominal where rnf = genericRnf
instance Binary Nominal

-- errorizes if the map mismatches the map in the Nominal
apply :: Map T.ParamId Type -> Nominal -> Scheme
apply m (Nominal params scheme) =
    Subst.apply subst scheme
    where
        subst = mempty { Subst.substTypes = Map.mapKeys (`find` params) m }
        find k =
            fromMaybe (error "Nominal.instantiate with wrong param map") .
            Map.lookup k
