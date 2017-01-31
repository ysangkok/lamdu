{-# LANGUAGE LambdaCase #-}
module Main where
import Prelude hiding (lookup)
import Control.Concurrent.MVar (MVar, withMVar, newMVar)
import Lamdu.Data.DbLayout as DbLayout hiding (view)
import Lamdu.Data.DbInit (withDB)
import Data.Store.IRef as IRef (IRef)
import qualified Lamdu.Expr.IRef as ExprIRef
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import           Data.Store.Db (Db)
import           Lamdu.Calc.Val.Annotated (Val)
import qualified Lamdu.VersionControl as VersionControl
import Lamdu.Data.Export.Codejam (compile)

withDb :: MVar (Maybe Db) -> (Db -> IO a) -> IO a
withDb mvar action =
    withMVar mvar $ \case
    Nothing -> error "Trying to use DB when it is already gone"
    Just db -> action db

replIRef :: IRef ViewM (ExprIRef.ValI ViewM)
replIRef = DbLayout.repl DbLayout.codeIRefs

runViewTransactionInIO :: MVar (Maybe Db) -> Transaction ViewM a -> IO a
runViewTransactionInIO dbM trans =
    withDb dbM $ \db ->
    DbLayout.runDbTransaction db (VersionControl.runAction trans)

main = do
        withDB "/home/janus/.lamdu" $ \db -> do
          dbmvar <- newMVar (Just db)
          let x = Transaction.readIRef (DbLayout.repl DbLayout.codeIRefs) >>= ExprIRef.readVal :: Transaction ViewM (Val (ExprIRef.ValI ViewM))
          res <- runViewTransactionInIO dbmvar x
          compiled <- runViewTransactionInIO dbmvar ( compile res )
          print compiled
