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
import qualified Lamdu.Calc.Val.Annotated as Annotated (Val(..), body)
import qualified Lamdu.VersionControl as VersionControl
import Lamdu.Data.Export.Codejam (compile)
-- import qualified Lamdu.Data.Definition as Def
import Control.Lens.Getter (view)
import qualified Lamdu.Calc.Val as Val
import qualified Lamdu.Calc.Type
import Lamdu.Calc.Identifier (Identifier(..))
import Data.ByteString.Char8 (pack)

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
          let x = Transaction.readIRef (DbLayout.repl DbLayout.codeIRefs) >>= ExprIRef.readVal
          val <- runViewTransactionInIO dbmvar x
          compiled <- runViewTransactionInIO dbmvar ( compile val )
          Val.BApp val <- return $ view Annotated.body val
          val <- pure $ view Val.applyArg val
          Val.BRecExtend val <- pure $ view Annotated.body val
          val <- pure $ view Val.recFieldVal val
          Val.BApp val <- return $ view Annotated.body val
          val <- pure $ view Val.applyArg val
          Val.BRecExtend val <- pure $ view Annotated.body val
          val <- pure $ view Val.recFieldVal val
          Val.BLeaf val <- return $ view Annotated.body val
          let x = (Lamdu.Calc.Type.NominalId $ Identifier $ pack "BI:float\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL" , pack "\SOH\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\a\NUL\224X=\177|\DC1\255\255\255\255\255\255\255\243")
          case val of
            Val.LLiteral (Val.PrimVal p d) -> do
              print $ (p, d)
              print $ x == (p, d)
            _ -> error "lol"
          -- print res
          -- print compiled
