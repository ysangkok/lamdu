module Lamdu.DataFile
    ( getDataFilePath, getLamduDir
    ) where

import           Control.Lens.Operators
import qualified System.Directory as Directory
import           System.FilePath ((</>))

getLamduDir :: IO FilePath
getLamduDir = Directory.getHomeDirectory <&> (</> ".lamdu")

getDataFilePath :: FilePath -> FilePath -> IO FilePath
getDataFilePath startDir fileName =
    return customPath
    where
        customPath = startDir </> fileName
