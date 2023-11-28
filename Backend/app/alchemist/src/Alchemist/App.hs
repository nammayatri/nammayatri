{-# LANGUAGE QuasiQuotes #-}

module Alchemist.App where

import Alchemist.DSL.Parser.API (apiParser)
import Alchemist.DSL.Parser.Storage
import Alchemist.DSL.Syntax.API
import Alchemist.DSL.Syntax.Storage
import Alchemist.Generator.Haskell
import Alchemist.Generator.SQL
import Alchemist.Utils
import qualified Data.Text as T
import Kernel.Prelude

processAPIDSL :: String -> Apis
processAPIDSL dsl = case apiParser dsl of
  Left err -> error $ T.pack $ "Parsing error: " ++ show err
  Right apiDef -> apiDef

mkBeamTable :: FilePath -> FilePath -> IO ()
mkBeamTable filePath yaml = do
  tableDef <- storageParser yaml
  writeToFile (filePath ++ "/" ++ tableNameHaskell tableDef ++ ".hs") (generateBeamTable tableDef)

mkBeamQueries :: FilePath -> FilePath -> IO ()
mkBeamQueries filePath yaml = do
  tableDef <- storageParser yaml
  writeToFile (filePath ++ "/" ++ tableNameHaskell tableDef ++ ".hs") (generateBeamQueries tableDef)

mkDomainType :: FilePath -> FilePath -> IO ()
mkDomainType filePath yaml = do
  tableDef <- storageParser yaml
  writeToFile (filePath ++ "/" ++ tableNameHaskell tableDef ++ ".hs") (generateDomainType tableDef)

mkSQLFile :: FilePath -> FilePath -> IO ()
mkSQLFile filePath yaml = do
  tableDef <- storageParser yaml
  writeToFile (filePath ++ "/" ++ tableNameSql tableDef ++ ".sql") (generateSQL tableDef)

mkServantAPI :: FilePath -> String -> IO ()
mkServantAPI filePath dsl = do
  let apiDef = processAPIDSL dsl
  writeToFile (filePath ++ "/" ++ T.unpack (head (map _moduleName apiDef)) ++ ".hs") (generateServantAPI apiDef)

mkDomainHandler :: FilePath -> String -> IO ()
mkDomainHandler filePath dsl = do
  let apiDef = processAPIDSL dsl
  writeToFile (filePath ++ "/" ++ T.unpack (head (map _moduleName apiDef)) ++ ".hs") (generateDomainHandler apiDef)
