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
import Text.Parsec

parseStorageDSL :: String -> TableDef
parseStorageDSL dsl = do
  case parse storageParser "" dsl of
    Left err -> error (T.pack $ "Parsing rrror: " ++ show err)
    Right tableDef -> tableDef

processAPIDSL :: String -> Apis
processAPIDSL dsl = case apiParser dsl of
  Left err -> error $ T.pack $ "Parsing error: " ++ show err
  Right apiDef -> apiDef

mkBeamTable :: FilePath -> String -> IO ()
mkBeamTable filePath dsl = do
  let tableDef = parseStorageDSL dsl
  writeToFile (filePath ++ "/" ++ tableNameHaskell tableDef ++ ".hs") (generateBeamTable tableDef)

mkBeamQueries :: FilePath -> String -> IO ()
mkBeamQueries filePath dsl = do
  let tableDef = parseStorageDSL dsl
  writeToFile (filePath ++ "/" ++ tableNameHaskell tableDef ++ ".hs") (generateBeamQueries tableDef)

mkDomainType :: FilePath -> String -> IO ()
mkDomainType filePath dsl = do
  let tableDef = parseStorageDSL dsl
  writeToFile (filePath ++ "/" ++ tableNameHaskell tableDef ++ ".hs") (generateDomainType tableDef)

mkSQLFile :: FilePath -> String -> IO ()
mkSQLFile filePath dsl = do
  let tableDef = parseStorageDSL dsl
  writeToFile (filePath ++ "/" ++ tableNameSql tableDef ++ ".sql") (generateSQL tableDef)

mkServantAPI :: FilePath -> String -> IO ()
mkServantAPI filePath dsl = do
  let apiDef = processAPIDSL dsl
  writeToFile (filePath ++ "/" ++ T.unpack (head (map _moduleName apiDef)) ++ ".hs") (generateServantAPI apiDef)
