-- {-# LANGUAGE QuasiQuotes #-}

module Alchemist.App where

import Alchemist.DSL.Parser.API
import Alchemist.DSL.Parser.Storage
import Alchemist.DSL.Parser.Transformer
import Alchemist.DSL.Syntax.API
import Alchemist.DSL.Syntax.Storage
import qualified Alchemist.DSL.Syntax.Transformer as ST
import Alchemist.Generator.Haskell
import Alchemist.Generator.Haskell.ApiTypes
import Alchemist.Generator.Haskell.TransformerFunctions
import Alchemist.Generator.Purs
import Alchemist.Generator.SQL
import Alchemist.Utils
import qualified Data.Text as T
import Kernel.Prelude
import System.Directory
import System.FilePath

mkBeamTable :: FilePath -> FilePath -> IO ()
mkBeamTable filePath yaml = do
  tableDef <- storageParser yaml
  mapM_ (\t -> writeToFile filePath (tableNameHaskell t ++ ".hs") (generateBeamTable t)) tableDef

mkBeamQueries :: FilePath -> FilePath -> IO ()
mkBeamQueries filePath yaml = do
  tableDef <- storageParser yaml
  mapM_ (\t -> writeToFile filePath (tableNameHaskell t ++ ".hs") (generateBeamQueries t)) tableDef

mkDomainType :: FilePath -> FilePath -> IO ()
mkDomainType filePath yaml = do
  tableDef <- storageParser yaml
  mapM_ (\t -> writeToFile filePath (tableNameHaskell t ++ ".hs") (generateDomainType t)) tableDef

mkSQLFile :: FilePath -> FilePath -> IO ()
mkSQLFile filePath yaml = do
  tableDef <- storageParser yaml
  mapM_ (\t -> writeToFile filePath (tableNameSql t ++ ".sql") (generateSQL t)) tableDef

mkServantAPI :: FilePath -> FilePath -> IO ()
mkServantAPI filePath yaml = do
  apiDef <- apiParser yaml
  writeToFile filePath (T.unpack (_moduleName apiDef) ++ ".hs") (generateServantAPI apiDef)

mkApiTypes :: FilePath -> FilePath -> IO ()
mkApiTypes filePath yaml = do
  apiDef <- apiParser yaml
  writeToFile filePath (T.unpack (_moduleName apiDef) ++ ".hs") (generateApiTypes apiDef)

mkDomainHandler :: FilePath -> FilePath -> IO ()
mkDomainHandler filePath yaml = do
  apiDef <- apiParser yaml
  let fileName = T.unpack (_moduleName apiDef) ++ ".hs"
  fileExists <- doesFileExist (filePath </> fileName)
  unless fileExists $ writeToFile filePath fileName (generateDomainHandler apiDef)

mkFrontendAPIBackend :: FilePath -> FilePath -> IO ()
mkFrontendAPIBackend filePath yaml = do
  apiDef <- apiParser yaml
  writeToFile filePath (T.unpack (_moduleName apiDef) ++ "1.purs") (generateBackendCallAPI apiDef)

mkFrontendAPIEndpoint :: FilePath -> FilePath -> IO ()
mkFrontendAPIEndpoint filePath yaml = do
  apiDef <- apiParser yaml
  writeToFile filePath (T.unpack (_moduleName apiDef) ++ "2.purs") (generateAPIEndpoint apiDef)

mkTransformerFunctions :: FilePath -> FilePath -> IO ()
mkTransformerFunctions filePath yaml = do
  functionDef <- transformerParser yaml
  traverse_ (checkIfImpureMappingsExist (ST._monads functionDef)) (ST._functions functionDef)
  writeToFile filePath (T.unpack (ST._moduleName functionDef) ++ ".hs") (generateTransformerFunctions functionDef)
  where
    checkIfImpureMappingsExist :: [Text] -> ST.TransformerTT -> IO ()
    checkIfImpureMappingsExist allMonads fn = do
      let impureMappings = ST._impureMapping fn
          extraMonads = ST._extraMonads fn
      when (not (null impureMappings) && null allMonads && null extraMonads) $
        error "Impure mappings exist but no monads are provided in the transformer definition"
