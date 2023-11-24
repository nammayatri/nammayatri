module Alchemist.Generator.Haskell.BeamQueries where

import Alchemist.DSL.Syntax.Storage
import Alchemist.Utils
import Data.List (intercalate)
import qualified Data.Text as Text
import Kernel.Prelude

generateImports :: TableDef -> String
generateImports tableDef =
  "{-# OPTIONS_GHC -Wno-orphans #-}\n\n"
    ++ "module Storage.Queries."
    ++ (capitalize $ tableNameHaskell tableDef)
    ++ " where\n\n"
    ++ "import Kernel.Beam.Functions\n"
    ++ "import Kernel.Prelude\n"
    ++ "import qualified Storage.Beam."
    ++ (capitalize $ tableNameHaskell tableDef)
    ++ " as Beam\n"
    ++ (intercalate "\n" $ map (\i -> "import qualified " ++ i ++ " as " ++ i) $ imports tableDef)
    ++ "\n\n"

toTTypeConversionFunction :: String -> String -> String
toTTypeConversionFunction haskellType fieldName
  | "Int" <- haskellType = "roundToIntegral " ++ fieldName
  | "Id " `Text.isInfixOf` (Text.pack haskellType) = "Kernel.Types.Id.Id " ++ fieldName
  | otherwise = fieldName

fromTTypeConversionFunction :: String -> String -> String
fromTTypeConversionFunction haskellType fieldName
  | "Int" <- haskellType = "realToFrac " ++ fieldName
  | "Id " `Text.isInfixOf` (Text.pack haskellType) = "Kernel.Types.Id.getId " ++ fieldName
  | otherwise = fieldName

-- Generates the FromTType' instance
fromTTypeInstance :: TableDef -> String
fromTTypeInstance tableDef =
  "instance FromTType' Beam." ++ tableNameHaskell tableDef ++ " Domain.Types." ++ tableNameHaskell tableDef ++ "." ++ tableNameHaskell tableDef ++ " where\n"
    ++ "  fromTType' Beam."
    ++ tableNameHaskell tableDef
    ++ "T {..} = do\n"
    ++ "    pure $\n"
    ++ "      Just\n"
    ++ "        "
    ++ tableNameHaskell tableDef
    ++ "\n"
    ++ "          { "
    ++ intercalate ",\n            " (map fromField (fields tableDef))
    ++ "\n          }\n\n"
  where
    fromField field = fieldName field ++ " = " ++ fromTTypeConversionFunction (haskellType field) (fieldName field)

-- Generates the ToTType' instance
toTTypeInstance :: TableDef -> String
toTTypeInstance tableDef =
  "instance ToTType' Beam." ++ tableNameHaskell tableDef ++ " Domain.Types." ++ tableNameHaskell tableDef ++ "." ++ tableNameHaskell tableDef ++ " where\n"
    ++ "  toTType' "
    ++ tableNameHaskell tableDef
    ++ " {..} = do\n"
    ++ "    Beam."
    ++ tableNameHaskell tableDef
    ++ "T\n"
    ++ "      { "
    ++ intercalate ",\n        " (map toField (fields tableDef))
    ++ "\n      }\n\n"
  where
    toField field = "Beam." ++ fieldName field ++ " = " ++ toTTypeConversionFunction (haskellType field) (fieldName field)

-- Generates both FromTType' and ToTType' instances
generateBeamQueries :: TableDef -> String
generateBeamQueries tableDef =
  generateImports tableDef ++ fromTTypeInstance tableDef ++ toTTypeInstance tableDef
