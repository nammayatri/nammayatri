module Alchemist.Generator.Haskell.BeamQueries where

import Alchemist.DSL.Syntax.Storage
import Alchemist.Utils
import Data.List (intercalate)
import Kernel.Prelude

generateImports :: TableDef -> String
generateImports tableDef =
  "{-# OPTIONS_GHC -Wno-orphans #-}\n\n"
    ++ "module Storage.Queries."
    ++ (capitalize $ tableNameHaskell tableDef)
    ++ " where\n\n"
    ++ "import Kernel.Beam.Functions\n"
    ++ "import qualified Storage.Beam."
    ++ (capitalize $ tableNameHaskell tableDef)
    ++ " as Beam"
    ++ (intercalate "\n" $ map (\i -> "import qualified " ++ i ++ " as " ++ i) $ imports tableDef)
    ++ "\n\n"

-- Determines the conversion function based on the Haskell type
toTTypeConversionFunction :: String -> String -> String
toTTypeConversionFunction haskellType fieldName =
  case haskellType of
    "Int" -> "roundToIntegral " ++ fieldName -- Example for Int to Float conversion
    "Id" -> "Id " ++ fieldName -- Example for a custom ID type
    _ -> fieldName -- Default case with no conversion

fromTTypeConversionFunction :: String -> String -> String
fromTTypeConversionFunction haskellType fieldName =
  case haskellType of
    "Int" -> "realToFrac " ++ fieldName -- Example for Int to Float conversion
    "Id" -> "getId " ++ fieldName -- Example for a custom ID type
    _ -> fieldName -- Default case with no conversion

-- Generates the FromTType' instance
fromTTypeInstance :: TableDef -> String
fromTTypeInstance tableDef =
  "instance FromTType' Beam." ++ tableNameHaskell tableDef ++ " " ++ tableNameHaskell tableDef ++ " where\n"
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
  "instance ToTType' Beam." ++ tableNameHaskell tableDef ++ " " ++ tableNameHaskell tableDef ++ " where\n"
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
