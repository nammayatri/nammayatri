module Alchemist.Generator.Haskell.BeamQueries where

import Alchemist.DSL.Syntax.Storage
import Data.List (intercalate)
import Kernel.Prelude

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
  "instance FromTType' BeamCS." ++ tableNameHaskell tableDef ++ " " ++ tableNameHaskell tableDef ++ " where\n"
    ++ "  fromTType' BeamCS."
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
  "instance ToTType' BeamCS." ++ tableNameHaskell tableDef ++ " " ++ tableNameHaskell tableDef ++ " where\n"
    ++ "  toTType' "
    ++ tableNameHaskell tableDef
    ++ " {..} = do\n"
    ++ "    BeamCS."
    ++ tableNameHaskell tableDef
    ++ "T\n"
    ++ "      { "
    ++ intercalate ",\n        " (map toField (fields tableDef))
    ++ "\n      }\n\n"
  where
    toField field = "BeamCS." ++ fieldName field ++ " = " ++ toTTypeConversionFunction (haskellType field) (fieldName field)

-- Generates both FromTType' and ToTType' instances
generateTTypeInstances :: TableDef -> String
generateTTypeInstances tableDef =
  fromTTypeInstance tableDef ++ toTTypeInstance tableDef
