module Alchemist.Generator.Haskell.BeamTable where

import Alchemist.DSL.Syntax.Storage
import Data.Char (toUpper)
import Data.List (intercalate)
import Kernel.Prelude

-- Converts a FieldDef to a Beam field declaration
fieldDefToBeam :: FieldDef -> String
fieldDefToBeam field =
  fieldName field ++ " :: B.C f " ++ haskellType field

-- Converts a list of fields to Beam field declarations
fieldsToBeam :: [FieldDef] -> String
fieldsToBeam fields =
  intercalate ",\n    " $ map fieldDefToBeam fields

-- Generates Haskell code for the primary key
primaryKeyToBeam :: TableDef -> String
primaryKeyToBeam tableDef =
  "instance B.Table " ++ tableNameHaskell tableDef ++ "T where\n"
    ++ "  data PrimaryKey "
    ++ tableNameHaskell tableDef
    ++ "T f = "
    ++ tableNameHaskell tableDef
    ++ "Id (B.C f "
    ++ (haskellType . head . filter (\f -> fieldName f `elem` primaryKey tableDef) . fields $ tableDef)
    ++ ")\n    deriving (Generic, B.Beamable)\n"
    ++ "  primaryKey = "
    ++ tableNameHaskell tableDef
    ++ "Id . "
    ++ head (primaryKey tableDef)
    ++ "\n"

-- Generates Haskell code for the table instances
tableInstancesToBeam :: TableDef -> String
tableInstancesToBeam tableDef =
  "type " ++ tableNameHaskell tableDef ++ " = " ++ tableNameHaskell tableDef ++ "T Identity\n\n"
    ++ "$(enableKVPG ''"
    ++ tableNameHaskell tableDef
    ++ "T ['"
    ++ head (primaryKey tableDef)
    ++ "] [['"
    ++ intercalate "', '" (secondaryKey tableDef)
    ++ "]])\n\n"
    ++ "$(mkTableInstances ''"
    ++ tableNameHaskell tableDef
    ++ "T \""
    ++ tableNameSql tableDef
    ++ "\")\n"

-- Generates Haskell code for a Beam table
generateBeamTable :: TableDef -> String
generateBeamTable tableDef =
  "{-# LANGUAGE DerivingStrategies #-}\n"
    ++ "{-# LANGUAGE TemplateHaskell #-}\n\n"
    ++ "module Storage.Beam."
    ++ (capitalize $ tableNameHaskell tableDef)
    ++ " where\n\n"
    ++ "import qualified Database.Beam as B\n"
    ++
    -- Add imports here
    (intercalate "\n" $ map (\i -> "import qualified " ++ i ++ " as " ++ i) $ imports tableDef)
    ++ "\n\n"
    ++ "data "
    ++ tableNameHaskell tableDef
    ++ "T f = "
    ++ tableNameHaskell tableDef
    ++ "T\n"
    ++ "  { "
    ++ fieldsToBeam (fields tableDef)
    ++ "\n  }\n"
    ++ "  deriving (Generic, B.Beamable)\n\n"
    ++ primaryKeyToBeam tableDef
    ++ "\n"
    ++ tableInstancesToBeam tableDef

-- Helper function to capitalize a string
capitalize :: String -> String
capitalize "" = ""
capitalize (x : xs) = toUpper x : xs
