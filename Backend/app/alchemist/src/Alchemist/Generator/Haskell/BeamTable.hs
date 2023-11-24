module Alchemist.Generator.Haskell.BeamTable where

import Alchemist.DSL.Syntax.Storage
import Alchemist.Utils
import Data.List (intercalate)
import Kernel.Prelude

formatType :: String -> String
formatType t = if ' ' `elem` t then "(" ++ t ++ ")" else t

-- Converts a FieldDef to a Beam field declaration
fieldDefToBeam :: FieldDef -> String
fieldDefToBeam field =
  fieldName field ++ " :: B.C f " ++ formatType (haskellType field)

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
    ++ formatType (haskellType fetchPrimaryKey)
    ++ ")\n    deriving (Generic, B.Beamable)\n"
    ++ "  primaryKey = "
    ++ tableNameHaskell tableDef
    ++ "Id . "
    ++ head (primaryKey tableDef)
    ++ "\n"
  where
    fetchPrimaryKey = head $ filter (\f -> fieldName f `elem` primaryKey tableDef) $ fields tableDef

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
    ++ "import Kernel.Prelude\n"
    ++ "import Tools.Beam.UtilsTH\n"
    -- Add imports here
    ++ (intercalate "\n" $ map (\i -> "import qualified " ++ i ++ " as " ++ i) $ imports tableDef)
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
