module Alchemist.Generator.Haskell.BeamTable where

import Alchemist.DSL.Syntax.Storage
import Alchemist.Utils
import Data.List (intercalate, isInfixOf)
import qualified Data.Text as T
import Kernel.Prelude

formatType :: String -> String
formatType t = if " " `isInfixOf` t then "(" ++ t ++ ")" else t

-- Converts a FieldDef to a Beam field declaration
fieldDefToBeam :: FieldDef -> String
fieldDefToBeam hfield =
  intercalate ",\n    " $
    map
      ( \field ->
          if bIsEncrypted field
            then
              bFieldName field ++ "Encrypted :: B.C f " ++ (wrapMaybe "Text" field) ++ ",\n"
                ++ "    "
                ++ bFieldName field
                ++ "Hash :: B.C f "
                ++ (wrapMaybe "DbHash" field)
            else bFieldName field ++ " :: B.C f " ++ formatType (bFieldType field)
      )
      (beamFields hfield)
  where
    wrapMaybe :: String -> BeamField -> String
    wrapMaybe beamType field = if isMaybeType (bFieldType field) then "(Maybe " ++ beamType ++ ")" else beamType

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
    ++ fetchPrimaryKey
    ++ "\n    deriving (Generic, B.Beamable)\n"
    ++ "  primaryKey = "
    ++ tableNameHaskell tableDef
    ++ "Id "
    ++ fromMaybe (error $ T.pack ("Primary Key not found for " ++ tableNameHaskell tableDef)) (getPrimaryKeys (primaryKey tableDef))
    ++ "\n"
  where
    fetchPrimaryKey = fromMaybe (error $ T.pack ("Primary Key not found for " ++ tableNameHaskell tableDef)) (generateKeyTypes (filter (\f -> fieldName f `elem` primaryKey tableDef) $ fields tableDef))

    getPrimaryKeys [] = Nothing
    getPrimaryKeys [xs] = Just $ ". " <> xs
    getPrimaryKeys xs = Just $ foldl' handleAccPrimary "<$> " xs
    handleAccPrimary acc x = if acc == "<$> " then (acc ++ x) else acc ++ " <*> " ++ x

    generateKeyTypes :: [FieldDef] -> Maybe String
    generateKeyTypes [] = Nothing
    generateKeyTypes xs = Just $ foldl' handleAcc "Id" xs
      where
        handleAcc acc x = acc ++ " (B.C f " ++ formatType (beamType x) ++ ")"

-- Generates Haskell code for the table instances
tableInstancesToBeam :: TableDef -> String
tableInstancesToBeam tableDef =
  "type " ++ tableNameHaskell tableDef ++ " = " ++ tableNameHaskell tableDef ++ "T Identity\n\n"
    ++ "$(enableKVPG ''"
    ++ tableNameHaskell tableDef
    ++ "T ['"
    ++ intercalate ", '" (primaryKey tableDef)
    ++ "] "
    ++ ( if (null (secondaryKey tableDef))
           then "[]"
           else
             ( "[['"
                 ++ intercalate ", '" (secondaryKey tableDef)
                 ++ "]]"
             )
       )
    ++ ")\n\n"
    ++ "$(mkTableInstances ''"
    ++ tableNameHaskell tableDef
    ++ "T \""
    ++ tableNameSql tableDef
    ++ "\")\n"

-- Generates Haskell code for a Beam table
generateBeamTable :: TableDef -> String
generateBeamTable tableDef =
  "{-# LANGUAGE DerivingStrategies #-}\n"
    ++ "{-# LANGUAGE TemplateHaskell #-}\n"
    ++ "{-# OPTIONS_GHC -Wno-unused-imports #-}\n\n"
    ++ "module Storage.Beam."
    ++ (capitalize $ tableNameHaskell tableDef)
    ++ " where\n\n"
    ++ "import qualified Database.Beam as B\n"
    ++ "import Kernel.Prelude\n"
    ++ "import Tools.Beam.UtilsTH\n"
    ++ "import Kernel.External.Encryption\n"
    -- Add imports here
    ++ intercalate "\n" (map ("import qualified " ++) (imports tableDef))
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
