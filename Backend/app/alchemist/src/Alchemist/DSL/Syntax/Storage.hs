module Alchemist.DSL.Syntax.Storage where

import Kernel.Prelude

data TableDef = TableDef
  { tableNameHaskell :: String,
    tableNameSql :: String,
    fields :: [FieldDef],
    imports :: [String],
    primaryKey :: [String],
    secondaryKey :: [String]
  }
  deriving (Show)

data FieldDef = FieldDef
  { fieldName :: String,
    haskellType :: String,
    sqlType :: String,
    constraints :: [FieldConstraint],
    defaultVal :: Maybe String,
    toTType :: Maybe String,
    fromTType :: Maybe String
  }
  deriving (Show)

data FieldConstraint = PrimaryKey | SecondaryKey | NotNull | AUTOINCREMENT deriving (Show, Eq)
