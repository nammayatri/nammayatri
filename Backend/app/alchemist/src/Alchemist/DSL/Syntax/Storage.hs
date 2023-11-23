module Alchemist.DSL.Syntax.Storage where

import Kernel.Prelude

data TableDef = TableDef
  { tableNameHaskell :: String,
    tableNameSql :: String,
    imports :: [String],
    fields :: [FieldDef],
    primaryKey :: [String],
    secondaryKey :: [String]
  }
  deriving (Show)

data FieldDef = FieldDef
  { fieldName :: String,
    haskellType :: String,
    sqlType :: String,
    constraints :: [FieldConstraint]
  }
  deriving (Show)

data FieldConstraint = PrimaryKey | SecondaryKey | NotNull | Default String | CustomConstraint String
  deriving (Show, Eq)
