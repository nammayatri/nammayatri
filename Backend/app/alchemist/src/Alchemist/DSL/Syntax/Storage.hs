module Alchemist.DSL.Syntax.Storage where

import Kernel.Prelude

data TableDef = TableDef
  { tableNameHaskell :: String,
    tableNameSql :: String,
    fields :: [FieldDef],
    imports :: [String],
    queries :: [QueryDef],
    primaryKey :: [String],
    secondaryKey :: [String],
    types :: Maybe [TypeObject]
  }
  deriving (Show)

data TypeObject = TypeObject (Text, [(Text, Text)])
  deriving (Show)

data QueryDef = QueryDef
  { queryName :: String,
    kvFunction :: String,
    params :: [(String, String)],
    whereClause :: WhereClause
  }
  deriving (Show)

data WhereClause = EmptyWhere | Leaf (String, String) | Query (Operator, [WhereClause]) deriving (Show)

data Operator = And | Or deriving (Show)

type FieldName = String

type HaskellType = String

type BeamType = String

type SqlType = String

data FieldDef = FieldDef
  { fieldName :: String,
    haskellType :: String,
    beamType :: String,
    sqlType :: String,
    constraints :: [FieldConstraint],
    defaultVal :: Maybe String,
    toTType :: Maybe String,
    fromTType :: Maybe String
  }
  deriving (Show)

data FieldConstraint = PrimaryKey | SecondaryKey | NotNull | AUTOINCREMENT deriving (Show, Eq)
