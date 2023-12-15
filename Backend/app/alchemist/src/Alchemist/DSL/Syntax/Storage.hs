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
    types :: Maybe [TypeObject],
    containsEncryptedField :: Bool
  }
  deriving (Show)

data TypeObject = TypeObject (String, ([(String, String)], Maybe String))
  deriving (Show)

data QueryDef = QueryDef
  { queryName :: String,
    kvFunction :: String,
    params :: [((String, String), Bool)],
    whereClause :: WhereClause,
    takeFullObjectAsInput :: Bool
  }
  deriving (Show)

data WhereClause = EmptyWhere | Leaf (String, String) | Query (Operator, [WhereClause]) deriving (Show)

data Operator = And | Or deriving (Show)

data FieldDef = FieldDef
  { fieldName :: String,
    haskellType :: String,
    beamType :: String,
    beamFields :: [BeamField],
    sqlType :: String,
    constraints :: [FieldConstraint],
    defaultVal :: Maybe String,
    toTType :: Maybe String,
    fromTType :: Maybe String,
    isEncrypted :: Bool
  }
  deriving (Show)

data FieldConstraint = PrimaryKey | SecondaryKey | NotNull | AUTOINCREMENT deriving (Show, Eq)

data BeamField = BeamField
  { bFieldName :: String,
    hFieldType :: String,
    bFieldType :: String,
    bConstraints :: [FieldConstraint],
    bSqlType :: String,
    bDefaultVal :: Maybe String,
    bfieldExtractor :: [String],
    bToTType :: Maybe String,
    bIsEncrypted :: Bool
  }
  deriving (Show)
