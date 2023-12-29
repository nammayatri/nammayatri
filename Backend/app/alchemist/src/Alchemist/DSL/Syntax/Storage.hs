module Alchemist.DSL.Syntax.Storage where

import Alchemist.GeneratorCore
import Kernel.Prelude

data SqlUpdates = SqlUpdates
  { fieldUpdates :: Maybe (String, SqlFieldUpdates),
    keysInPrimaryKey :: [String]
  }
  deriving (Show)

data MigrationFile = MigrationFile
  { sqlTableName :: String,
    fields_ :: [FieldDef],
    primaryKeys :: [String],
    rawLastSqlFile :: String
  }
  deriving (Show)

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

newtype TypeObject = TypeObject (String, ([(String, String)], [String])) --  (TypeName, ([(Field, HaskellType)], [InstanceToDerive]))
  deriving (Show)

data QueryDef = QueryDef
  { queryName :: String,
    kvFunction :: String,
    params :: [((String, String), Bool)],
    whereClause :: WhereClause,
    takeFullObjectAsInput :: Bool
  }
  deriving (Show)

data WhereClause = EmptyWhere | Leaf (String, String, Maybe Operator) | Query (Operator, [WhereClause]) deriving (Show)

data Operator = And | Or | In | Eq | GreaterThan | LessThan | GreaterThanOrEq | LessThanOrEq deriving (Show, Eq)

comparisonOperator :: [Operator]
comparisonOperator = [In, Eq, GreaterThan, LessThan, GreaterThanOrEq, LessThanOrEq]

data FieldDef = FieldDef
  { fieldName :: String,
    haskellType :: String,
    beamFields :: [BeamField],
    fromTType :: Maybe String,
    isEncrypted :: Bool
  }
  deriving (Show)

data FieldConstraint = PrimaryKey | SecondaryKey | NotNull | AUTOINCREMENT deriving (Show, Eq, Ord)

data SqlFieldUpdates = DropNotNull | DropDefault | AddNotNull | AddDefault String | TypeChange | DropPrimaryKey | AddPrimaryKey deriving (Show)

instance Eq SqlFieldUpdates where
  (==) DropNotNull DropNotNull = True
  (==) DropDefault DropDefault = True
  (==) AddNotNull AddNotNull = True
  (==) DropPrimaryKey DropPrimaryKey = True
  (==) AddPrimaryKey AddPrimaryKey = True
  (==) (AddDefault _) (AddDefault _) = True
  (==) TypeChange TypeChange = True
  (==) _ _ = False

data BeamField = BeamField
  { bFieldName :: String,
    hFieldType :: String,
    bFieldType :: String,
    bConstraints :: [FieldConstraint],
    bFieldUpdates :: [SqlFieldUpdates],
    bSqlType :: String,
    bDefaultVal :: Maybe String,
    bfieldExtractor :: [String],
    bToTType :: Maybe String,
    bIsEncrypted :: Bool
  }
  deriving (Show)

type StorageM = BuilderM TableDef
