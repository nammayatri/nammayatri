{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module DBQuery.Types where

import qualified Data.Aeson as A
import qualified Data.Map.Strict as M
import Kernel.Prelude

data InsertQuery = InsertQuery
  { schema :: SchemaName,
    dbModel :: DBModel,
    termWarps :: [TermWrap],
    mappings :: Mapping
  }

data UpdateQuery = UpdateQuery
  { schema :: SchemaName,
    dbModel :: DBModel,
    whereClause :: Where,
    setClauses :: [Set],
    mappings :: Mapping
  }

data DeleteQuery = DeleteQuery
  { schema :: SchemaName,
    dbModel :: DBModel,
    whereClause :: Where,
    mappings :: Mapping
  }

newtype QueryError = QueryError Text
  deriving (Show)

instance Exception QueryError

newtype Mapping = Mapping {getMapping :: M.Map Text Text}
  deriving stock (Show)
  deriving newtype (FromJSON)

newtype SchemaName = SchemaName {getSchemaName :: Text}
  deriving stock (Show)

newtype DBModel = DBModel {getDBModel :: Text}
  deriving stock (Show, Generic)
  deriving newtype (FromJSON)

type Where = [Clause]

newtype Column = Column {getColumn :: Text}
  deriving stock (Show)

data Clause where
  And :: [Clause] -> Clause
  Or :: [Clause] -> Clause
  Is :: Column -> Term -> Clause
  deriving stock (Show)

-- FIXME GADT not required?
data Term where
  In :: [Value] -> Term
  Eq :: Value -> Term
  Null :: Term
  GreaterThan :: Value -> Term
  GreaterThanOrEq :: Value -> Term
  LessThan :: Value -> Term
  LessThanOrEq :: Value -> Term
  Like :: Text -> Term
  Not :: Term -> Term
  deriving stock (Show)

data Set = Set Column Value
  deriving stock (Show)

-- FIXME GADT not required?
data TermWrap where
  TermWrap :: Column -> Value -> TermWrap
  deriving stock (Show)

newtype Value = Value {getValue :: A.Value}
  deriving stock (Show)
  deriving newtype (FromJSON)
