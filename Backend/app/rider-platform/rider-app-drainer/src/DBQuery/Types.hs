{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wwarn=ambiguous-fields #-}

module DBQuery.Types where

import qualified Data.Aeson as A
import qualified Data.Map.Strict as M
import Data.Text as T
import qualified Data.Vector as V
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
  deriving stock (Show, Generic, Eq, Ord)
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

data Value = SqlNull | SqlString Text | SqlInteger Integer | SqlNum Double | SqlValue Text | SqlList [Value]
  deriving stock (Show)

instance FromJSON Value where
  parseJSON (A.String "SqlNull") = pure SqlNull
  parseJSON (A.String str) | T.length str >= 2 && T.isPrefixOf "\"" str && T.isSuffixOf "\"" str = do
    case readMaybe @Text $ T.unpack str of
      Just txt -> pure $ SqlString txt
      Nothing -> fail "Could not read Text in double quotes"
  parseJSON (A.String str) = case readMaybe @Integer $ T.unpack str of
    Just int -> pure $ SqlInteger int
    Nothing -> case readMaybe @Double $ T.unpack str of
      Just num -> pure $ SqlNum num
      Nothing -> pure $ SqlValue str
  parseJSON (A.Array ar) = SqlList . V.toList <$> (parseJSON `mapM` ar)
  parseJSON _ = fail "Expected String or Array"
