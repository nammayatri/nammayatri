{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types.DBSync.DBModel where

import DBQuery.Types
import qualified Data.Aeson as A
import qualified Data.Text as T
import Kernel.Prelude

newtype DBModelOptions = DBModelOptions {getDBModelOptions :: DBModel}
  deriving stock (Show, Generic)

instance FromJSON DBModelOptions where
  parseJSON = A.withText "DBModelOptions" $ \options -> case dropSuffix "Options" options of
    Nothing -> fail $ T.unpack ("Expected a DBModelOptions but got '" <> options <> "'")
    Just model -> DBModelOptions <$> parseJSON (A.String model)

newtype DBModelObject = DBModelObject {getDBModelObject :: DBModel}
  deriving stock (Show, Generic)

instance FromJSON DBModelObject where
  parseJSON = A.withText "DBModelObject" $ \obj -> case dropSuffix "Object" obj of
    Nothing -> fail $ T.unpack ("Expected a DBModelObject but got '" <> obj <> "'")
    Just model -> DBModelObject <$> parseJSON (A.String model)

-- dropSuffix "tableOptions" = Just "table"
dropSuffix :: Text -> Text -> Maybe Text
dropSuffix suffix str = do
  let l = T.length str - T.length suffix
  if T.drop l str == suffix then Just $ T.take l str else Nothing
