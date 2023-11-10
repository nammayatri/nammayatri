{-# LANGUAGE DerivingStrategies #-}

module Types.DBSync.Delete where

import DBQuery.Types
import qualified Data.Aeson as A
import qualified Data.Vector as V
import EulerHS.Prelude
import Types.DBSync.DBModel
import Utils.Parse

data DBDeleteObject = DBDeleteObject
  { dbModel :: DBModel,
    contents :: DBDeleteObjectContent,
    mappings :: Mapping,
    whereObj :: A.Value -- required for Kafka
  }
  deriving stock (Show)

instance FromJSON DBDeleteObject where
  parseJSON = A.withObject "DBDeleteObject" $ \o -> do
    A.Array a <- o A..: "contents"
    flip (A.withObject "last contents") (V.last a) $ \obj' -> do
      -- why last?
      A.Object obj <- obj' A..: "contents"
      contents <- obj' A..: "contents"
      tagOptions :: DBModelOptions <- obj' A..: "tag"
      whereObj <- obj A..: "value1"
      mbMappings <- o A..:? "mappings"
      let mappings = fromMaybe (Mapping []) mbMappings
          dbModel = tagOptions.getDBModelOptions
      pure DBDeleteObject {..}

newtype DBDeleteObjectContent = DBDeleteObjectContent Where
  deriving stock (Show)

instance FromJSON DBDeleteObjectContent where
  parseJSON contents = do
    whereClause <- parseDeleteCommandValues contents
    return $ DBDeleteObjectContent whereClause
