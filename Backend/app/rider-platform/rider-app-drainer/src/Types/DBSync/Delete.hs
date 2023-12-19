{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wwarn=ambiguous-fields #-}

module Types.DBSync.Delete where

import DBQuery.Types
import qualified Data.Aeson as A
import qualified Data.Map.Strict as M
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
    contentsV2 <- o A..: "contents_v2"
    command <- contentsV2 A..: "command"
    tagObject :: DBModelOptions <- command A..: "tag"
    contents <- command A..: "contents"
    A.Object obj <- command A..: "contents"
    mbMappings <- o A..:? "mappings"
    whereObj <- obj A..: "clauseContents"
    let mappings = fromMaybe (Mapping M.empty) mbMappings
        dbModel = tagObject.getDBModelOptions
    pure DBDeleteObject {..}

newtype DBDeleteObjectContent = DBDeleteObjectContent Where
  deriving stock (Show)

instance FromJSON DBDeleteObjectContent where
  parseJSON contents = do
    whereClause <- parseDeleteCommandValues contents
    return $ DBDeleteObjectContent whereClause
