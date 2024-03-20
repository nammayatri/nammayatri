{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wwarn=ambiguous-fields #-}

module Types.DBSync.Update where

import DBQuery.Types
import qualified Data.Aeson as A
import qualified Data.Map.Strict as M
import EulerHS.Prelude
import Types.DBSync.DBModel
import Utils.Parse

-- Each update option contains a list of (key, value) pairs to set during
-- the update and a list of (key, value) pairs to use as a where clause.

data DBUpdateObject = DBUpdateObject
  { dbModel :: DBModel,
    contents :: DBUpdateObjectContent,
    mappings :: Mapping,
    updatedModel :: Maybe A.Object -- required for Kafka
  }
  deriving stock (Show)

instance FromJSON DBUpdateObject where
  parseJSON = A.withObject "DBUpdateObject" $ \o -> do
    contentsV2 <- o A..: "contents_v2"
    command <- contentsV2 A..: "command"
    tagObject :: DBModelOptions <- command A..: "tag"
    contents <- command A..: "contents"
    mbMappings <- o A..:? "mappings"
    updatedModel <- o A..:? "updatedModel"
    let mappings = fromMaybe (Mapping M.empty) mbMappings
        dbModel = tagObject.getDBModelOptions
    pure DBUpdateObject {..}

data DBUpdateObjectContent = DBUpdateObjectContent [Set] Where
  deriving stock (Show)

instance FromJSON DBUpdateObjectContent where
  parseJSON contents = do
    (updVals, whereClause) <- parseUpdateCommandValues contents
    return $ DBUpdateObjectContent updVals whereClause
