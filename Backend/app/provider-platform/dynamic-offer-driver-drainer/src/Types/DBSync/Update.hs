{-# LANGUAGE DerivingStrategies #-}

-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types.DBSync.Update where

import DBQuery.Types
import qualified Data.Aeson as A
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
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
    A.Array a <- o A..: "contents"
    flip (A.withObject "last contents") (V.last a) $ \obj -> do
      -- why last?
      tagOptions :: DBModelOptions <- obj A..: "tag"
      contents <- obj A..: "contents"
      mbMappings <- o A..:? "mappings"
      updatedModel <- o A..:? "updatedModel"
      let mappings = fromMaybe (Mapping M.empty) mbMappings
          dbModel = tagOptions.getDBModelOptions
      pure DBUpdateObject {..}

data DBUpdateObjectContent = DBUpdateObjectContent [Set] Where
  deriving stock (Show)

instance FromJSON DBUpdateObjectContent where
  parseJSON contents = do
    (updVals, whereClause) <- parseUpdateCommandValues contents
    return $ DBUpdateObjectContent updVals whereClause
