module Types.DBSync.Update where

import Data.Aeson as A
import Database.Beam.Postgres (Postgres)
import EulerHS.Prelude
import Sequelize
import Types.DBSync.DBModel
import Utils.Parse

-- Each update option contains a list of (key, value) pairs to set during
-- the update and a list of (key, value) pairs to use as a where clause.

data DBUpdateObjectContent table = DBUpdateObjectContent [Set Postgres table] (Where Postgres table)

type DBUpdateObject = DBObject DBUpdateObjectContent

instance FromJSON DBUpdateObject where
  parseJSON = A.withObject "DBUpdateObject" $ \o -> do
    contents <- o .: "contents"
    dbModelOptions :: DBModelOptions <- o .: "tag"
    buildDBObject dbModelOptions.getDBModel $ do
      (updVals, whereClause) <- parseUpdateCommandValues contents
      pure $ DBUpdateObjectContent updVals whereClause
