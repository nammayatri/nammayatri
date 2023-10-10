module Types.DBSync.Delete where

import Data.Aeson as A
import Database.Beam.Postgres (Postgres)
import EulerHS.Prelude
import Sequelize
import Types.DBSync.DBModel
import Utils.Parse

newtype DBDeleteObjectContent (table :: TableK) = DBDeleteObjectContent (Where Postgres table)

type DBDeleteObject = DBObject DBDeleteObjectContent

instance FromJSON DBDeleteObject where
  parseJSON = A.withObject "DBDeleteObject" $ \o -> do
    contents <- o .: "contents"
    dbModelOptions :: DBModelOptions <- o .: "tag"
    buildDBObject @DBDeleteObjectContent dbModelOptions.getDBModelOptions $ do
      whereClause <- parseDeleteCommandValues contents
      return $ DBDeleteObjectContent whereClause
