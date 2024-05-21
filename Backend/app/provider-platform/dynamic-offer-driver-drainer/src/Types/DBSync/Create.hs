{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wwarn=ambiguous-fields #-}

module Types.DBSync.Create where

import DBQuery.Types
import qualified Data.Aeson as A
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import EulerHS.Prelude
import Types.DBSync.DBModel
import Utils.Parse

data DBCreateObject = DBCreateObject
  { dbModel :: DBModel,
    contents :: DBCreateObjectContent,
    mappings :: Mapping,
    contentsObj :: A.Object
  }
  deriving stock (Show)

instance FromJSON DBCreateObject where
  parseJSON = A.withObject "DBCreateObject" $ \o -> do
    A.Array a <- o A..: "contents" -- TODO remove "contents" field after roll out
    contentsObj <- flip (A.withObject "last contents") (V.last a) $ \obj -> do
      -- can we use contents_v2?
      obj A..: "contents"
    contentsV2 <- o A..: "contents_v2"
    command <- contentsV2 A..: "command"
    tagObject :: DBModelObject <- command A..: "tag"
    contents <- command A..: "contents"
    mbMappings <- o A..:? "mappings"
    let mappings = fromMaybe (Mapping M.empty) mbMappings
        dbModel = tagObject.getDBModelObject
    pure DBCreateObject {..}

newtype DBCreateObjectContent = DBCreateObjectContent [TermWrap]
  deriving stock (Show)

instance FromJSON DBCreateObjectContent where
  parseJSON contents = do
    termWarpList <- parseCreateCommandValues contents
    return $ DBCreateObjectContent termWarpList
