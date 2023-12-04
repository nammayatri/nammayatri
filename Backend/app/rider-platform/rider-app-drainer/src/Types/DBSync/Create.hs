{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

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
    A.Array a <- o A..: "contents"
    flip (A.withObject "last contents") (V.last a) $ \obj -> do
      -- why last?
      tagObject :: DBModelObject <- obj A..: "tag"
      contentsObj <- obj A..: "contents"
      contents <- obj A..: "contents_v2" -- TODO remove "contents" field after roll out
      mbMappings <- obj A..:? "mappings"
      let mappings = fromMaybe (Mapping M.empty) mbMappings
          dbModel = tagObject.getDBModelObject
      pure DBCreateObject {..}

newtype DBCreateObjectContent = DBCreateObjectContent [TermWrap]
  deriving stock (Show)

instance FromJSON DBCreateObjectContent where
  parseJSON contents = do
    termWarpList <- parseCreateCommandValues contents
    return $ DBCreateObjectContent termWarpList
