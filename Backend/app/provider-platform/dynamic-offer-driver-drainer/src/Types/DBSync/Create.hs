{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Types.DBSync.Create where

import Data.Aeson as A
import Kernel.Prelude
import Types.DBSync.DBModel

newtype DBCreateObjectContent (table :: TableK) = DBCreateObjectContent (table Identity)
  deriving (Generic)

type DBCreateObject = DBObject DBCreateObjectContent

-- TODO test
deriving newtype instance ToJSON (table Identity) => ToJSON (DBCreateObjectContent table)

deriving newtype instance FromJSON (table Identity) => FromJSON (DBCreateObjectContent table)

instance FromJSON DBCreateObject

instance ToJSON DBCreateObject
