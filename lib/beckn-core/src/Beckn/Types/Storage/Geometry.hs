{-# LANGUAGE StandaloneDeriving #-}

module Beckn.Types.Storage.Geometry where

import Data.Aeson
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)

data GeometryT f = Geometry
  { _id :: B.C f Int,
    _region :: B.C f Text
  }
  deriving (Generic, B.Beamable)

type Geometry = GeometryT Identity

type GeometryPrimaryKey = B.PrimaryKey GeometryT Identity

instance B.Table GeometryT where
  data PrimaryKey GeometryT f = GeometryPrimaryKey (B.C f Int)
    deriving (Generic, B.Beamable)
  primaryKey = GeometryPrimaryKey . _id

deriving instance Show Geometry

instance ToJSON Geometry where
  toJSON = genericToJSON defaultOptions

instance FromJSON Geometry where
  parseJSON = genericParseJSON defaultOptions

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity GeometryT)
fieldEMod =
  B.modifyTableFields
    B.tableModification
      { _id = "id",
        _region = "region"
      }
