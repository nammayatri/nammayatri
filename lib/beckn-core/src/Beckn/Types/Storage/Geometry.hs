{-# LANGUAGE StandaloneDeriving #-}

module Beckn.Types.Storage.Geometry where

import Data.Aeson
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)

data GeometryT f = Geometry
  { id :: B.C f Int,
    region :: B.C f Text
  }
  deriving (Generic, B.Beamable)

type Geometry = GeometryT Identity

type GeometryPrimaryKey = B.PrimaryKey GeometryT Identity

instance B.Table GeometryT where
  data PrimaryKey GeometryT f = GeometryPrimaryKey (B.C f Int)
    deriving (Generic, B.Beamable)
  primaryKey = GeometryPrimaryKey . id

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
      { id = "id",
        region = "region"
      }
