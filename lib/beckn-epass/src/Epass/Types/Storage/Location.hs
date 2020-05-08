{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}


module Epass.Types.Storage.Location where

import           Data.Aeson
import           Data.Time
import           EulerHS.Prelude

import qualified Database.Beam   as B

data Point = Point
  { _lat  :: Double
  , _long :: Double
  }
  deriving (Generic)

data Bound = Bound [Point] deriving (Generic)

data LocationT f =
  Location
    { _id        :: B.C f Text
    , _type      :: B.C f Text
    , _lat       :: B.C f Double
    , _long      :: B.C f Double
    , _bound     :: B.C f Value
    , _district  :: B.C f Text
    , _city      :: B.C f Text
    , _state     :: B.C f Text
    , _country   :: B.C f Text
    , _ward      :: B.C f Text
    , _pincode   :: B.C f Int
    , _address   :: B.C f Text
    , _info      :: B.C f Text
    , _createdAt :: B.C f LocalTime
    , _updatedAt :: B.C f LocalTime
    }
  deriving (Generic, B.Beamable)

type Location = LocationT Identity

type LocationPrimaryKey = B.PrimaryKey LocationT Identity

instance B.Table LocationT where
  data PrimaryKey LocationT f = LocationPrimaryKey (B.C f Text)
                               deriving (Generic, B.Beamable)
  primaryKey = LocationPrimaryKey . _id

deriving instance Show Location

deriving instance Eq Location

deriving instance ToJSON Location

deriving instance FromJSON Location

fieldEMod ::
     B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity LocationT)
fieldEMod =
  B.modifyTableFields
    B.tableModification
      { _createdAt = "created_at"
      , _updatedAt = "updated_at"
      }


