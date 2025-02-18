{-# LANGUAGE ApplicativeDo #-}

module Domain.Types.Extra.Location where

import Data.Aeson
import EulerHS.Prelude hiding (id, state)
import Kernel.Prelude
import Kernel.Types.Common (Meters (..))
import Kernel.Utils.GenericPretty

data DummyLocationInfo = DummyLocationInfo
  { dummyId :: Text,
    lat :: Double,
    lon :: Double,
    distance :: Meters,
    baseFare :: Int,
    door :: Maybe Text,
    building :: Maybe Text,
    street :: Maybe Text,
    area :: Maybe Text,
    city :: Maybe Text,
    state :: Maybe Text,
    country :: Maybe Text,
    areaCode :: Maybe Text,
    fullAddress :: Maybe Text,
    instructions :: Maybe Text,
    extras :: Maybe Text
  }
  deriving (Generic, Show, Read, Eq, PrettyShow, ToJSON, FromJSON, ToSchema)

dummyFromLocationData :: DummyLocationInfo
dummyFromLocationData =
  DummyLocationInfo
    { dummyId = "dummyFromLocation",
      lat = 12.9421783,
      lon = 77.62205,
      distance = Meters 1000,
      baseFare = 40,
      door = Just "817",
      building = Just "20th Main Rd",
      street = Just "Koramangala 8th Block",
      area = Just "Koramangala, Koramangala 8th Block, 20th Main Rd",
      city = Just "Bengaluru",
      state = Just "Karnataka 560095",
      country = Just "India",
      areaCode = Just "560095",
      fullAddress = Just "817, 20th Main Rd, Koramangala 8th Block, Koramangala, Bengaluru, Karnataka 560095, 560095, India",
      instructions = Just "Beware of the cruel world",
      extras = Just "Landmark: Near Apple Signal"
    }

dummyToLocationData :: DummyLocationInfo
dummyToLocationData =
  DummyLocationInfo
    { dummyId = "dummyToLocation",
      lat = 12.938797,
      lon = 77.624116,
      distance = Meters 1000,
      baseFare = 0,
      door = Just "831",
      building = Just "17th F Main Rd",
      street = Just "6th Block",
      area = Just "Koramangala, 6th Block",
      city = Just "Bengaluru",
      state = Just "Karnataka 560095",
      country = Just "India",
      areaCode = Just "560095",
      fullAddress = Just "Rohit 17th F Main Rd, 6th Block, Koramangala, Bengaluru, Karnataka 560095, 560095, India",
      instructions = Just "Beware of dogs",
      extras = Just "Landmark: Near Sony Signal"
    }
