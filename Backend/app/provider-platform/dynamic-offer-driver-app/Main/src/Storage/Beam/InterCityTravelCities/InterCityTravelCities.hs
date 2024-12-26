{-# LANGUAGE StandaloneDeriving #-}

module Storage.Beam.InterCityTravelCities.InterCityTravelCities where

import qualified Database.Beam as B
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import Tools.Beam.UtilsTH

data InterCityTravelCitiesT f = InterCityTravelCitiesT
  { cityName :: B.C f Kernel.Prelude.Text,
    lat :: B.C f Kernel.Prelude.Double,
    lng :: B.C f Kernel.Prelude.Double,
    merchantId :: B.C f Kernel.Prelude.Text,
    state :: B.C f Kernel.Types.Beckn.Context.IndianState,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table InterCityTravelCitiesT where
  data PrimaryKey InterCityTravelCitiesT f = InterCityTravelCitiesId (B.C f Kernel.Prelude.Text) (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = InterCityTravelCitiesId <$> cityName <*> merchantId

type InterCityTravelCities = InterCityTravelCitiesT Identity

$(enableKVPG ''InterCityTravelCitiesT ['cityName, 'merchantId] [])

$(mkTableInstances ''InterCityTravelCitiesT "inter_city_travel_cities")
