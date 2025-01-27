{-# LANGUAGE StandaloneDeriving #-}

module Storage.Beam.InterCityTravelCities.InterCityTravelCitiesGeom where

import qualified Database.Beam as B
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import Tools.Beam.UtilsTH

data InterCityTravelCitiesGeomT f = InterCityTravelCitiesGeomT
  { cityName :: B.C f Kernel.Prelude.Text,
    lat :: B.C f Kernel.Prelude.Double,
    lng :: B.C f Kernel.Prelude.Double,
    merchantId :: B.C f Kernel.Prelude.Text,
    state :: B.C f Kernel.Types.Beckn.Context.IndianState,
    geom :: B.C f (Maybe Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table InterCityTravelCitiesGeomT where
  data PrimaryKey InterCityTravelCitiesGeomT f = InterCityTravelCitiesId (B.C f Kernel.Prelude.Text) (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = InterCityTravelCitiesId <$> cityName <*> merchantId

type InterCityTravelCitiesGeom = InterCityTravelCitiesGeomT Identity

$(enableKVPG ''InterCityTravelCitiesGeomT ['cityName, 'merchantId] [])

$(mkTableInstances ''InterCityTravelCitiesGeomT "inter_city_travel_cities")
