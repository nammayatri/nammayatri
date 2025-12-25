{-# LANGUAGE ApplicativeDo #-}

module Domain.Types.InterCityTravelCities where

import Data.Aeson
import qualified Domain.Types.Merchant
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id

data InterCityTravelCities = InterCityTravelCities
  { cityName :: Kernel.Prelude.Text,
    lat :: Kernel.Prelude.Double,
    lng :: Kernel.Prelude.Double,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    state :: Kernel.Types.Beckn.Context.IndianState,
    geom :: Maybe Text,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
