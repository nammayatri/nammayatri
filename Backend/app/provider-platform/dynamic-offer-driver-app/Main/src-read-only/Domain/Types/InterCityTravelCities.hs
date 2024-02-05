{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.InterCityTravelCities where

import qualified Domain.Types.Merchant
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data InterCityTravelCities = InterCityTravelCities
  { cityName :: Kernel.Prelude.Text,
    lat :: Kernel.Prelude.Double,
    lng :: Kernel.Prelude.Double,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    state :: Kernel.Types.Beckn.Context.IndianState,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
