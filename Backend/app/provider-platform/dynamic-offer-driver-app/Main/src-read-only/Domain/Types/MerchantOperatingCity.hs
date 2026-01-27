{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.MerchantOperatingCity where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Kernel.External.Maps.Types
import qualified Kernel.External.Types
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data MerchantOperatingCity = MerchantOperatingCity
  { city :: Kernel.Types.Beckn.Context.City,
    country :: Kernel.Types.Beckn.Context.Country,
    currency :: Kernel.Types.Common.Currency,
    distanceUnit :: Kernel.Types.Common.DistanceUnit,
    id :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    language :: Kernel.External.Types.Language,
    location :: Kernel.External.Maps.Types.LatLong,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantShortId :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant,
    state :: Kernel.Types.Beckn.Context.IndianState,
    stdCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    supportNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, Eq, ToSchema)
