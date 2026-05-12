{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.RiderPreferences (module Domain.Types.RiderPreferences, module ReExport) where

import Data.Aeson
import Domain.Types.Extra.RiderPreferences as ReExport
import qualified Domain.Types.Extra.RiderPreferences
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data RiderPreferences = RiderPreferences
  { createdAt :: Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Domain.Types.RiderPreferences.RiderPreferences,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    preferenceData :: Domain.Types.Extra.RiderPreferences.PreferenceData,
    preferenceType :: Domain.Types.Extra.RiderPreferences.PreferenceType,
    riderId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
