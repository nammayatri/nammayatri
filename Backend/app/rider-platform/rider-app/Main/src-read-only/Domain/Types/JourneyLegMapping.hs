{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.JourneyLegMapping where
import Kernel.Prelude
import Data.Aeson
import qualified Kernel.Types.Id
import qualified Domain.Types.Journey
import qualified Domain.Types.JourneyLeg
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Tools.Beam.UtilsTH



data JourneyLegMapping
    = JourneyLegMapping {id :: Kernel.Types.Id.Id Domain.Types.JourneyLegMapping.JourneyLegMapping,
                         isDeleted :: Kernel.Prelude.Bool,
                         journeyId :: Kernel.Types.Id.Id Domain.Types.Journey.Journey,
                         journeyLegId :: Kernel.Types.Id.Id Domain.Types.JourneyLeg.JourneyLeg,
                         merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                         merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
                         sequenceNumber :: Kernel.Prelude.Int,
                         createdAt :: Kernel.Prelude.UTCTime,
                         updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)



