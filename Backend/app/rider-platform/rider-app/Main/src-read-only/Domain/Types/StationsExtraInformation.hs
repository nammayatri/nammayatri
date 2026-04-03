{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.StationsExtraInformation where
import Kernel.Prelude
import Data.Aeson
import qualified Kernel.Types.Id
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.StationType
import qualified Tools.Beam.UtilsTH



data StationsExtraInformation
    = StationsExtraInformation {address :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                                createdAt :: Kernel.Prelude.UTCTime,
                                id :: Kernel.Types.Id.Id Domain.Types.StationsExtraInformation.StationsExtraInformation,
                                merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                                merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
                                stationId :: Kernel.Prelude.Text,
                                suggestedDestinations :: Kernel.Prelude.Maybe [Domain.Types.StationType.SuggestedStations],
                                updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)



