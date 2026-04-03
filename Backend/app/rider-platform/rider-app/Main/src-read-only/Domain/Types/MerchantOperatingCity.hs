{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.MerchantOperatingCity where
import Kernel.Prelude
import Data.Aeson
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Domain.Types.Merchant
import qualified Tools.Beam.UtilsTH



data MerchantOperatingCity
    = MerchantOperatingCity {city :: Kernel.Types.Beckn.Context.City,
                             country :: Kernel.Types.Beckn.Context.Country,
                             distanceUnit :: Kernel.Types.Common.DistanceUnit,
                             driverOfferMerchantOperatingCityId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                             id :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
                             lat :: Kernel.Prelude.Double,
                             long :: Kernel.Prelude.Double,
                             merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                             merchantShortId :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant,
                             state :: Kernel.Types.Beckn.Context.IndianState,
                             stdCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                             createdAt :: Kernel.Prelude.UTCTime,
                             updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)



