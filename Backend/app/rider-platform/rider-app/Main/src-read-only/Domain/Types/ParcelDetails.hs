{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.ParcelDetails where
import Kernel.Prelude
import Data.Aeson
import qualified Kernel.Types.Id
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.ParcelType
import qualified Domain.Types.SearchRequest
import qualified Tools.Beam.UtilsTH



data ParcelDetails
    = ParcelDetails {merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                     merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
                     parcelType :: Domain.Types.ParcelType.ParcelType,
                     quantity :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
                     searchRequestId :: Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest,
                     createdAt :: Kernel.Prelude.UTCTime,
                     updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)



