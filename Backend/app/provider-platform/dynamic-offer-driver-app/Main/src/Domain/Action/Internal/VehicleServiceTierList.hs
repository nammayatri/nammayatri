module Domain.Action.Internal.VehicleServiceTierList
  ( VehicleServiceTierInfo (..),
    getVehicleServiceTiers,
  )
where

import Domain.Types.Common (ServiceTierType)
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.VehicleServiceTier as DVST
import Environment (Flow)
import Kernel.Prelude
import Kernel.Types.Beckn.Context (City)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.VehicleServiceTier as CQVST
import Tools.Error

data VehicleServiceTierInfo = VehicleServiceTierInfo
  { serviceTierType :: ServiceTierType,
    serviceTierName :: Text,
    vehicleIconUrl :: Maybe Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

getVehicleServiceTiers :: Id DM.Merchant -> City -> Maybe Text -> Flow [VehicleServiceTierInfo]
getVehicleServiceTiers merchantId city apiKey = do
  merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantDoesNotExist merchantId.getId)
  unless (Just merchant.internalApiKey == apiKey) $
    throwError $ AuthBlocked "Invalid BPP internal api key"
  merchantOpCity <-
    CQMOC.findByMerchantIdAndCity merchantId city
      >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantId: " <> merchantId.getId <> " ,city: " <> show city)
  tiers <- CQVST.findAllByMerchantOpCityId merchantOpCity.id Nothing
  pure $ map mkVehicleServiceTierInfo tiers

mkVehicleServiceTierInfo :: DVST.VehicleServiceTier -> VehicleServiceTierInfo
mkVehicleServiceTierInfo tier =
  VehicleServiceTierInfo
    { serviceTierType = tier.serviceTierType,
      serviceTierName = tier.name,
      vehicleIconUrl = showBaseUrl <$> tier.vehicleIconUrl
    }
