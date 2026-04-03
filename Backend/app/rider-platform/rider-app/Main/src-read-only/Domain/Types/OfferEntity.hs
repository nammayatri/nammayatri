{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.OfferEntity where
import Kernel.Prelude
import Data.Aeson
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Kernel.Beam.Lib.UtilsTH
import qualified Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH



data OfferEntity
    = OfferEntity {amountSaved :: Kernel.Types.Common.HighPrecMoney,
                   createdAt :: Kernel.Prelude.UTCTime,
                   discountAmount :: Kernel.Types.Common.HighPrecMoney,
                   entityId :: Kernel.Prelude.Text,
                   entityType :: Domain.Types.OfferEntity.EntityType,
                   id :: Kernel.Types.Id.Id Domain.Types.OfferEntity.OfferEntity,
                   merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                   merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
                   offerCode :: Kernel.Prelude.Text,
                   offerDescription :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                   offerId :: Kernel.Prelude.Text,
                   offerSponsoredBy :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                   offerTitle :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                   offerTnc :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                   payoutAmount :: Kernel.Types.Common.HighPrecMoney,
                   postOfferAmount :: Kernel.Types.Common.HighPrecMoney,
                   updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, ( Show))
data EntityType = BOOKING | RIDE deriving (Show, ( Eq), ( Ord), ( Read), ( Generic), ( ToJSON), ( FromJSON), ( ToSchema), ( Kernel.Prelude.ToParamSchema))

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnum (''EntityType))

$(Kernel.Utils.TH.mkFromHttpInstanceForEnum (''EntityType))

