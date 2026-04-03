{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.CrisRecon where
import Kernel.Prelude
import Data.Aeson
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Tools.Beam.UtilsTH



data CrisRecon
    = CrisRecon {bppOrderId :: Kernel.Prelude.Text,
                 createdAt :: Kernel.Prelude.UTCTime,
                 dateIst :: Kernel.Prelude.Text,
                 fareAmount :: Kernel.Types.Common.HighPrecMoney,
                 id :: Kernel.Types.Id.Id Domain.Types.CrisRecon.CrisRecon,
                 updatedAt :: Kernel.Prelude.UTCTime,
                 merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
                 merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity)}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)



