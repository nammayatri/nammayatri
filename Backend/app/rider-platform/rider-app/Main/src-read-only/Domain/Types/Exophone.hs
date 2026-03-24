{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.Exophone where
import Kernel.Prelude
import Data.Aeson
import qualified Kernel.External.Call.Types
import qualified Kernel.Types.Id
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Tools.Beam.UtilsTH



data Exophone
    = Exophone {backupPhone :: Kernel.Prelude.Text,
                callService :: Kernel.External.Call.Types.CallService,
                createdAt :: Kernel.Prelude.UTCTime,
                enableAlternateNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
                id :: Kernel.Types.Id.Id Domain.Types.Exophone.Exophone,
                isPrimaryDown :: Kernel.Prelude.Bool,
                merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
                primaryPhone :: Kernel.Prelude.Text,
                updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)



