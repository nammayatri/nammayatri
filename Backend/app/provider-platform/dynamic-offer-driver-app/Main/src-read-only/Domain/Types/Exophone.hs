{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.Exophone where
import Kernel.Prelude
import Data.Aeson
import qualified Data.Text
import qualified Kernel.External.Call.Types
import qualified Kernel.Types.Id
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Tools.Beam.UtilsTH



data Exophone
    = Exophone {backupPhone :: Data.Text.Text,
                callService :: Kernel.External.Call.Types.CallService,
                exophoneType :: Domain.Types.Exophone.ExophoneType,
                id :: Kernel.Types.Id.Id Domain.Types.Exophone.Exophone,
                isPrimaryDown :: Kernel.Prelude.Bool,
                merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
                primaryPhone :: Data.Text.Text,
                createdAt :: Kernel.Prelude.UTCTime,
                updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
data ExophoneType = CALL_RIDE | END_RIDE | CALL_DELIVERY_SENDER | CALL_DELIVERY_RECEIVER deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''ExophoneType))

