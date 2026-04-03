{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.MessageDictionary where
import Kernel.Prelude
import Data.Aeson
import qualified Kernel.Types.Id
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Tools.Beam.UtilsTH



data MessageDictionary
    = MessageDictionary {id :: Kernel.Types.Id.Id Domain.Types.MessageDictionary.MessageDictionary,
                         merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                         merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
                         messageKey :: Kernel.Prelude.Text,
                         messageType :: Domain.Types.MessageDictionary.MessageType,
                         createdAt :: Kernel.Prelude.UTCTime,
                         updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
data MessageType = CancellationReason deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''MessageType))

