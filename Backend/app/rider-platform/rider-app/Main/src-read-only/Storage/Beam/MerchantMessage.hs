{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.MerchantMessage where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Domain.Types.MerchantMessage
import qualified Data.Aeson
import qualified Database.Beam as B



data MerchantMessageT f
    = MerchantMessageT {containsUrlButton :: (B.C f Kernel.Prelude.Bool),
                        createdAt :: (B.C f Kernel.Prelude.UTCTime),
                        jsonData :: (B.C f (Kernel.Prelude.Maybe Data.Aeson.Value)),
                        merchantId :: (B.C f Kernel.Prelude.Text),
                        merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
                        message :: (B.C f Kernel.Prelude.Text),
                        messageKey :: (B.C f Domain.Types.MerchantMessage.MessageKey),
                        messageType :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                        senderHeader :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                        templateId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                        updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table MerchantMessageT
    where data PrimaryKey MerchantMessageT f = MerchantMessageId (B.C f Kernel.Prelude.Text) (B.C f Domain.Types.MerchantMessage.MessageKey) deriving (Generic, B.Beamable)
          primaryKey = MerchantMessageId <$> merchantOperatingCityId <*> messageKey
type MerchantMessage = MerchantMessageT Identity

$(enableKVPG (''MerchantMessageT) [('merchantOperatingCityId), ('messageKey)] [])

$(mkTableInstances (''MerchantMessageT) "merchant_message")

