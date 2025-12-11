{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.MerchantMessage where

import qualified Data.Aeson
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.MerchantMessage
import Kernel.External.Encryption
import qualified Kernel.External.Types
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data MerchantMessageT f = MerchantMessageT
  { containsUrlButton :: B.C f Kernel.Prelude.Bool,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    jsonData :: B.C f (Kernel.Prelude.Maybe Data.Aeson.Value),
    language :: B.C f (Kernel.Prelude.Maybe Kernel.External.Types.Language),
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    message :: B.C f Kernel.Prelude.Text,
    messageKey :: B.C f Domain.Types.MerchantMessage.MessageKey,
    senderHeader :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    templateId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table MerchantMessageT where
  data PrimaryKey MerchantMessageT f = MerchantMessageId (B.C f Kernel.Prelude.Text) (B.C f Domain.Types.MerchantMessage.MessageKey) deriving (Generic, B.Beamable)
  primaryKey = MerchantMessageId <$> merchantOperatingCityId <*> messageKey

type MerchantMessage = MerchantMessageT Identity

$(enableKVPG ''MerchantMessageT ['merchantOperatingCityId, 'messageKey] [])

$(mkTableInstances ''MerchantMessageT "merchant_message")
