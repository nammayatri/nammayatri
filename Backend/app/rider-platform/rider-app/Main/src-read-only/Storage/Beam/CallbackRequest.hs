{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.CallbackRequest where

import qualified Database.Beam as B
import qualified Domain.Types.CallbackRequest
import Domain.Types.Common ()
import Kernel.External.Encryption
import qualified Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data CallbackRequestT f = CallbackRequestT
  { createdAt :: B.C f Kernel.Prelude.UTCTime,
    customerMobileCountryCode :: B.C f Kernel.Prelude.Text,
    customerName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    customerPhoneEncrypted :: B.C f Kernel.Prelude.Text,
    customerPhoneHash :: B.C f Kernel.External.Encryption.DbHash,
    id :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f Kernel.Prelude.Text,
    status :: B.C f Domain.Types.CallbackRequest.CallbackRequestStatus,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table CallbackRequestT where
  data PrimaryKey CallbackRequestT f = CallbackRequestId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = CallbackRequestId . id

type CallbackRequest = CallbackRequestT Identity

$(enableKVPG ''CallbackRequestT ['id] [])

$(mkTableInstances ''CallbackRequestT "callback_request")
