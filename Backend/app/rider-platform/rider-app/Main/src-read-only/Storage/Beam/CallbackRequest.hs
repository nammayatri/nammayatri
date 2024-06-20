{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.CallbackRequest where

import qualified Database.Beam as B
import qualified Domain.Types.CallbackRequest
import Kernel.External.Encryption
import qualified Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data CallbackRequestT f = CallbackRequestT
  { id :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f Kernel.Prelude.Text,
    customerName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    customerPhoneEncrypted :: B.C f Kernel.Prelude.Text,
    customerPhoneHash :: B.C f Kernel.External.Encryption.DbHash,
    customerMobileCountryCode :: B.C f Kernel.Prelude.Text,
    status :: B.C f Domain.Types.CallbackRequest.CallbackRequestStatus,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table CallbackRequestT where
  data PrimaryKey CallbackRequestT f = CallbackRequestId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = CallbackRequestId . id

type CallbackRequest = CallbackRequestT Identity

$(enableKVPG ''CallbackRequestT ['id] [])

$(mkTableInstances ''CallbackRequestT "callback_request")
