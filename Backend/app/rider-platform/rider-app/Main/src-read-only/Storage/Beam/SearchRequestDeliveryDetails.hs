{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.SearchRequestDeliveryDetails where

import qualified BecknV2.OnDemand.Enums
import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import qualified Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data SearchRequestDeliveryDetailsT f = SearchRequestDeliveryDetailsT
  { initiatedAs :: (B.C f BecknV2.OnDemand.Enums.DeliveryInitiation),
    receiverName :: (B.C f Kernel.Prelude.Text),
    receiverPhoneNumberEncrypted :: (B.C f Kernel.Prelude.Text),
    receiverPhoneNumberHash :: (B.C f Kernel.External.Encryption.DbHash),
    searchRequestId :: (B.C f Kernel.Prelude.Text),
    senderName :: (B.C f Kernel.Prelude.Text),
    senderPhoneNumberEncrypted :: (B.C f Kernel.Prelude.Text),
    senderPhoneNumberHash :: (B.C f Kernel.External.Encryption.DbHash),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table SearchRequestDeliveryDetailsT where
  data PrimaryKey SearchRequestDeliveryDetailsT f = SearchRequestDeliveryDetailsId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = SearchRequestDeliveryDetailsId . searchRequestId

type SearchRequestDeliveryDetails = SearchRequestDeliveryDetailsT Identity

$(enableKVPG (''SearchRequestDeliveryDetailsT) [('searchRequestId)] [])

$(mkTableInstances (''SearchRequestDeliveryDetailsT) "search_request_delivery_details")
