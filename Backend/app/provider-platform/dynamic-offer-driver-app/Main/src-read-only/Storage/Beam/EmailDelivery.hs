{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.EmailDelivery where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.EmailDelivery
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data EmailDeliveryT f = EmailDeliveryT
  { bounceSubType :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    bounceType :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    deliveredAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    failureReason :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    id :: (B.C f Kernel.Prelude.Text),
    lastEventAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    ownerId :: (B.C f Kernel.Prelude.Text),
    ownerType :: (B.C f Domain.Types.EmailDelivery.EmailDeliveryOwnerType),
    provider :: (B.C f (Kernel.Prelude.Maybe Domain.Types.EmailDelivery.EmailDeliveryProvider)),
    providerMessageId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    sentAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    status :: (B.C f Domain.Types.EmailDelivery.EmailDeliveryStatus),
    toAddress :: (B.C f Kernel.Prelude.Text),
    triggeredBy :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table EmailDeliveryT where
  data PrimaryKey EmailDeliveryT f = EmailDeliveryId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = EmailDeliveryId . id

type EmailDelivery = EmailDeliveryT Identity

$(enableKVPG (''EmailDeliveryT) [('id)] [[('ownerId)], [('providerMessageId)]])

$(mkTableInstances (''EmailDeliveryT) "email_delivery")
