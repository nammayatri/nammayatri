{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.DraftTicketChange where

import qualified Data.Aeson
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.DraftTicketChange
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data DraftTicketChangeT f = DraftTicketChangeT
  { createdAt :: B.C f Kernel.Prelude.UTCTime,
    draftPayload :: B.C f (Kernel.Prelude.Maybe Data.Aeson.Value),
    id :: B.C f Kernel.Prelude.Text,
    isApprovalRequired :: B.C f Kernel.Prelude.Bool,
    message :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    status :: B.C f Domain.Types.DraftTicketChange.DraftStatus,
    ticketMerchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    updatedAt :: B.C f Kernel.Prelude.UTCTime,
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)
  }
  deriving (Generic, B.Beamable)

instance B.Table DraftTicketChangeT where
  data PrimaryKey DraftTicketChangeT f = DraftTicketChangeId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = DraftTicketChangeId . id

type DraftTicketChange = DraftTicketChangeT Identity

$(enableKVPG ''DraftTicketChangeT ['id] [])

$(mkTableInstances ''DraftTicketChangeT "draft_ticket_change")
