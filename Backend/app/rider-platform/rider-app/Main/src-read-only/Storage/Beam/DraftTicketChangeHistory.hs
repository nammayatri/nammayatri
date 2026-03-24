{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.DraftTicketChangeHistory where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Domain.Types.DraftTicketChange
import qualified Data.Aeson
import qualified Database.Beam as B



data DraftTicketChangeHistoryT f
    = DraftTicketChangeHistoryT {createdAt :: (B.C f Kernel.Prelude.UTCTime),
                                 draftPayload :: (B.C f (Kernel.Prelude.Maybe Data.Aeson.Value)),
                                 id :: (B.C f Kernel.Prelude.Text),
                                 isApprovalRequired :: (B.C f Kernel.Prelude.Bool),
                                 message :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                                 reviewedBy :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                                 status :: (B.C f Domain.Types.DraftTicketChange.DraftStatus),
                                 ticketMerchantId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                                 ticketPlaceId :: (B.C f Kernel.Prelude.Text),
                                 updatedAt :: (B.C f Kernel.Prelude.UTCTime),
                                 merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                                 merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text)))}
    deriving (Generic, B.Beamable)
instance B.Table DraftTicketChangeHistoryT
    where data PrimaryKey DraftTicketChangeHistoryT f = DraftTicketChangeHistoryId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = DraftTicketChangeHistoryId . id
type DraftTicketChangeHistory = DraftTicketChangeHistoryT Identity

$(enableKVPG (''DraftTicketChangeHistoryT) [('id)] [])

$(mkTableInstances (''DraftTicketChangeHistoryT) "draft_ticket_change_history")

