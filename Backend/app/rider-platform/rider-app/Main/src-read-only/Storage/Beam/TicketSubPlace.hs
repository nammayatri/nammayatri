{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.TicketSubPlace where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Domain.Types.TicketSubPlace
import qualified Data.Aeson
import qualified Database.Beam as B



data TicketSubPlaceT f
    = TicketSubPlaceT {createdAt :: (B.C f Kernel.Prelude.UTCTime),
                       description :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                       enforcedTicketPlaceId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                       id :: (B.C f Kernel.Prelude.Text),
                       isActive :: (B.C f Kernel.Prelude.Bool),
                       name :: (B.C f Kernel.Prelude.Text),
                       rules :: (B.C f (Kernel.Prelude.Maybe Data.Aeson.Value)),
                       subPlaceType :: (B.C f Domain.Types.TicketSubPlace.SubPlaceType),
                       ticketPlaceId :: (B.C f Kernel.Prelude.Text),
                       updatedAt :: (B.C f Kernel.Prelude.UTCTime),
                       merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                       merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text)))}
    deriving (Generic, B.Beamable)
instance B.Table TicketSubPlaceT
    where data PrimaryKey TicketSubPlaceT f = TicketSubPlaceId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = TicketSubPlaceId . id
type TicketSubPlace = TicketSubPlaceT Identity

$(enableKVPG (''TicketSubPlaceT) [('id)] [[('ticketPlaceId)]])

$(mkTableInstances (''TicketSubPlaceT) "ticket_sub_place")

