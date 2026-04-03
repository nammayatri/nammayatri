{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.TicketService where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Domain.Types.TicketService
import qualified Data.Time
import qualified Data.Aeson
import qualified Database.Beam as B



data TicketServiceT f
    = TicketServiceT {allowCancellation :: (B.C f Kernel.Prelude.Bool),
                      allowFutureBooking :: (B.C f Kernel.Prelude.Bool),
                      businessHours :: (B.C f [Kernel.Prelude.Text]),
                      expiry :: (B.C f Domain.Types.TicketService.ExpiryType),
                      id :: (B.C f Kernel.Prelude.Text),
                      isClosed :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
                      maxSelection :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                      maxVerification :: (B.C f Kernel.Prelude.Int),
                      note :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                      operationalEndDate :: (B.C f (Kernel.Prelude.Maybe Data.Time.Day)),
                      operationalStartDate :: (B.C f (Kernel.Prelude.Maybe Data.Time.Day)),
                      operationalDays :: (B.C f [Kernel.Prelude.Text]),
                      placesId :: (B.C f Kernel.Prelude.Text),
                      priority :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                      rules :: (B.C f (Kernel.Prelude.Maybe Data.Aeson.Value)),
                      service :: (B.C f Kernel.Prelude.Text),
                      serviceDetails :: (B.C f (Kernel.Prelude.Maybe Data.Aeson.Value)),
                      shortDesc :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                      subPlaceId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                      merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                      merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                      createdAt :: (B.C f Kernel.Prelude.UTCTime),
                      updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table TicketServiceT
    where data PrimaryKey TicketServiceT f = TicketServiceId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = TicketServiceId . id
type TicketService = TicketServiceT Identity

$(enableKVPG (''TicketServiceT) [('id)] [])

$(mkTableInstances (''TicketServiceT) "ticket_service")

