{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.TicketBookingPeopleCategory where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Data.Aeson
import qualified Database.Beam as B



data TicketBookingPeopleCategoryT f
    = TicketBookingPeopleCategoryT {amountToRefund :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
                                    id :: (B.C f Kernel.Prelude.Text),
                                    name :: (B.C f Kernel.Prelude.Text),
                                    numberOfUnits :: (B.C f Kernel.Prelude.Int),
                                    numberOfUnitsCancelled :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
                                    peopleCategoryId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                                    currency :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency)),
                                    pricePerUnit :: (B.C f Kernel.Types.Common.HighPrecMoney),
                                    ticketBookingServiceCategoryId :: (B.C f Kernel.Prelude.Text),
                                    vendorSplitDetails :: (B.C f (Kernel.Prelude.Maybe Data.Aeson.Value)),
                                    merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                                    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
                                    createdAt :: (B.C f Kernel.Prelude.UTCTime),
                                    updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table TicketBookingPeopleCategoryT
    where data PrimaryKey TicketBookingPeopleCategoryT f = TicketBookingPeopleCategoryId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = TicketBookingPeopleCategoryId . id
type TicketBookingPeopleCategory = TicketBookingPeopleCategoryT Identity

$(enableKVPG (''TicketBookingPeopleCategoryT) [('id)] [])

$(mkTableInstances (''TicketBookingPeopleCategoryT) "ticket_booking_people_category")

