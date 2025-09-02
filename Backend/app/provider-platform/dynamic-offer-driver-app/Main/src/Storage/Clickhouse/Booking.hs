module Storage.Clickhouse.Booking where

import qualified Domain.Types.Booking as DB
import qualified Domain.Types.Location as DL
import Kernel.Prelude as P
import Kernel.Storage.ClickhouseV2 as CH
import qualified Kernel.Storage.ClickhouseV2.UtilsTH as TH
import Kernel.Types.Id

data BookingT f = BookingT
  { id :: C f (Id DB.Booking),
    createdAt :: C f UTCTime,
    fromLocationId :: C f (Maybe (Id DL.Location)),
    toLocationId :: C f (Maybe (Id DL.Location))
  }
  deriving (Generic)

deriving instance Show Booking

-- TODO move to TH (quietSnake)
bookingTTable :: BookingT (FieldModification BookingT)
bookingTTable =
  BookingT
    { id = "id",
      createdAt = "created_at",
      fromLocationId = "from_location_id",
      toLocationId = "to_location_id"
    }

type Booking = BookingT Identity

$(TH.mkClickhouseInstances ''BookingT 'SELECT_FINAL_MODIFIER)

findById ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  Id DB.Booking ->
  m (Maybe Booking)
findById bookingId = do
  res <-
    CH.findAll $
      CH.select $
        CH.filter_
          ( \booking ->
              booking.id CH.==. bookingId
          )
          (CH.all_ @CH.APP_SERVICE_CLICKHOUSE bookingTTable)
  pure $ listToMaybe res
