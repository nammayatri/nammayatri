{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Clickhouse.BookingCancellationReason where

import Control.Lens ((^?), _head)
import qualified Domain.Types.Booking as DB
import qualified Domain.Types.BookingCancellationReason as DBCR
import qualified Domain.Types.CancellationReason as CR
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DR
import Kernel.Prelude
import Kernel.Storage.ClickhouseV2 as CH
import qualified Kernel.Storage.ClickhouseV2.UtilsTH as TH
import Kernel.Types.Id

data BookingCancellationReasonT f = BookingCancellationReasonT
  { additionalInfo :: C f (Maybe Text),
    bookingId :: C f (Id DB.Booking),
    date :: C f (Maybe UTCTime),
    merchantId :: C f (Maybe (Id DM.Merchant)),
    riderId :: C f (Maybe (Id DP.Person)),
    reasonCode :: C f (Maybe CR.CancellationReasonCode),
    reasonStage :: C f (Maybe CR.CancellationStage),
    rideId :: C f (Maybe (Id DR.Ride)),
    source :: C f DBCR.CancellationSource
  }
  deriving (Generic)

deriving instance Show BookingCancellationReason

bookingCancellationReasonTTable :: BookingCancellationReasonT (FieldModification BookingCancellationReasonT)
bookingCancellationReasonTTable =
  BookingCancellationReasonT
    { additionalInfo = "additional_info",
      bookingId = "booking_id",
      date = "date",
      riderId = "rider_id",
      merchantId = "merchant_id",
      reasonCode = "reason_code",
      reasonStage = "reason_stage",
      rideId = "ride_id",
      source = "source"
    }

type BookingCancellationReason = BookingCancellationReasonT Identity

instance CH.ClickhouseValue CR.CancellationReasonCode where
  fromClickhouseValue = parseAsEnum @CR.CancellationReasonCode

instance CH.ClickhouseValue CR.CancellationStage where
  fromClickhouseValue = parseAsEnum @CR.CancellationStage

instance CH.ClickhouseValue DBCR.CancellationSource where
  fromClickhouseValue = parseAsEnum @DBCR.CancellationSource

$(TH.mkClickhouseInstances ''BookingCancellationReasonT 'NO_SELECT_MODIFIER)

countCancelledBookingsByRiderIdGroupByByUserAndDriver ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  Id DP.Person ->
  UTCTime ->
  m (Int, Int)
countCancelledBookingsByRiderIdGroupByByUserAndDriver riderId createdAt = do
  res <-
    CH.findAll $
      CH.select_
        ( \bookingCancellationReason -> do
            let userCount = CH.count_ (CH.distinct bookingCancellationReason.bookingId)
            let driverCount = CH.count_ (CH.distinct bookingCancellationReason.bookingId)
            CH.groupBy (bookingCancellationReason.source) $ \source -> do
              (source, userCount, driverCount)
        )
        $ CH.selectModifierOverride CH.NO_SELECT_MODIFIER $
          CH.filter_
            ( \bookingCancellationReason ->
                bookingCancellationReason.date >=. Just createdAt
                  CH.&&. bookingCancellationReason.riderId CH.==. Just riderId
            )
            (CH.all_ @CH.APP_SERVICE_CLICKHOUSE bookingCancellationReasonTTable)
  let userCount = fromMaybe 0 $ (map (\(_, a, _) -> a) $ filter (\(a, _, _) -> a == DBCR.ByUser) res) ^? _head
  let driverCount = fromMaybe 0 $ (map (\(_, _, a) -> a) $ filter (\(a, _, _) -> a == DBCR.ByDriver) res) ^? _head
  pure (userCount, driverCount)
