module Domain.Action.UI.Invoice (getInvoice, getSourceAndDestination, notAvailableText) where

import qualified API.Types.UI.Invoice
import Control.Monad (msum)
import Data.Time (UTCTime (UTCTime, utctDay), getCurrentTime)
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Kernel.Types.Distance
  ( DistanceUnit (Meter),
    Meters (..),
    convertHighPrecMetersToDistance,
    metersToHighPrecMeters,
  )
import Kernel.Types.Error (PersonError (PersonNotFound))
import qualified Kernel.Types.Id
import Kernel.Utils.Common (fromMaybeM)
import qualified Storage.Clickhouse.Booking as CHB
import qualified Storage.Clickhouse.Location as CHL
import qualified Storage.Clickhouse.Ride as CHR
import qualified Storage.Clickhouse.RideDetails as CHRD
import qualified Storage.Queries.Person as QP

getInvoice ::
  ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
  ) ->
  Kernel.Prelude.Maybe UTCTime ->
  Kernel.Prelude.Maybe UTCTime ->
  Kernel.Prelude.Maybe Kernel.Prelude.Text ->
  Environment.Flow [API.Types.UI.Invoice.InvoiceRes]
getInvoice (mbPersonId, _merchantId, _merchantOpCityId) mbFromDate mbToDate mbRcNo = do
  driver <- traverse QP.findById mbPersonId >>= fromMaybeM (PersonNotFound $ show mbPersonId) . join
  now <- liftIO getCurrentTime
  let defaultFrom = UTCTime (utctDay now) 0
      fromDate = fromMaybe defaultFrom mbFromDate
      toDate = fromMaybe now mbToDate
  rideLs <- CHR.getAllCompletedRidesByDriverId driver.id fromDate toDate
  catMaybes <$> mapM (makeInvoiceResponse driver) rideLs
  where
    makeInvoiceResponse driver ride = do
      mbVehicleNumber <- msum <$> CHRD.findByIdAndVehicleNumber (Kernel.Types.Id.cast ride.id) mbRcNo
      mbBooking <- CHB.findById ride.bookingId
      (source, destination) <- getSourceAndDestination mbBooking
      let chargeableDistance = metersToHighPrecMeters . Meters $ fromMaybe 0 ride.chargeableDistance
          invoiceResponse vehicleNumber =
            API.Types.UI.Invoice.InvoiceRes
              { date = ride.createdAt,
                driverName = unwords [driver.firstName, fromMaybe "" driver.lastName],
                vehicleNumber = vehicleNumber,
                chargeableDistance = chargeableDistance,
                fare = fromMaybe 0 ride.fare,
                rideStartTime = fromMaybe ride.createdAt ride.tripStartTime,
                rideEndTime = fromMaybe ride.updatedAt ride.tripEndTime,
                shortRideId = ride.shortId.getShortId,
                source = source,
                destination = destination,
                chargeableDistanceWithUnit = convertHighPrecMetersToDistance Meter chargeableDistance
              }
      pure $ invoiceResponse <$> mbVehicleNumber

getSourceAndDestination :: Maybe CHB.Booking -> Environment.Flow (Kernel.Prelude.Text, Kernel.Prelude.Text)
getSourceAndDestination (Just booking) = do
  let getFullAddress (Just locId) = fromMaybe notAvailableText <$> CHL.findFullAddressById locId booking.createdAt
      getFullAddress _ = pure notAvailableText
  source <- getFullAddress booking.fromLocationId
  destination <- getFullAddress booking.toLocationId
  pure (source, destination)
getSourceAndDestination _ = pure (notAvailableText, notAvailableText)

notAvailableText :: Kernel.Prelude.Text
notAvailableText = "N/A"
