module Domain.Action.UI.FareBreakup where

import Data.Aeson
import Domain.SharedLogic.RideDiscount (isProjectedFareParamTag)
import Domain.Types.FareBreakup
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Kernel.Utils.TH
import SharedLogic.Booking (getfareBreakups)
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.QueriesExtra.RideLite as QRideLite
import qualified Storage.Queries.Ride as QRide
import Tools.Error

data FareBreakupEntity = Booking | Ride
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Kernel.Utils.TH.mkHttpInstancesForEnum ''FareBreakupEntity)

data BookingFareBreakupRes = BookingFareBreakupRes
  { fareBreakup :: [FareBreakupAPIEntity],
    estimatedFareBreakup :: [FareBreakupAPIEntity]
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

mkFareBreakupAPIEntity :: FareBreakup -> FareBreakupAPIEntity
mkFareBreakupAPIEntity FareBreakup {..} =
  FareBreakupAPIEntity
    { amount = roundToIntegral amount.amount,
      amountWithCurrency = mkPriceAPIEntity amount,
      ..
    }

getBookingFareBreakup ::
  (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, EncFlow m r, MonadFlow m) =>
  (Id DP.Person, Id DM.Merchant) ->
  Text ->
  Maybe FareBreakupEntity ->
  m BookingFareBreakupRes
getBookingFareBreakup _ entityId mbEntityType = do
  booking <- case fromMaybe Booking mbEntityType of
    Booking -> QBooking.findById (Id entityId) >>= fromMaybeM (BookingDoesNotExist entityId)
    Ride -> do
      ride <- QRideLite.findByIdLite (Id entityId) >>= fromMaybeM (RideNotFound entityId)
      QBooking.findById ride.bookingId >>= fromMaybeM (BookingDoesNotExist ride.bookingId.getId)
  mbRide <- QRide.findByRBId booking.id
  (fareBreakups, estimatedFareBreakups) <- getfareBreakups booking mbRide
  pure
    BookingFareBreakupRes
      { fareBreakup = filter (not . isProjectedFareParamTag . (.description)) (mkFareBreakupAPIEntity <$> fareBreakups),
        estimatedFareBreakup = filter (not . isProjectedFareParamTag . (.description)) (mkFareBreakupAPIEntity <$> estimatedFareBreakups)
      }
