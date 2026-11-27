module SharedLogic.Finance.RideInvoice
  ( rideFinanceInvoiceId,
    isFinalRideOfBooking,
    markBookingInvoicesStatus,
    resolveInvoiceIssuedToAddress,
  )
where

import Control.Applicative ((<|>))
import qualified Data.Text as T
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.DriverGstin as DDG
import qualified Domain.Types.FleetOwnerInformation as DFOI
import qualified "beckn-spec" Domain.Types.Invoice as BeckInvoice
import qualified Domain.Types.Ride as DRide
import Kernel.Prelude
import Kernel.Types.Id
import qualified Lib.Finance.Core.Types as Finance
import qualified Lib.Finance.Domain.Types.Invoice as FInvoice
import qualified Lib.Finance.Invoice.Service as InvoiceSvc
import qualified Lib.Finance.Storage.Beam.BeamFlow as BeamFlow
import qualified Lib.Finance.Storage.Queries.InvoiceExtra as QFinanceInvoiceExtra

rideFinanceInvoiceId :: DRide.Ride -> DRB.Booking -> Maybe Text
rideFinanceInvoiceId ride booking
  | ride.status == DRide.COMPLETED = booking.financeInvoiceId
  | ride.status == DRide.CANCELLED && booking.status == DRB.CANCELLED = booking.financeInvoiceId
  | otherwise = Nothing

isFinalRideOfBooking :: UTCTime -> [DRide.Ride] -> Bool
isFinalRideOfBooking rideCreatedAt = all (\otherRide -> otherRide.createdAt <= rideCreatedAt)

markBookingInvoicesStatus :: (BeamFlow.BeamFlow m r, Finance.HasActorInfo m r) => FInvoice.InvoiceStatus -> BeckInvoice.InvoiceType -> Id DRB.Booking -> m ()
markBookingInvoicesStatus status invoiceType bookingId = do
  invoices <- QFinanceInvoiceExtra.findByReferenceIdWithOptions bookingId.getId (Just invoiceType) Nothing [FInvoice.Draft, FInvoice.Issued] Nothing Nothing
  forM_ (filter ((/= status) . (.status)) invoices) $ \inv -> InvoiceSvc.updateInvoiceStatus inv.id status

resolveInvoiceIssuedToAddress :: Maybe DDG.DriverGstin -> Maybe DFOI.FleetOwnerInformation -> Text -> Text
resolveInvoiceIssuedToAddress mbBuyerGstinRow mbFleetInfo opCityAddress =
  fromMaybe opCityAddress (gstinAddress <|> fleetOwnerAddress)
  where
    nonBlank t = if T.null (T.strip t) then Nothing else Just t
    gstinAddress = (mbBuyerGstinRow >>= (.address)) >>= nonBlank
    fleetOwnerAddress = do
      addr <- (mbFleetInfo >>= (.address)) >>= nonBlank
      pure $ addr <> maybe "" (\st -> ", " <> show st) (mbFleetInfo >>= (.addressState))
