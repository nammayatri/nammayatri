{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Beckn.Common where

import qualified BecknV2.OnDemand.Tags as Tags
import qualified BecknV2.OnDemand.Types as Spec
import Control.Applicative ((<|>))
import qualified Data.Text as T
import Domain.Types.BecknConfig as DBC
import Domain.Types.Booking as DRB
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.DriverStats as DStats
import qualified Domain.Types.FareParameters as Fare
import qualified Domain.Types.FleetOwnerInformation as DFOI
import Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantPaymentMethod as DMPM
import Domain.Types.Person as SP
import qualified Domain.Types.Person as DP
import Domain.Types.Ride as DRide
import Domain.Types.SearchTry as DST
import Domain.Types.Vehicle as SVeh
import Kernel.External.Maps.Types as Maps
import Kernel.External.Payment.Interface as Payment
import qualified Kernel.External.Payment.Stripe.Types as Stripe
import Kernel.Prelude
import Kernel.Types.Common as Common
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common (CacheFlow)
import qualified Storage.Queries.FleetDriverAssociation as QFDA
import qualified Storage.Queries.FleetOwnerInformation as QFOI

data BPPInvoiceInfo = BPPInvoiceInfo
  { issuedById :: Maybe Text,
    issuedByName :: Maybe Text,
    issuedByAddress :: Maybe Text,
    supplierName :: Maybe Text,
    supplierAddress :: Maybe Text,
    supplierGSTIN :: Maybe Text,
    supplierTaxNo :: Maybe Text,
    supplierId :: Maybe Text
  }

emptyBPPInvoiceInfo :: BPPInvoiceInfo
emptyBPPInvoiceInfo = BPPInvoiceInfo Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

mkBPPInvoiceInfoTagGroup :: BPPInvoiceInfo -> Maybe [Spec.TagGroup]
mkBPPInvoiceInfoTagGroup info =
  Tags.convertToTagGroup
    [ (Tags.ISSUED_BY_ID, info.issuedById),
      (Tags.ISSUED_BY_NAME, info.issuedByName),
      (Tags.ISSUED_BY_ADDRESS, info.issuedByAddress),
      (Tags.SUPPLIER_NAME, info.supplierName),
      (Tags.SUPPLIER_ADDRESS, info.supplierAddress),
      (Tags.SUPPLIER_GSTIN, info.supplierGSTIN),
      (Tags.SUPPLIER_TAX_NO, info.supplierTaxNo),
      (Tags.SUPPLIER_ID, info.supplierId)
    ]

resolveBPPInvoiceInfoForRide ::
  (EsqDBFlow m r, CacheFlow m r, MonadFlow m) =>
  DM.Merchant ->
  DMOC.MerchantOperatingCity ->
  Id DP.Person ->
  m BPPInvoiceInfo
resolveBPPInvoiceInfoForRide merchant merchantOpCity driverId = do
  let merchantAddress = Just $ show merchantOpCity.city <> ", " <> show merchantOpCity.state <> ", " <> show merchantOpCity.country
  mbFleetInfo <- resolveFleetInfo driverId
  pure $ buildBPPInvoiceInfo merchant merchantAddress mbFleetInfo

resolveFleetInfo :: (EsqDBFlow m r, CacheFlow m r, MonadFlow m) => Id DP.Person -> m (Maybe DFOI.FleetOwnerInformation)
resolveFleetInfo driverId = do
  mFleetAssoc <- QFDA.findByDriverId driverId True
  case mFleetAssoc of
    Just assoc -> QFOI.findByPrimaryKey (Id assoc.fleetOwnerId :: Id DP.Person)
    Nothing -> pure Nothing

buildBPPInvoiceInfo :: DM.Merchant -> Maybe Text -> Maybe DFOI.FleetOwnerInformation -> BPPInvoiceInfo
buildBPPInvoiceInfo merchant merchantAddress mbFleetInfo =
  let mid = merchant.id.getId
      mName = merchant.name
   in case mbFleetInfo of
        Just fleetInfo ->
          BPPInvoiceInfo
            { issuedById = Just mid,
              issuedByName = Just mName,
              issuedByAddress = merchantAddress,
              supplierName = fleetInfo.fleetName,
              supplierAddress = fleetInfo.stripeAddress <&> formatStripeAddress,
              supplierGSTIN = fleetInfo.gstNumberDec,
              supplierTaxNo = fleetInfo.vatNumber <|> fleetInfo.gstNumberDec,
              supplierId = Just fleetInfo.fleetOwnerPersonId.getId
            }
        Nothing ->
          BPPInvoiceInfo
            { issuedById = Just mid,
              issuedByName = Just mName,
              issuedByAddress = merchantAddress,
              supplierName = Just mName,
              supplierAddress = merchantAddress,
              supplierGSTIN = merchant.gstin,
              supplierTaxNo = merchant.vatNumber <|> merchant.gstin,
              supplierId = Just mid
            }

formatStripeAddress :: Stripe.Address -> Text
formatStripeAddress addr =
  T.intercalate ", " $ catMaybes [addr.line1, addr.line2, addr.city, addr.state, addr.postal_code, addr.country]

data BookingDetails = BookingDetails
  { ride :: DRide.Ride,
    booking :: DRB.Booking,
    driver :: SP.Person,
    driverStats :: DStats.DriverStats,
    vehicle :: SVeh.Vehicle,
    riderPhone :: Maybe Text,
    isValueAddNP :: Bool,
    bppConfig :: DBC.BecknConfig,
    merchant :: DM.Merchant,
    paymentMethodInfo :: Maybe DMPM.PaymentMethodInfo,
    paymentUrl :: Maybe Text
  }

data DRideAssignedReq = DRideAssignedReq
  { bookingDetails :: BookingDetails,
    image :: Maybe Text,
    isDriverBirthDay :: Bool,
    vehicleAge :: Maybe Months,
    isFreeRide :: Bool,
    driverAccountId :: Maybe Payment.AccountId,
    estimateId :: Maybe Text,
    isAlreadyFav :: Bool,
    favCount :: Int,
    isSafetyPlus :: Bool,
    bppInvoiceInfo :: BPPInvoiceInfo
  }

data DRideStartedReq = DRideStartedReq
  { bookingDetails :: BookingDetails,
    tripStartLocation :: Maybe Maps.LatLong,
    estimateId :: Maybe Text
  }

data DRideCompletedReq = DRideCompletedReq
  { bookingDetails :: BookingDetails,
    fareParams :: Fare.FareParameters,
    tripEndLocation :: Maybe Maps.LatLong,
    estimateId :: Maybe Text
  }

data DBookingCancelledReq = DBookingCancelledReq
  { booking :: DRB.Booking,
    bookingDetails :: Maybe BookingDetails,
    cancellationSource :: SBCR.CancellationSource,
    cancellationFee :: Maybe Common.PriceAPIEntity,
    estimateId :: Maybe Text,
    cancellationReasonCode :: Maybe Text
  }

data DDriverArrivedReq = DDriverArrivedReq
  { bookingDetails :: BookingDetails,
    arrivalTime :: Maybe UTCTime,
    estimateId :: Maybe Text
  }

data CurrentSearchInfo = CurrentSearchInfo
  { routeDistance :: Maybe Meters,
    dropLocation :: Maybe LatLong,
    searchTry :: DST.SearchTry
  }
  deriving (Generic, Show)
