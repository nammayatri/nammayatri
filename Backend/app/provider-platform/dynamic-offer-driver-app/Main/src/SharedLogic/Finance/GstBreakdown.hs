{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module SharedLogic.Finance.GstBreakdown
  ( computeGstBreakdownForRideOwner,
    computeGstBreakdownForPerson,
    computeGstBreakdownGSTIN,
  )
where

import Control.Applicative ((<|>))
import qualified Domain.Types.Location as DLocation
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.TransporterConfig as DTC
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney)
import Kernel.Types.Id (Id)
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, logError, logWarning)
import qualified Lib.Finance.Invoice.Interface as Finance
import qualified Lib.Finance.Utils.GstBreakdown as Finance
import qualified SharedLogic.DriverIdentityInfo as DIInfo
import qualified Storage.Queries.DriverIdentityInfo as QDII
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.FleetOwnerInformation as QFOI

-- | Ride-earning GST: compare driver/fleet residence proof vs booking pickup location.
--   If 'mbFleetOwnerId' is set, use the fleet owner's addressState; otherwise the driver's.
computeGstBreakdownForRideOwner ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  DTC.GstBreakup ->
  DLocation.Location -> -- booking.fromLocation (pickup)
  Maybe (Id DP.Person) -> -- ride.fleetOwnerId
  Id DP.Person -> -- ride.driverId
  HighPrecMoney ->
  m (Maybe Finance.GstAmountBreakdown)
computeGstBreakdownForRideOwner gstBreakup fromLocation mbFleetOwnerId driverId totalGst = do
  mbResidenceState <- case mbFleetOwnerId of
    Just fleetOwnerId -> resolveFleetOwnerAddressState fleetOwnerId
    Nothing -> resolveDriverAddressState driverId
  when (isNothing mbResidenceState) $
    logWarning $
      "GST breakdown: missing counterparty addressState for pickupLocationId="
        <> fromLocation.id.getId
        <> "; falling back to intra-state CGST/SGST"
  let pickupAddress = fromLocation.address
  pure $
    computeGstBreakdownByPlace
      gstBreakup
      mbResidenceState
      pickupAddress.state
      Nothing
      pickupAddress.city
      totalGst

-- | Subscription / prepaid GST: counterparty residence proof vs platform PoB (merchant operating city).
--   Not used for rides — the payer is 'personId' with an explicit 'isFleetOwner' flag.
computeGstBreakdownForPerson ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  DTC.GstBreakup ->
  DMOC.MerchantOperatingCity ->
  Id DP.Person -> -- paying person
  Bool -> -- isFleetOwner
  HighPrecMoney ->
  m (Maybe Finance.GstAmountBreakdown)
computeGstBreakdownForPerson gstBreakup merchantOperatingCity personId isFleetOwner totalGst = do
  mbReceiverState <-
    if isFleetOwner
      then resolveFleetOwnerAddressState personId
      else resolveDriverAddressState personId
  when (isNothing mbReceiverState) $
    logWarning $
      "GST breakdown: missing counterparty addressState for merchantOpCityId="
        <> merchantOperatingCity.id.getId
        <> "; falling back to intra-state CGST/SGST"
  let (supplierState, supplierCity) = (Just $ show merchantOperatingCity.state, Just $ show merchantOperatingCity.city)
  pure $
    computeGstBreakdownByPlace
      gstBreakup
      supplierState
      mbReceiverState
      supplierCity
      Nothing
      totalGst

-- | Determine GST jurisdiction by comparing supplier vs receiver place
--   (state first; city fallback when either state is missing), then split the total GST.
--   Falls back to intra-state CGST/SGST when place cannot be compared.
computeGstBreakdownByPlace ::
  DTC.GstBreakup ->
  Maybe Text -> -- supplier state
  Maybe Text -> -- receiver state
  Maybe Text -> -- supplier city
  Maybe Text -> -- receiver city
  HighPrecMoney ->
  Maybe Finance.GstAmountBreakdown
computeGstBreakdownByPlace gstBreakup supplierState receiverState supplierCity receiverCity totalGst =
  Finance.computeGstBreakdownFromRates (toGstRateBreakup jurisdiction gstBreakup) totalGst
  where
    jurisdiction =
      fromMaybe Finance.IntraState $
        Finance.compareIndianPlace supplierState receiverState supplierCity receiverCity

-- | Determine GST jurisdiction by comparing the first 2 characters (state code)
--   of the seller and buyer GSTINs, then split the total GST accordingly.
--   Falls back to intra-state CGST/SGST when either GSTIN is missing
--   or too short to extract a state code.
computeGstBreakdownGSTIN ::
  DTC.GstBreakup ->
  Maybe Text -> -- seller (supplier) GSTIN
  Maybe Text -> -- buyer (receiver) GSTIN
  HighPrecMoney ->
  Maybe Finance.GstAmountBreakdown
computeGstBreakdownGSTIN gstBreakup sellerGstin buyerGstin totalGst =
  Finance.computeGstBreakdownFromRates (toGstRateBreakup jurisdiction gstBreakup) totalGst
  where
    jurisdiction =
      fromMaybe Finance.IntraState $
        Finance.compareIndianGstinStateCode sellerGstin buyerGstin

resolveFleetOwnerAddressState ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id DP.Person ->
  m (Maybe Text)
resolveFleetOwnerAddressState fleetOwnerId = do
  mbFleetInfo <- QFOI.findByPrimaryKey fleetOwnerId
  case mbFleetInfo of
    Nothing -> do
      logError $
        "GST breakdown: FleetOwnerInformation not found for fleetOwnerId="
          <> fleetOwnerId.getId
          <> "; expected when fleetOwnerId / isFleetOwner is set"
      pure Nothing
    Just fleetInfo -> pure $ show <$> fleetInfo.addressState

resolveDriverAddressState ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id DP.Person ->
  m (Maybe Text)
resolveDriverAddressState driverId = do
  mbDriverInfo <- QDI.findById driverId
  case mbDriverInfo of
    Nothing -> do
      logError $
        "GST breakdown: DriverInformation not found for driverId="
          <> driverId.getId
          <> "; expected for active driver"
      pure Nothing
    Just driverInfo -> do
      mbDriverIdentityInfo <- QDII.findByDriverId driverId
      let identityInfo = DIInfo.getIdentityInfo mbDriverIdentityInfo driverInfo
      pure $ show <$> identityInfo.addressState

toGstRateBreakup :: Finance.GstJurisdiction -> DTC.GstBreakup -> Finance.GstRateBreakup
toGstRateBreakup Finance.IntraState gstBreakup =
  Finance.InfraStateBreakup $
    Finance.GstRateInfraStateBreakup
      { cgstRate = gstBreakup.cgstPercentage,
        sgstRate = gstBreakup.sgstPercentage
      }
toGstRateBreakup Finance.InterState gstBreakup =
  Finance.InterStateBreakup $
    Finance.GstRateInterStateBreakup
      { igstRate = gstBreakup.igstPercentage <|> ((+) <$> gstBreakup.cgstPercentage <*> gstBreakup.sgstPercentage)
      }
