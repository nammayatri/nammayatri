{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- Common types and functions to break import cycles between UI.Ride, UI.Ride.EndRide, and Dashboard.Ride
module Domain.Action.UI.Ride.Common
  ( DriverRideRes (..),
    FareUISection (..),
    FareUIComponent (..),
    RideEarnings (..),
    EarningsLabels (..),
    fetchEarningsLabels,
    mkDriverRideRes,
    Stop (..),
    DeliveryPersonDetailsAPIEntity (..),
    mkLocationFromLocationMapping,
    calculateLocations,
    makeStop,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as A
import Data.Functor ((<&>))
import qualified Data.HashMap.Strict as HM
import Data.List (find, nub, partition, sort)
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Data.OpenApi (ToSchema)
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Time (UTCTime, diffUTCTime)
import qualified Domain.Action.UI.Location as DLocUI
import qualified Domain.Types as DTC
import qualified Domain.Types as DVST
import qualified Domain.Types.BapMetadata as DSM
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.BookingCancellationReason as DBCR
import qualified Domain.Types.DriverGoHomeRequest as DDGR
import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.Exophone as DExophone
import qualified Domain.Types.FareParameters as DFareParams
import qualified Domain.Types.Location as DLoc
import qualified Domain.Types.MerchantPaymentMethod as DMPM
import qualified Domain.Types.ParcelType as DParcel
import qualified Domain.Types.Rating as DRating
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.RideDetails as RD
import qualified Domain.Types.StopInformation as DSI
import qualified Domain.Types.VehicleVariant as DVeh
import GHC.Generics (Generic)
import Kernel.Beam.Functions (runInReplica)
import qualified Kernel.External.Types as KET
import Kernel.Prelude (roundToIntegral)
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.InMem as IM
import Kernel.Types.CacheFlow (CacheFlow)
import Kernel.Types.Common (BaseUrl, Distance, EncFlow, EsqDBFlow, HighPrecMeters, Meters, Months, Seconds, convertHighPrecMetersToDistance, convertMetersToDistance)
import Kernel.Types.Confidence (Confidence)
import Kernel.Types.Id
import Kernel.Types.Price
import qualified Lib.Queries.GateInfo as QGI
import qualified Lib.Queries.SpecialLocation as QSL
import qualified Lib.Yudhishthira.Storage.Beam.BeamFlow as LYBF
import qualified Lib.Yudhishthira.Tools.Utils as LYTU
import qualified Lib.Yudhishthira.Types as LYT
import SharedLogic.FareCalculator (fareSum)
import qualified SharedLogic.RideFootnotes as RFN
import SharedLogic.Type (BillingCategory)
import qualified Storage.Queries.BookingCancellationReason as QBCR
import qualified Storage.Queries.FareParameters as SQFP
import qualified Storage.Queries.Location as QLoc
import qualified Storage.Queries.LocationMapping as QLM
import qualified Storage.Queries.Translations as QTranslations
import Tools.DynamicLogic (getAppDynamicLogic)
import qualified Tools.Utils as Tools
import Prelude hiding (id)

-- DeliveryPersonDetailsAPIEntity type (match Ride.hs)
data DeliveryPersonDetailsAPIEntity = DeliveryPersonDetailsAPIEntity
  { name :: Text,
    primaryExophone :: Text
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

-- Stop type
-- Remove Eq from deriving clause due to missing Eq instance for DLoc.LocationAPIEntity
data Stop = Stop
  { location :: DLoc.LocationAPIEntity,
    stopInfo :: Maybe DSI.StopInformation
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data BookingType = CURRENT | ADVANCED
  deriving stock (Eq, Show, Generic, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- DriverRideRes type (partial, add all fields as in Ride.hs)
data DriverRideRes = DriverRideRes
  { id :: Id DRide.Ride,
    shortRideId :: ShortId DRide.Ride,
    status :: DRide.RideStatus,
    fromLocation :: DLoc.LocationAPIEntity,
    toLocation :: Maybe DLoc.LocationAPIEntity,
    stops :: [Stop],
    driverName :: Text,
    driverNumber :: Maybe Text,
    vehicleVariant :: DVeh.VehicleVariant,
    pickupDropOutsideOfThreshold :: Maybe Bool,
    billingCategory :: BillingCategory,
    vehicleModel :: Text,
    vehicleColor :: Text,
    vehicleNumber :: Text,
    computedFare :: Maybe Money,
    estimatedBaseFare :: Money,
    computedFareWithCurrency :: Maybe PriceAPIEntity,
    estimatedBaseFareWithCurrency :: PriceAPIEntity,
    estimatedBaseFareWithCurrencyV2 :: PriceAPIEntity,
    estimatedDistance :: Maybe Meters,
    estimatedDistanceWithUnit :: Maybe Distance,
    driverSelectedFare :: Money,
    driverSelectedFareWithCurrency :: PriceAPIEntity,
    actualRideDistance :: HighPrecMeters,
    actualRideDistanceWithUnit :: Distance,
    rideRating :: Maybe Int,
    riderName :: Maybe Text,
    driverArrivalTime :: Maybe UTCTime,
    tripStartTime :: Maybe UTCTime,
    tripEndTime :: Maybe UTCTime,
    specialLocationTag :: Maybe Text,
    actualDuration :: Maybe Seconds,
    estimatedDuration :: Maybe Seconds,
    chargeableDistance :: Maybe Meters,
    chargeableDistanceWithUnit :: Maybe Distance,
    exoPhone :: Text,
    bapName :: Maybe Text,
    bapLogo :: Maybe BaseUrl,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    customerExtraFee :: Maybe Money,
    customerExtraFeeWithCurrency :: Maybe PriceAPIEntity,
    disabilityTag :: Maybe Text,
    coinsRewardedOnGoldTierRide :: Maybe Int,
    requestedVehicleVariant :: DVeh.VehicleVariant,
    isOdometerReadingsRequired :: Bool,
    vehicleServiceTier :: DVST.ServiceTierType,
    vehicleServiceTierName :: Text,
    isVehicleAirConditioned :: Maybe Bool,
    vehicleCapacity :: Maybe Int,
    driverGoHomeRequestId :: Maybe (Id DDGR.DriverGoHomeRequest),
    payerVpa :: Maybe Text,
    autoPayStatus :: Maybe DI.DriverAutoPayStatus,
    customerCancellationDues :: HighPrecMoney,
    estimatedTollCharges :: Maybe HighPrecMoney,
    parkingCharge :: Maybe HighPrecMoney,
    tollCharges :: Maybe HighPrecMoney,
    tollConfidence :: Maybe Confidence,
    customerCancellationDuesWithCurrency :: PriceAPIEntity,
    estimatedTollChargesWithCurrency :: Maybe PriceAPIEntity,
    parkingChargeWithCurrency :: Maybe PriceAPIEntity,
    tollChargesWithCurrency :: Maybe PriceAPIEntity,
    isFreeRide :: Maybe Bool,
    stopLocationId :: Maybe (Id DLoc.Location),
    tripCategory :: DTC.TripCategory,
    nextStopLocation :: Maybe DLoc.Location,
    lastStopLocation :: Maybe DLoc.Location,
    startOdometerReading :: Maybe DRide.OdometerReading,
    endOdometerReading :: Maybe DRide.OdometerReading,
    returnTime :: Maybe UTCTime,
    vehicleAge :: Maybe Months,
    roundTrip :: Bool,
    tripScheduledAt :: UTCTime,
    isValueAddNP :: Bool,
    bookingType :: BookingType,
    enableFrequentLocationUpdates :: Maybe Bool,
    fleetOwnerId :: Maybe Text,
    enableOtpLessRide :: Bool,
    cancellationSource :: Maybe DBCR.CancellationSource,
    tipAmount :: Maybe PriceAPIEntity,
    penalityCharge :: Maybe PriceAPIEntity,
    senderDetails :: Maybe DeliveryPersonDetailsAPIEntity,
    receiverDetails :: Maybe DeliveryPersonDetailsAPIEntity,
    extraFareMitigationFlag :: Maybe Bool,
    parcelType :: Maybe DParcel.ParcelType,
    parcelQuantity :: Maybe Int,
    isInsured :: Maybe Bool,
    insuredAmount :: Maybe Text,
    isPetRide :: Bool,
    riderMobileNumber :: Maybe Text,
    paymentInstrument :: Maybe DMPM.PaymentInstrument,
    paymentMode :: Maybe DMPM.PaymentMode,
    commissionCharges :: Maybe HighPrecMoney,
    discountAmount :: Maybe HighPrecMoney,
    pickupZoneGateId :: Maybe Text,
    pickupZoneGateName :: Maybe Text,
    pickupZoneGateType :: Maybe Text,
    pickupZoneEntryFeeAmount :: Maybe Double,
    specialLocationName :: Maybe Text,
    specialLocationCategory :: Maybe Text,
    isValidRide :: Maybe Bool,
    amountToCollectInCash :: Maybe HighPrecMoney,
    amountToBeSettledOnline :: Maybe HighPrecMoney,
    amountToCollectInCashWithCurrency :: Maybe PriceAPIEntity,
    amountToBeSettledOnlineWithCurrency :: Maybe PriceAPIEntity,
    rideEarnings :: Maybe RideEarnings
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data FareUISection
  = FareBreakup
  | Footnote
  deriving stock (Show, Eq, Ord, Generic, Enum, Bounded)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data FareUIComponent = FareUIComponent
  { title :: Text,
    price :: PriceAPIEntity,
    uiTranslation :: Maybe Text,
    section :: FareUISection
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data UiTranslationRaw
  = UiTrPlain Text
  | UiTrByKey Text (HM.HashMap Text Text)
  deriving stock (Show, Generic)

instance FromJSON UiTranslationRaw where
  parseJSON v = case v of
    A.String s -> pure $ UiTrPlain s
    A.Object _ -> A.withObject "UiTranslationByKey" (\o -> UiTrByKey <$> o A..: "key" <*> o A..:? "vars" A..!= HM.empty) v
    _ -> fail "uiTranslation must be string or {key, vars}"

data FareUIComponentRaw = FareUIComponentRaw
  { title :: Text,
    price :: PriceAPIEntity,
    uiTranslation :: Maybe UiTranslationRaw,
    section :: FareUISection
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

data RideEarnings = RideEarnings
  { fareBreakup :: [FareUIComponent],
    footnotes :: [FareUIComponent]
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

buildRideEarnings ::
  (LYBF.BeamFlow m r) =>
  KET.Language ->
  EarningsLabels ->
  DRB.Booking ->
  DRide.Ride ->
  DFareParams.FareParameters ->
  m RideEarnings
buildRideEarnings lang labels booking ride fp = do
  let fare = fromMaybe booking.estimatedFare ride.fare
      discount = fromMaybe 0 ride.discountAmount
      commission = fromMaybe 0 ride.commission
      tips = fromMaybe 0 ride.tipAmount
      cur = ride.currency
      amountPaidByCustomer = fare - discount + tips
      EarningsLabels {lblAmountPaid, lblDiscount, lblTips, lblCommission, lblFare} = labels
      cancellationDues = fromMaybe 0 fp.customerCancellationDues -- we will deccide where to fetch this ?
  let mkComp sec key mbLabel value applicable =
        if applicable
          then
            Just
              FareUIComponent
                { title = key,
                  price = PriceAPIEntity (roundAmountByCurrency' cur value) cur,
                  uiTranslation = mbLabel,
                  section = sec
                }
          else Nothing
      fareBreakupItems =
        catMaybes
          [ mkComp FareBreakup "AMOUNT_PAID_BY_CUSTOMER" lblAmountPaid amountPaidByCustomer True,
            mkComp FareBreakup "DISCOUNT" lblDiscount discount (discount > 0),
            mkComp FareBreakup "TIPS" lblTips tips (tips > 0),
            mkComp FareBreakup "COMMISSION" lblCommission commission (commission /= 0),
            mkComp FareBreakup "CUSTOMER_CANCELLATION_CHARGE" lblFare cancellationDues (cancellationDues > 0)
          ]
  footnoteItems <- buildFootnotes lang booking ride fp

  pure RideEarnings {fareBreakup = fareBreakupItems, footnotes = footnoteItems}

buildFootnotes ::
  (LYBF.BeamFlow m r) =>
  KET.Language ->
  DRB.Booking ->
  DRide.Ride ->
  DFareParams.FareParameters ->
  m [FareUIComponent]
buildFootnotes lang booking ride fp = do
  (logics, _mbVersion) <- getAppDynamicLogic (cast ride.merchantOperatingCityId) LYT.RIDE_FOOTNOTES_DISPLAY ride.createdAt Nothing Nothing
  if null logics
    then pure []
    else do
      mbRideFareParams <- maybe (pure Nothing) SQFP.findById ride.fareParametersId
      let waitingTimeSeconds = case (ride.tripStartTime, ride.driverArrivalTime) of
            (Just startT, Just arrivalT) -> Just (round (diffUTCTime startT arrivalT))
            _ -> Nothing
          logicInput =
            RFN.RideFootnotesLogicInput
              { ride = ride,
                booking = booking,
                rideFareParams = mbRideFareParams,
                bookingFareParams = fp,
                waitingTimeSeconds = waitingTimeSeconds,
                language = pack (show lang)
              }
      resp <- LYTU.runLogics logics logicInput
      case A.fromJSON resp.result :: A.Result [FareUIComponentRaw] of
        A.Success rawItems -> do
          let allKeys = nub $
                flip concatMap rawItems $ \raw -> case raw.uiTranslation of
                  Just (UiTrByKey k _) -> [k]
                  Nothing -> [raw.title]
                  _ -> []
          rows <-
            IM.withInMemCache (["FNT", pack (show lang)] <> sort allKeys) 3600 $
              QTranslations.findAllByMessageKeysAndLanguage allKeys lang
          let (preferred, fallbacks) = partition (\t -> t.language == lang) rows
              txMap =
                HM.union
                  (HM.fromList [(t.messageKey, t.message) | t <- preferred])
                  (HM.fromList [(t.messageKey, t.message) | t <- fallbacks])
          pure $ catMaybes $ map (resolveRawComponent txMap) rawItems
        A.Error _ -> pure []

resolveRawComponent :: HM.HashMap Text Text -> FareUIComponentRaw -> Maybe FareUIComponent
resolveRawComponent txMap raw =
  let ui = resolveUiTranslationFromMap txMap raw.title raw.uiTranslation
   in if maybe False (T.isInfixOf "{{") ui
        then Nothing
        else
          Just
            FareUIComponent
              { title = raw.title,
                price = raw.price,
                uiTranslation = ui,
                section = raw.section
              }

resolveUiTranslationFromMap :: HM.HashMap Text Text -> Text -> Maybe UiTranslationRaw -> Maybe Text
resolveUiTranslationFromMap txMap titleKey = \case
  Just (UiTrPlain s) -> Just s
  Just (UiTrByKey k vs) ->
    let template = fromMaybe k (HM.lookup k txMap)
     in Just (substituteVars template vs)
  Nothing -> HM.lookup titleKey txMap

substituteVars :: Text -> HM.HashMap Text Text -> Text
substituteVars = HM.foldrWithKey (\k v acc -> T.replace ("{{" <> k <> "}}") v acc)

resolveLabel ::
  (CacheFlow m r, EsqDBFlow m r) =>
  KET.Language ->
  Text ->
  m (Maybe Text)
resolveLabel lang key = fmap (fmap (.message)) $ QTranslations.findByErrorAndLanguage key lang

data EarningsLabels = EarningsLabels
  { lblAmountPaid :: Maybe Text,
    lblDiscount :: Maybe Text,
    lblTips :: Maybe Text,
    lblCommission :: Maybe Text,
    lblNetEarnings :: Maybe Text,
    lblFare :: Maybe Text
  }

fetchEarningsLabels ::
  (CacheFlow m r, EsqDBFlow m r) =>
  KET.Language ->
  m EarningsLabels
fetchEarningsLabels lang =
  EarningsLabels
    <$> resolveLabel lang "AMOUNT_PAID_BY_CUSTOMER"
    <*> resolveLabel lang "DISCOUNT"
    <*> resolveLabel lang "TIPS"
    <*> resolveLabel lang "COMMISSION"
    <*> resolveLabel lang "NET_DRIVER_EARNINGS"
    <*> resolveLabel lang "FARE"

mkDriverRideRes ::
  ( EncFlow m r,
    LYBF.BeamFlow m r,
    Esq.EsqDBReplicaFlow m r
  ) =>
  KET.Language ->
  Maybe EarningsLabels ->
  RD.RideDetails ->
  Maybe Text ->
  Maybe DRating.Rating ->
  Maybe DExophone.Exophone ->
  (DRide.Ride, DRB.Booking) ->
  Maybe DSM.BapMetadata ->
  Maybe (Id DDGR.DriverGoHomeRequest) ->
  Maybe DI.DriverInformation ->
  Bool ->
  [DSI.StopInformation] ->
  Maybe Text ->
  m DriverRideRes
mkDriverRideRes language mbEarningsLabels rideDetails driverNumber rideRating mbExophone (ride, booking) bapMetadata goHomeReqId driverInfo isValueAddNP stopsInfo mbRiderMobileNumber = do
  let fareParams = booking.fareParams
      estimatedBaseFareGross = fareSum (fareParams{driverSelectedFare = Nothing}) Nothing -- it should not be part of estimatedBaseFare
      estimatedCommission = fromMaybe 0 booking.commission
      estimatedBaseFare = max 0 (estimatedBaseFareGross - estimatedCommission)
      estimatedBaseFareGrossV2 = fareSum fareParams Nothing
      estimatedBaseFareV2 = max 0 (estimatedBaseFareGrossV2 - estimatedCommission)
      rideCommission = fromMaybe 0 ride.commission
      rideTipsAmount = fromMaybe 0 ride.tipAmount
      computedFareNet = (\fareAmt -> max 0 (fareAmt - rideCommission + rideTipsAmount)) <$> ride.fare
  let initial = "" :: Text
  (nextStopLocation, lastStopLocation) <- case booking.tripCategory of
    DTC.Rental _ -> calculateLocations booking.id booking.stopLocationId
    _ -> return (Nothing, Nothing)
  cancellationReason <- if ride.status == DRide.CANCELLED then runInReplica (QBCR.findByRideId (Just ride.id)) else pure Nothing

  -- Lookup pickup zone gate info for entry fee
  (mbGateInfo, mbSpecialLoc) <- case booking.pickupGateId of
    Just gateId -> do
      mbGate <- QGI.findById (Id gateId)
      mbSL <- case mbGate of
        Just gate -> QSL.findById gate.specialLocationId
        Nothing -> pure Nothing
      pure (mbGate, mbSL)
    Nothing -> pure (Nothing, Nothing)

  -- See `amount*` semantics matrix (Cash / Online):
  --   amountToCollectInCash   : Cash   -> FinalFare - Offer (commission included)
  --                             Online -> Nothing
  --   amountToBeSettledOnline : Cash   -> Offer
  --                             Online -> FinalFare - Commission (offer already applied)
  let offer = fromMaybe 0 ride.discountAmount
      commission = fromMaybe 0 ride.commission
      mbAmountToCollectInCash =
        case (booking.paymentInstrument, ride.fare) of
          (Just DMPM.Cash, Just fareAmt) -> Just $ max 0 (fareAmt - offer + rideTipsAmount)
          _ -> Nothing
      mbAmountToBeSettledOnline =
        case booking.paymentInstrument of
          Just DMPM.Cash -> if offer > 0 then Just offer else Nothing
          _ -> case ride.fare of
            Just fareAmt -> Just $ max 0 (fareAmt - commission + rideTipsAmount)
            Nothing -> Nothing

  mbRideEarningsVal <- case mbEarningsLabels of
    Just earningsLabels -> Just <$> buildRideEarnings language earningsLabels booking ride fareParams
    Nothing -> pure Nothing

  return $
    DriverRideRes
      { id = ride.id,
        shortRideId = ride.shortId,
        status = ride.status,
        fromLocation = DLocUI.makeLocationAPIEntity booking.fromLocation,
        toLocation = DLocUI.makeLocationAPIEntity <$> booking.toLocation,
        stops = map (makeStop stopsInfo) booking.stops,
        driverName = rideDetails.driverName,
        driverNumber,
        vehicleNumber = rideDetails.vehicleNumber,
        vehicleColor = fromMaybe initial rideDetails.vehicleColor,
        vehicleVariant = fromMaybe DVeh.SEDAN rideDetails.vehicleVariant,
        vehicleModel = fromMaybe initial rideDetails.vehicleModel,
        computedFare = roundToIntegral <$> computedFareNet,
        computedFareWithCurrency = (\fare -> PriceAPIEntity (roundAmountByCurrency' ride.currency fare) ride.currency) <$> computedFareNet,
        estimatedDuration = booking.estimatedDuration,
        actualDuration = roundToIntegral <$> (diffUTCTime <$> ride.tripEndTime <*> ride.tripStartTime),
        estimatedBaseFare = roundToIntegral estimatedBaseFare,
        estimatedBaseFareWithCurrency = PriceAPIEntity (roundAmountByCurrency' ride.currency estimatedBaseFare) ride.currency,
        estimatedBaseFareWithCurrencyV2 = PriceAPIEntity (roundAmountByCurrency' ride.currency estimatedBaseFareV2) ride.currency,
        estimatedDistance = booking.estimatedDistance,
        estimatedDistanceWithUnit = convertMetersToDistance booking.distanceUnit <$> booking.estimatedDistance,
        driverSelectedFare = roundToIntegral $ fromMaybe 0.0 fareParams.driverSelectedFare,
        driverSelectedFareWithCurrency = flip PriceAPIEntity fareParams.currency $ fromMaybe 0.0 fareParams.driverSelectedFare,
        actualRideDistance = ride.traveledDistance,
        actualRideDistanceWithUnit = convertHighPrecMetersToDistance ride.distanceUnit ride.traveledDistance,
        createdAt = ride.createdAt,
        updatedAt = ride.updatedAt,
        riderName = booking.riderName,
        driverArrivalTime = ride.driverArrivalTime,
        pickupDropOutsideOfThreshold = ride.pickupDropOutsideOfThreshold,
        tripStartTime = ride.tripStartTime,
        tripEndTime = ride.tripEndTime,
        specialLocationTag = booking.specialLocationTag,
        rideRating = rideRating <&> (.ratingValue),
        chargeableDistance = ride.chargeableDistance,
        chargeableDistanceWithUnit = convertMetersToDistance ride.distanceUnit <$> ride.chargeableDistance,
        exoPhone = maybe booking.primaryExophone (\exophone -> if not exophone.isPrimaryDown then exophone.primaryPhone else exophone.backupPhone) mbExophone,
        customerExtraFee = roundToIntegral <$> fareParams.customerExtraFee,
        customerExtraFeeWithCurrency = flip PriceAPIEntity fareParams.currency <$> fareParams.customerExtraFee,
        bapName = bapMetadata <&> (.name),
        bapLogo = bapMetadata >>= (.logoUrl),
        disabilityTag = booking.disabilityTag,
        coinsRewardedOnGoldTierRide = booking.coinsRewardedOnGoldTierRide,
        requestedVehicleVariant = DVeh.castServiceTierToVariant booking.vehicleServiceTier,
        isOdometerReadingsRequired = DTC.isOdometerReadingsRequired booking.tripCategory,
        vehicleServiceTier = booking.vehicleServiceTier,
        vehicleServiceTierName = booking.vehicleServiceTierName,
        vehicleCapacity = booking.vehicleServiceTierSeatingCapacity,
        isVehicleAirConditioned = booking.isAirConditioned,
        driverGoHomeRequestId = goHomeReqId,
        payerVpa = driverInfo >>= (.payerVpa),
        autoPayStatus = driverInfo >>= (.autoPayStatus),
        isFreeRide = ride.isFreeRide,
        customerCancellationDues = fromMaybe 0 fareParams.customerCancellationDues,
        estimatedTollCharges = fareParams.tollCharges,
        parkingCharge = fareParams.parkingCharge,
        tollCharges = ride.tollCharges,
        tollConfidence = ride.tollConfidence,
        customerCancellationDuesWithCurrency = flip PriceAPIEntity fareParams.currency $ fromMaybe 0 fareParams.customerCancellationDues,
        estimatedTollChargesWithCurrency = flip PriceAPIEntity fareParams.currency <$> fareParams.tollCharges,
        parkingChargeWithCurrency = flip PriceAPIEntity fareParams.currency <$> fareParams.parkingCharge,
        tollChargesWithCurrency = flip PriceAPIEntity ride.currency <$> ride.tollCharges,
        startOdometerReading = ride.startOdometerReading,
        endOdometerReading = ride.endOdometerReading,
        stopLocationId = booking.stopLocationId,
        tripCategory = booking.tripCategory,
        returnTime = booking.returnTime,
        vehicleAge = rideDetails.vehicleAge,
        roundTrip = fromMaybe False booking.roundTrip,
        nextStopLocation = nextStopLocation,
        lastStopLocation = lastStopLocation,
        tripScheduledAt = booking.startTime,
        bookingType = if ride.status == DRide.NEW && ride.isAdvanceBooking && maybe False (.hasAdvanceBooking) driverInfo then ADVANCED else CURRENT,
        isValueAddNP,
        enableFrequentLocationUpdates = ride.enableFrequentLocationUpdates,
        fleetOwnerId = rideDetails.fleetOwnerId,
        enableOtpLessRide = fromMaybe False ride.enableOtpLessRide,
        cancellationSource = fmap (\cr -> cr.source) cancellationReason,
        tipAmount = flip PriceAPIEntity ride.currency <$> ride.tipAmount,
        penalityCharge = flip PriceAPIEntity ride.currency <$> ride.cancellationFeeIfCancelled,
        senderDetails = booking.senderDetails <&> (\sd -> DeliveryPersonDetailsAPIEntity (sd.name) sd.primaryExophone),
        receiverDetails = booking.receiverDetails <&> (\rd -> DeliveryPersonDetailsAPIEntity (rd.name) rd.primaryExophone),
        extraFareMitigationFlag = driverInfo >>= (.extraFareMitigationFlag),
        parcelType = booking.parcelType,
        parcelQuantity = booking.parcelQuantity,
        isInsured = Just $ ride.isInsured,
        insuredAmount = ride.insuredAmount,
        isPetRide = booking.isPetRide,
        riderMobileNumber = mbRiderMobileNumber,
        billingCategory = booking.billingCategory,
        paymentInstrument = booking.paymentInstrument,
        paymentMode = booking.paymentMode,
        commissionCharges = ride.commission,
        discountAmount = ride.discountAmount,
        pickupZoneGateId = booking.pickupGateId,
        pickupZoneGateName = mbGateInfo <&> (.name),
        pickupZoneGateType = mbGateInfo <&> (pack . show . (.gateType)),
        pickupZoneEntryFeeAmount = mbGateInfo >>= (.entryFeeAmount),
        specialLocationName = mbSpecialLoc <&> (.locationName),
        specialLocationCategory = mbSpecialLoc <&> (.category),
        isValidRide = Just $ Tools.isValidRide ride,
        amountToCollectInCash = mbAmountToCollectInCash,
        amountToBeSettledOnline = mbAmountToBeSettledOnline,
        amountToCollectInCashWithCurrency = (\amt -> PriceAPIEntity (roundAmountByCurrency' ride.currency amt) ride.currency) <$> mbAmountToCollectInCash,
        amountToBeSettledOnlineWithCurrency = (\amt -> PriceAPIEntity (roundAmountByCurrency' ride.currency amt) ride.currency) <$> mbAmountToBeSettledOnline,
        rideEarnings = mbRideEarningsVal
      }

-- calculateLocations moved from UI.Ride
makeStop :: [DSI.StopInformation] -> DLoc.Location -> Stop
makeStop stopsInfo stop =
  let stopInfo = find (\s -> s.stopLocId == stop.id) stopsInfo
   in Stop (DLocUI.makeLocationAPIEntity stop) stopInfo

calculateLocations ::
  ( CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id DRB.Booking ->
  Maybe (Id DLoc.Location) ->
  m (Maybe DLoc.Location, Maybe DLoc.Location)
calculateLocations bookingId stopLocationId = do
  maxOrder <- QLM.maxOrderByEntity (bookingId.getId)
  case stopLocationId of
    Nothing -> do
      lastLoc <- if maxOrder == 0 then pure Nothing else mkLocationFromLocationMapping bookingId maxOrder
      return (Nothing, lastLoc)
    Just nextStopId -> do
      nextLoc <- QLoc.findById nextStopId
      lastLoc <- mkLocationFromLocationMapping bookingId (maxOrder - 1)
      return (nextLoc, lastLoc)

mkLocationFromLocationMapping ::
  ( CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id DRB.Booking ->
  Int ->
  m (Maybe DLoc.Location)
mkLocationFromLocationMapping bookingId order = do
  locMap <- listToMaybe <$> QLM.findByEntityIdOrderAndVersion (bookingId.getId) order QLM.latestTag
  case locMap of
    Nothing -> pure Nothing
    Just locMap_ -> QLoc.findById locMap_.locationId
