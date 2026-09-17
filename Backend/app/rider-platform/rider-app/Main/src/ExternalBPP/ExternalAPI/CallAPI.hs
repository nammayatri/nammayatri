module ExternalBPP.ExternalAPI.CallAPI where

import qualified BecknV2.FRFS.Enums as Spec
import Data.List (nub, sortOn)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Data.Time.Format (defaultTimeLocale, formatTime)
import Domain.Types hiding (ONDC)
import Domain.Types.Beckn.FRFS.OnSearch
import Domain.Types.BecknConfig
import qualified Domain.Types.FRFSQuote as DQuote
import Domain.Types.FRFSQuoteCategory
import Domain.Types.FRFSQuoteCategoryType
import Domain.Types.FRFSTicketBooking
import Domain.Types.IntegratedBPPConfig
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import Domain.Types.Person
import Domain.Types.RouteStopMapping
import Domain.Types.Station
import Domain.Types.StationType
import qualified ExternalBPP.ExternalAPI.Bus.EBIX.Order as EBIXOrder
import qualified ExternalBPP.ExternalAPI.Bus.EBIX.Status as EBIXStatus
import qualified ExternalBPP.ExternalAPI.Direct.Order as DIRECTOrder
import qualified ExternalBPP.ExternalAPI.Direct.Status as DIRECTStatus
import qualified ExternalBPP.ExternalAPI.Direct.Utils as DirectUTILS
import qualified ExternalBPP.ExternalAPI.Direct.Verify as DIRECTVerify
import qualified ExternalBPP.ExternalAPI.Metro.CMRL.BusinessHour as CMRLBusinessHour
import qualified ExternalBPP.ExternalAPI.Metro.CMRL.DurationDetails as CMRLDurationDetails
import qualified ExternalBPP.ExternalAPI.Metro.CMRL.FareByOriginDest as CMRLFareByOriginDest
import qualified ExternalBPP.ExternalAPI.Metro.CMRL.FareMatrix as CMRLFareMatrix
import qualified ExternalBPP.ExternalAPI.Metro.CMRL.Order as CMRLOrder
import qualified ExternalBPP.ExternalAPI.Metro.CMRL.PassengerViewStatus as CMRLPassengerViewStatus
import qualified ExternalBPP.ExternalAPI.Metro.CMRL.StationList as CMRLStationList
import qualified ExternalBPP.ExternalAPI.Metro.CMRL.TicketStatus as CMRLStatus
import qualified ExternalBPP.ExternalAPI.Metro.CMRL.V2.BusinessHour as CMRLV2BusinessHour
import qualified ExternalBPP.ExternalAPI.Metro.CMRL.V2.GetFare as CMRLV2GetFare
import qualified ExternalBPP.ExternalAPI.Metro.CMRL.V2.OperatingHours as CMRLV2OperatingHours
import qualified ExternalBPP.ExternalAPI.Metro.CMRL.V2.Order as CMRLV2Order
import qualified ExternalBPP.ExternalAPI.Metro.CMRL.V2.StationList as CMRLV2StationList
import qualified ExternalBPP.ExternalAPI.Metro.CMRL.V2.TicketDetails as CMRLV2TicketDetails
import qualified ExternalBPP.ExternalAPI.Metro.CMRL.V2.TicketStatus as CMRLV2TicketStatus
import qualified ExternalBPP.ExternalAPI.Metro.KMRL.Cancel as KMRLCancel
import qualified ExternalBPP.ExternalAPI.Metro.KMRL.GetFare as KMRLGetFare
import qualified ExternalBPP.ExternalAPI.Metro.KMRL.Order as KMRLOrder
import qualified ExternalBPP.ExternalAPI.Metro.KMRL.StationList as KMRLStationList
import qualified ExternalBPP.ExternalAPI.Metro.KMRL.Transport as KMRLTransport
import qualified ExternalBPP.ExternalAPI.Subway.CRIS.BookJourney as CRISBookJourney
import qualified ExternalBPP.ExternalAPI.Subway.CRIS.RouteFare as CRISRouteFare
import qualified ExternalBPP.ExternalAPI.Subway.CRIS.RouteFareV3 as CRISRouteFareV3
import qualified ExternalBPP.ExternalAPI.Subway.CRIS.Types as CRISTypes
import ExternalBPP.ExternalAPI.Types
import Kernel.External.Encryption
import Kernel.External.MasterCloudForward (HasMasterCloudForwarder)
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import Kernel.Randomizer
import Kernel.Storage.Esqueleto.Config
import qualified Kernel.Storage.InMem as IM
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.FRFSSeller.Common as FRFSSellerCommon
import qualified SharedLogic.FRFSUtils as FRFSUtils
import qualified Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import qualified Storage.Queries.Person as QPerson
import Tools.Error
import qualified Tools.Metrics.BAPMetrics as Metrics

getProviderName :: IntegratedBPPConfig -> Text
getProviderName integrationBPPConfig =
  case (integrationBPPConfig.providerName, integrationBPPConfig.providerConfig) of
    (Just name, _) -> name
    (_, CMRL _) -> "Chennai Metro Rail Limited"
    (_, CMRLV2 _) -> "Chennai Metro Rail Limited v2"
    (_, EBIX _) -> "Kolkata Buses"
    (_, DIRECT _) -> "Direct Multimodal Services"
    (_, ONDC _) -> "ONDC Services"
    (_, CRIS _) -> "CRIS Subway"
    (_, KMRL _) -> "Kochi Metro Rail Limited"

data BasicRouteDetail = BasicRouteDetail
  { routeCode :: Text,
    startStopCode :: Text,
    endStopCode :: Text,
    color :: Maybe Text
  }
  deriving (Show)

data FareRoute = FareRoute
  { segments :: NonEmpty BasicRouteDetail,
    mbProviderRouteId :: Maybe Text
  }
  deriving (Show)

data SubwayFareDetail = SubwayFareDetail
  { viaPoints :: Text,
    changeOver :: Text,
    rawChangeOver :: Text,
    getAllFares :: Bool
  }
  deriving (Show)

getFares :: (CoreMetrics m, MonadTime m, MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r, EsqDBReplicaFlow m r, ServiceFlow m r, HasShortDurationRetryCfg r c, HasRequestId r, MonadReader r m, HasMasterCloudForwarder r) => Id Person -> Id Merchant -> Id MerchantOperatingCity -> IntegratedBPPConfig -> NonEmpty BasicRouteDetail -> Spec.VehicleCategory -> Maybe Spec.ServiceTierType -> Maybe SubwayFareDetail -> m [FRFSUtils.FRFSFare]
getFares riderId merchantId merchantOperatingCityId integrationBPPConfig fareRouteDetails vehicleCategory serviceTier subwayFareDetail =
  getFaresForJourneyType riderId merchantId merchantOperatingCityId integrationBPPConfig fareRouteDetails vehicleCategory serviceTier subwayFareDetail Nothing

getFaresForJourneyType :: (CoreMetrics m, MonadTime m, MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r, EsqDBReplicaFlow m r, ServiceFlow m r, HasShortDurationRetryCfg r c, HasRequestId r, MonadReader r m, HasMasterCloudForwarder r) => Id Person -> Id Merchant -> Id MerchantOperatingCity -> IntegratedBPPConfig -> NonEmpty BasicRouteDetail -> Spec.VehicleCategory -> Maybe Spec.ServiceTierType -> Maybe SubwayFareDetail -> Maybe DQuote.FRFSQuoteType -> m [FRFSUtils.FRFSFare]
getFaresForJourneyType riderId merchantId merchantOperatingCityId integrationBPPConfig fareRouteDetails vehicleCategory serviceTier subwayFareDetail mbJourneyType = do
  let journeyCode = show @Text $ FRFSSellerCommon.metroJourneyCode (fromMaybe DQuote.SingleJourney mbJourneyType)
  let (routeCode, startStopCode, endStopCode) = getRouteCodeAndStartAndStop
  case integrationBPPConfig.providerConfig of
    CMRL config' ->
      CMRLFareByOriginDest.getFareByOriginDest integrationBPPConfig config' $
        CMRLFareByOriginDest.FareByOriginDestReq
          { origin = startStopCode,
            destination = endStopCode,
            ticketType = journeyCode
          }
    CMRLV2 config' -> do
      now <- getCurrentTime
      let travelDatetime = T.pack $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" now
      CMRLV2GetFare.getFare integrationBPPConfig config' riderId.getId $
        CMRLV2GetFare.GetFareReq
          { operatorNameId = config'.operatorNameId,
            fromStationId = extractStationCode startStopCode,
            toStationId = extractStationCode endStopCode,
            ticketTypeId = CMRLV2Order.ticketTypeIdFor config' (fromMaybe DQuote.SingleJourney mbJourneyType),
            merchantId = config'.merchantId,
            travelDatetime = travelDatetime,
            fareTypeId = config'.fareTypeId
          }
    ONDC _ -> FRFSUtils.getFares riderId vehicleCategory serviceTier integrationBPPConfig merchantId merchantOperatingCityId routeCode startStopCode endStopCode
    EBIX _ -> do
      fares <- FRFSUtils.getFares riderId vehicleCategory serviceTier integrationBPPConfig merchantId merchantOperatingCityId routeCode startStopCode endStopCode
      return $
        map
          ( \FRFSUtils.FRFSFare {..} ->
              let FRFSUtils.FRFSVehicleServiceTier {..} = vehicleServiceTier
               in FRFSUtils.FRFSFare
                    { vehicleServiceTier =
                        FRFSUtils.FRFSVehicleServiceTier
                          { serviceTierType =
                              case serviceTierType of
                                Spec.ASHOK_LEYLAND_AC -> Spec.AC
                                Spec.MIDI_AC -> Spec.AC
                                Spec.VOLVO_AC -> Spec.AC
                                Spec.ELECTRIC_V -> Spec.AC
                                Spec.ELECTRIC_V_PMI -> Spec.AC
                                a -> a,
                            ..
                          },
                      ..
                    }
          )
          fares
    DIRECT _ -> FRFSUtils.getFares riderId vehicleCategory serviceTier integrationBPPConfig merchantId merchantOperatingCityId routeCode startStopCode endStopCode
    CRIS config' -> do
      SubwayFareDetail {viaPoints, changeOver, rawChangeOver, getAllFares} <- subwayFareDetail & fromMaybeM (InternalError "SubwayFareDetail not found")
      callCRISAPI config' changeOver rawChangeOver viaPoints startStopCode endStopCode getAllFares
    KMRL config' -> do
      manager <- KMRLTransport.kmrlManager config'
      fareData <-
        KMRLGetFare.getFare config' manager $
          KMRLGetFare.FareReq
            { travellers = 1,
              sourceStationId = startStopCode,
              destinationStationId = endStopCode,
              metroType = config'.metroType,
              ticketType = journeyCode
            }
      let farePrice = realToFrac fareData.ticketFare
      return
        [ FRFSUtils.FRFSFare
            { categories =
                [ FRFSUtils.FRFSTicketCategory
                    { category = ADULT,
                      price = Price {amountInt = round farePrice, amount = farePrice, currency = INR},
                      offeredPrice = Price {amountInt = round farePrice, amount = farePrice, currency = INR},
                      eligibility = True,
                      bppItemId = FRFSUtils.getProviderName integrationBPPConfig
                    }
                ],
              fareDetails = Nothing,
              farePolicyId = Nothing,
              vehicleServiceTier =
                FRFSUtils.FRFSVehicleServiceTier
                  { serviceTierType = Spec.ORDINARY,
                    serviceTierProviderCode = "ORDINARY",
                    serviceTierShortName = "ORDINARY",
                    serviceTierDescription = "ORDINARY",
                    serviceTierLongName = "ORDINARY",
                    isAirConditioned = Just False
                  },
              fareQuoteType = Nothing,
              fareQuoteId = Nothing
            }
        ]
  where
    callCRISAPI config' changeOver rawChangeOver viaPoints startStopCode endStopCode getAllFares = do
      routeFareReq <- getRouteFareRequest startStopCode endStopCode changeOver rawChangeOver viaPoints riderId (config'.useRouteFareV4 /= Just True)
      resp <- withTryCatch "CRIS:getRouteFare" $ if config'.useRouteFareV4 == Just True then CRISRouteFare.getRouteFare config' merchantOperatingCityId routeFareReq getAllFares else CRISRouteFareV3.getRouteFare config' merchantOperatingCityId routeFareReq getAllFares
      case resp of
        Left err -> do
          logError $ "Error while calling CRIS API: " <> show err
          return []
        Right (fares, _) -> return fares

    getRouteCodeAndStartAndStop :: (Text, Text, Text)
    getRouteCodeAndStartAndStop = do
      let firstFareRouteDetail = NE.head fareRouteDetails
      let lastFareRouteDetail = NE.last fareRouteDetails
      let routeCode = firstFareRouteDetail.routeCode
      let startStopCode = firstFareRouteDetail.startStopCode
      let endStopCode = lastFareRouteDetail.endStopCode
      (routeCode, startStopCode, endStopCode)

getRouteFareRequest :: (CoreMetrics m, MonadFlow m, EsqDBFlow m r, EncFlow m r, CacheFlow m r) => Text -> Text -> Text -> Text -> Text -> Id Person -> Bool -> m CRISTypes.CRISFareRequest
getRouteFareRequest sourceCode destCode changeOver rawChangeOver viaPoints personId useDummy = do
  if useDummy
    then getDummyRouteFareRequest
    else do
      person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
      mbMobileNumber <- mapM decrypt person.mobileNumber
      mbImeiNumber <- mapM decrypt person.imeiNumber
      sessionId <- getRandomInRange (1, 1000000 :: Int)
      return $
        CRISTypes.CRISFareRequest
          { mobileNo = mbMobileNumber,
            imeiNo = fromMaybe "ed409d8d764c04f7" mbImeiNumber,
            appSession = sessionId,
            sourceCode = sourceCode,
            destCode = destCode,
            changeOver = changeOver,
            rawChangeOver = rawChangeOver,
            via = viaPoints
          }
  where
    getDummyRouteFareRequest :: MonadFlow m => m CRISTypes.CRISFareRequest
    getDummyRouteFareRequest = do
      sessionId <- getRandomInRange (1, 1000000 :: Int)
      return $
        CRISTypes.CRISFareRequest
          { mobileNo = Just "1111111111",
            imeiNo = "abcdefgh",
            appSession = sessionId,
            sourceCode = sourceCode,
            destCode = destCode,
            changeOver = changeOver,
            rawChangeOver = rawChangeOver,
            via = viaPoints
          }

extractStationCode :: Text -> Text
extractStationCode code = fromMaybe code $ listToMaybe $ drop 1 $ T.splitOn "|" code

createOrder :: (MonadFlow m, ServiceFlow m r, HasShortDurationRetryCfg r c, Metrics.HasBAPMetrics m r, HasRequestId r, MonadReader r m, HasMasterCloudForwarder r) => IntegratedBPPConfig -> Seconds -> (Maybe Text, Maybe Text) -> FRFSTicketBooking -> [FRFSQuoteCategory] -> m ProviderOrder
createOrder integrationBPPConfig qrTtl riderDetails booking quoteCategories =
  createOrderForJourneyType integrationBPPConfig qrTtl riderDetails booking Nothing quoteCategories Nothing (Just groupSize) Nothing
  where
    fareParameters = FRFSUtils.mkFareParameters (FRFSUtils.mkCategoryPriceItemFromQuoteCategories quoteCategories)
    groupSize = fareParameters.totalQuantity

createOrderForJourneyType :: (MonadFlow m, ServiceFlow m r, HasShortDurationRetryCfg r c, Metrics.HasBAPMetrics m r, HasRequestId r, MonadReader r m, HasMasterCloudForwarder r) => IntegratedBPPConfig -> Seconds -> (Maybe Text, Maybe Text) -> FRFSTicketBooking -> Maybe Text -> [FRFSQuoteCategory] -> Maybe Text -> Maybe Int -> Maybe Int -> m ProviderOrder
createOrderForJourneyType integrationBPPConfig qrTtl (_mRiderName, mRiderNumber) booking mbBuyerSubscriberId quoteCategories mbFareQuoteId mbGrpSize mbNoOfTickets = do
  let bookingJourneyCode = show @Text $ FRFSSellerCommon.metroJourneyCode booking._type
  Metrics.startMetrics Metrics.CREATE_ORDER_FRFS (getProviderName integrationBPPConfig) booking.searchId.getId booking.merchantOperatingCityId.getId
  resp <-
    case integrationBPPConfig.providerConfig of
      CMRL config' -> CMRLOrder.createOrder config' integrationBPPConfig booking quoteCategories mRiderNumber
      CMRLV2 config' -> CMRLV2Order.createOrder config' integrationBPPConfig booking quoteCategories mRiderNumber mbFareQuoteId mbGrpSize mbNoOfTickets
      EBIX config' -> EBIXOrder.createOrder config' integrationBPPConfig qrTtl booking quoteCategories
      DIRECT config' -> DIRECTOrder.createOrder config' integrationBPPConfig qrTtl booking quoteCategories
      CRIS config' -> CRISBookJourney.createOrder config' integrationBPPConfig booking quoteCategories
      KMRL config' -> do
        manager <- KMRLTransport.kmrlManager config'
        let fareParameters = FRFSUtils.mkFareParameters (FRFSUtils.mkCategoryPriceItemFromQuoteCategories quoteCategories)
            travellers = fareParameters.totalQuantity
        ticket <-
          KMRLOrder.bookTicket config' manager $
            KMRLOrder.BookTicketReq
              { -- The booking carries the canonical "code|stationId"; KMRL's roster has no code,
                -- so that reads as "|ALV" while getFare was quoted with the bare id. Strip it.
                sourceStationId = extractStationCode booking.fromStationCode,
                destinationStationId = extractStationCode booking.toStationCode,
                metroType = config'.metroType,
                ticketType = bookingJourneyCode,
                travellers,
                ticketFare = realToFrac booking.totalPrice.amount,
                transactionId = KMRLOrder.toKMRLTransactionId config'.bapTransactionIdPrefixes config'.defaultTransactionIdPrefix (fromMaybe "" mbBuyerSubscriberId) booking.searchId.getId
              }
        now <- getCurrentTime
        let validity = addUTCTime (fromIntegral qrTtl.getSeconds) now
        pure
          ProviderOrder
            { orderId = fromMaybe ticket.ticketRefId booking.bppOrderId,
              tickets =
                replicate
                  (max 1 travellers)
                  ProviderTicket
                    { -- ticketRefId, NOT ticketNo. Every later KMRL call keys off the ref id --
                      -- status (@kochi_metro.go:503@) and both cancels (@:584@, @:631@) send
                      -- @ticketRefId@ -- and ticketNumber is the only id we persist, so storing
                      -- ticketNo here made every KMRL status refresh and cancellation address a
                      -- ticket the gateway cannot resolve. Go keeps both (@:458-459@: TicketRefId
                      -- and Tag) and publishes ticketNo as the buyer-visible NUMBER tag; carrying
                      -- both needs a new frfs_ticket column, which is follow-up work. Until then
                      -- the operator handle wins: a wrong displayed number is cosmetic, a cancel
                      -- that silently fails is not.
                      ticketNumber = ticket.ticketRefId,
                      vehicleNumber = Nothing,
                      description = ticket.ticketTypeDispName,
                      qrData = ticket.ticketGUID,
                      qrStatus = fromMaybe "UNCLAIMED" (KMRLOrder.transformKochiStatus =<< ticket.ticketStatus),
                      qrValidity = validity,
                      qrRefreshAt = Nothing,
                      commencingHours = Nothing
                    }
            }
      _ -> throwError $ InternalError "Unimplemented!"
  Metrics.finishMetrics Metrics.CREATE_ORDER_FRFS (getProviderName integrationBPPConfig) booking.searchId.getId booking.merchantOperatingCityId.getId
  return resp

getBppOrderId :: (CacheFlow m r, EsqDBFlow m r, EncFlow m r, HasRequestId r, MonadReader r m, HasMasterCloudForwarder r) => IntegratedBPPConfig -> FRFSTicketBooking -> m (Maybe Text)
getBppOrderId integratedBPPConfig booking = do
  case integratedBPPConfig.providerConfig of
    CMRL _ -> Just <$> CMRLOrder.getBppOrderId booking
    CMRLV2 _ -> Just <$> CMRLV2Order.getBppOrderId booking
    EBIX _ -> Just <$> EBIXOrder.getBppOrderId booking
    DIRECT _ -> Just <$> DIRECTOrder.getBppOrderId booking
    CRIS _ -> Just <$> CRISBookJourney.getBppOrderId booking
    _ -> return Nothing

getTicketStatus :: (MonadTime m, MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r, HasRequestId r, MonadReader r m, HasMasterCloudForwarder r) => IntegratedBPPConfig -> FRFSTicketBooking -> m [ProviderTicket]
getTicketStatus integrationBPPConfig booking = do
  case integrationBPPConfig.providerConfig of
    CMRL config' -> CMRLStatus.getTicketStatus config' booking
    CMRLV2 config' -> CMRLV2TicketStatus.getTicketStatus config' booking
    EBIX config' -> EBIXStatus.getTicketStatus config' booking
    DIRECT config' -> DIRECTStatus.getTicketStatus config' booking
    CRIS _config' -> return []
    _ -> throwError $ InternalError "Unimplemented!"

softCancelTicket :: (MonadFlow m, EncFlow m r, HasRequestId r, MonadReader r m) => IntegratedBPPConfig -> Text -> m (Maybe KMRLCancel.SoftCancelQuote)
softCancelTicket integrationBPPConfig ticketRefId =
  case integrationBPPConfig.providerConfig of
    KMRL config' -> do
      manager <- KMRLTransport.kmrlManager config'
      Just <$> KMRLCancel.softCancelTicket config' manager ticketRefId
    _ -> pure Nothing

hardCancelTicket :: (MonadFlow m, EncFlow m r, HasRequestId r, MonadReader r m) => IntegratedBPPConfig -> Text -> m (Maybe KMRLCancel.HardCancelResult)
hardCancelTicket integrationBPPConfig ticketRefId =
  case integrationBPPConfig.providerConfig of
    KMRL config' -> do
      manager <- KMRLTransport.kmrlManager config'
      KMRLCancel.hardCancelTicket config' manager ticketRefId
    _ -> pure Nothing

-- | Every branch returns the operator's RAW status code, never a Beckn wire code. The sole
-- consumer, @Domain.Action.Beckn.FRFSSeller.Status.operatorCodeToStatus@, matches on raw codes;
-- wire codes are derived from the domain status afterwards by @Utils.wireTicketStatus@, so a
-- pre-translated code here silently matches nothing and the ticket keeps its stored status.
--
-- For KMRL the raw code is @secondaryTicketStatus@ (UNUSED/USED/cancelled/EXPIRED), which is the
-- field the Go reference feeds to its own kochi status map; the primary @ticketStatus@ is left
-- untranslated there and surfaced as a separate PRIMARY_STATUS tag. See
-- public-transport-bpp/internal/app/publicTransport/metro/kochi_metro.go:475 (the map) and :530
-- (the field it is applied to).
getTicketDetailStatusCode :: (MonadTime m, MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r, HasRequestId r, MonadReader r m, HasMasterCloudForwarder r) => IntegratedBPPConfig -> Text -> m (Maybe Text)
getTicketDetailStatusCode integrationBPPConfig ticketNumber =
  case integrationBPPConfig.providerConfig of
    CMRLV2 config' -> CMRLV2TicketDetails.getTicketDetails config' ticketNumber
    KMRL config' -> do
      manager <- KMRLTransport.kmrlManager config'
      info <- KMRLOrder.getTicketStatus config' manager ticketNumber
      pure (Just info.secondaryTicketStatus)
    _ -> return Nothing

verifyTicket :: (MonadTime m, MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r, HasRequestId r, MonadReader r m, HasMasterCloudForwarder r) => IntegratedBPPConfig -> Text -> m TicketPayload
verifyTicket integrationBPPConfig encryptedQrData = do
  case integrationBPPConfig.providerConfig of
    DIRECT config' -> DIRECTVerify.verifyTicket config' encryptedQrData
    _ -> throwError $ InternalError "Unimplemented!"

generateQR :: (MonadTime m, MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r, HasRequestId r, MonadReader r m, HasMasterCloudForwarder r) => IntegratedBPPConfig -> TicketPayload -> m Text
generateQR integrationBPPConfig ticketPayload = do
  case integrationBPPConfig.providerConfig of
    DIRECT config' -> DirectUTILS.generateQR config' ticketPayload
    _ -> throwError $ InternalError "Unimplemented!"

generateUpdatedQRTicket :: (MonadTime m, MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r, HasRequestId r, MonadReader r m, HasMasterCloudForwarder r) => IntegratedBPPConfig -> Id FRFSTicketBooking -> (TicketPayload -> m TicketPayload) -> m [TicketPayload]
generateUpdatedQRTicket integrationBPPConfig ticketBookingId updateFn = do
  case integrationBPPConfig.providerConfig of
    DIRECT config' -> DIRECTVerify.generateUpdatedQRTicket config' ticketBookingId updateFn
    _ -> throwError $ InternalError "Unimplemented!"

getBusinessHour :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r, CacheFlow m r, EncFlow m r, HasRequestId r, MonadReader r m, HasMasterCloudForwarder r) => IntegratedBPPConfig -> m CMRLBusinessHour.BusinessHourResult
getBusinessHour integrationBPPConfig = do
  case integrationBPPConfig.providerConfig of
    CMRL config' -> CMRLBusinessHour.getBusinessHour config'
    CMRLV2 config' -> do
      response <- CMRLV2BusinessHour.getBusinessHour config'
      -- Convert V2 response to V1 format for compatibility
      let findParam name = maybe "" (.paramValue) $ find (\p -> p.paramName == name) response.commonParamList
      return
        CMRLBusinessHour.BusinessHourResult
          { qrBookingStartTime = findParam "qrBookingStartTime",
            qrBookingEndTime = findParam "qrBookingEndTime",
            businessStartTime = findParam "businessStartTime",
            businessEndTime = findParam "businessEndTime",
            qrTicketRestrictionStartTime = findParam "qrTicketRestrictionStartTime",
            qrTicketRestrictionEndTime = findParam "qrTicketRestrictionEndTime"
          }
    _ -> throwError $ InternalError "Unimplemented!"

getDurationDetails :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r, HasRequestId r, MonadReader r m, HasMasterCloudForwarder r) => IntegratedBPPConfig -> CMRLDurationDetails.DurationDetailsReq -> m [CMRLDurationDetails.DurationDetailsResult]
getDurationDetails integrationBPPConfig req = do
  case integrationBPPConfig.providerConfig of
    CMRL config' -> CMRLDurationDetails.getDurationDetails config' req
    _ -> throwError $ InternalError "Unimplemented!"

getFareMatrix :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r, HasRequestId r, MonadReader r m, HasMasterCloudForwarder r) => IntegratedBPPConfig -> m [CMRLFareMatrix.FareMatrixRes]
getFareMatrix integrationBPPConfig = do
  case integrationBPPConfig.providerConfig of
    CMRL config' -> CMRLFareMatrix.getFareMatrix config'
    _ -> throwError $ InternalError "Unimplemented!"

getPassengerViewStatus :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r, HasRequestId r, MonadReader r m, HasMasterCloudForwarder r) => IntegratedBPPConfig -> CMRLPassengerViewStatus.PassengerViewStatusReq -> m [CMRLPassengerViewStatus.TicketDetails]
getPassengerViewStatus integrationBPPConfig req = do
  case integrationBPPConfig.providerConfig of
    CMRL config' -> CMRLPassengerViewStatus.getPassengerViewStatus config' req
    _ -> throwError $ InternalError "Unimplemented!"

getStationList :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r, HasRequestId r, MonadReader r m, HasMasterCloudForwarder r) => IntegratedBPPConfig -> m [CMRLStationList.Station]
getStationList integrationBPPConfig =
  IM.withInMemCache ["FRFSStationList", integrationBPPConfig.id.getId] 3600 $ do
    stations <- fetchStationList integrationBPPConfig
    when (null stations) . throwError $ InternalError "Operator returned an empty station roster"
    pure stations

fetchStationList :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r, HasRequestId r, MonadReader r m, HasMasterCloudForwarder r) => IntegratedBPPConfig -> m [CMRLStationList.Station]
fetchStationList integrationBPPConfig =
  case integrationBPPConfig.providerConfig of
    CMRL config' -> CMRLStationList.getStationList config'
    CMRLV2 config' -> map fromCMRLV2Station <$> CMRLV2StationList.getStationList config'
    KMRL config' -> do
      manager <- KMRLTransport.kmrlManager config'
      KMRLStationList.toCMRLStations <$> KMRLStationList.getStationList config' manager
    _ -> throwError $ InternalError "Unimplemented!"

fromCMRLV2Station :: CMRLV2StationList.Station -> CMRLStationList.Station
fromCMRLV2Station s =
  CMRLStationList.Station
    { id = 0,
      lineId = maybe "" show s.lineId,
      stationId = s.stationUniqueid,
      code = s.stationShortName,
      name = s.stationName,
      taName = s.stationNameTamil,
      address = "",
      latitude = fromMaybe 0.0 (readMaybe . T.unpack =<< s.latitude),
      longitude = fromMaybe 0.0 (readMaybe . T.unpack =<< s.longitude),
      sequenceNo = fromMaybe 0 s.sequenceNo
    }

getOperatingHoursTags :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r, HasRequestId r, MonadReader r m, HasMasterCloudForwarder r) => IntegratedBPPConfig -> m [(Text, Text)]
getOperatingHoursTags integrationBPPConfig =
  case integrationBPPConfig.providerConfig of
    CMRLV2 config' -> do
      result <-
        try @_ @SomeException $
          IM.withInMemCache ["FRFSOperatingHoursTags", integrationBPPConfig.id.getId] 3600 $ do
            tags <- CMRLV2OperatingHours.getOperatingHoursTags config'
            when (null tags) . throwError $ InternalError "Operator returned no usable operating-hours params"
            pure tags
      case result of
        Right tags -> pure tags
        Left err -> do
          logWarning $ "Operating hours unavailable for " <> integrationBPPConfig.id.getId <> ", publishing without them: " <> show err
          pure []
    _ -> pure []

getOperatingWindow :: (CoreMetrics m, MonadFlow m, CacheFlow m r, EncFlow m r, HasRequestId r, MonadReader r m, HasMasterCloudForwarder r) => IntegratedBPPConfig -> m (Maybe (UTCTime, UTCTime))
getOperatingWindow integrationBPPConfig =
  CMRLV2OperatingHours.operatingWindowFromTags <$> getOperatingHoursTags integrationBPPConfig

getPaymentDetails :: (MonadFlow m) => Merchant -> MerchantOperatingCity -> BecknConfig -> (Maybe Text, Maybe Text) -> FRFSTicketBooking -> m BknPaymentParams
getPaymentDetails _merchant _merchantOperatingCity _bapConfig (_mRiderName, _mRiderNumber) _booking = throwError $ InternalError "getPaymentDetails: Unimplemented!"

getChangeOverAndViaPoints :: (MonadFlow m, ServiceFlow m r, HasShortDurationRetryCfg r c) => [BasicRouteDetail] -> IntegratedBPPConfig -> m (Text, Text, Text)
getChangeOverAndViaPoints fareRouteDetails integrationBPPConfig = do
  allStations <- buildStations fareRouteDetails integrationBPPConfig
  let stationCodes = map (.stationCode) allStations
      viaStations = case stationCodes of
        [] -> []
        [_] -> []
        xs -> nub $ drop 1 (take (length xs - 1) xs)
      changeOverStationCodes = nub $ concatMap (\rd -> [rd.startStopCode, rd.endStopCode]) fareRouteDetails
      changeOverPoints = case changeOverStationCodes of
        [] -> []
        [_] -> []
        xs -> nub $ drop 1 (take (length xs - 1) xs)
      configuredChangeOverStations = case integrationBPPConfig.providerConfig of
        CRIS config -> fromMaybe [] (changeOverIndirectStations config) <> fromMaybe [] (changeOverDirectStations config)
        _ -> []
      changeOverStations = filter (`elem` configuredChangeOverStations) changeOverPoints
      viaPoints = if null viaStations then " " else T.intercalate "-" viaStations
      changeOver = if null changeOverStations then " " else T.intercalate "-" changeOverStations
      rawChangeOver = if null changeOverPoints then " " else T.intercalate "-" changeOverPoints
  return (viaPoints, changeOver, rawChangeOver)

buildStations :: (MonadFlow m, ServiceFlow m r, HasShortDurationRetryCfg r c) => [BasicRouteDetail] -> IntegratedBPPConfig -> m [DStation]
buildStations basicRouteDetails integratedBPPConfig = do
  let lastStopIndex = length basicRouteDetails - 1
  stationsArray <- do
    mapWithIndexM
      ( \idx routeDetail -> do
          let startStopType = if idx == 0 then START else TRANSIT
          let endStopType = if idx == lastStopIndex then END else TRANSIT
          fromStation <- OTPRest.getStationByGtfsIdAndStopCode routeDetail.startStopCode integratedBPPConfig >>= fromMaybeM (StationNotFound routeDetail.startStopCode)
          toStation <- OTPRest.getStationByGtfsIdAndStopCode routeDetail.endStopCode integratedBPPConfig >>= fromMaybeM (StationNotFound routeDetail.endStopCode)
          stops <- OTPRest.getRouteStopMappingByRouteCode routeDetail.routeCode integratedBPPConfig
          return $ fromMaybe [] (mkStations fromStation toStation stops startStopType endStopType routeDetail.color)
      )
      basicRouteDetails
  return $ concat stationsArray
  where
    mapWithIndexM f = zipWithM f [0 ..]

mkStations :: Station -> Station -> [RouteStopMapping] -> StationType -> StationType -> Maybe Text -> Maybe [DStation]
mkStations fromStation toStation stops startStopType endStopType routeColor =
  ((,) <$> find (\stop -> stop.stopCode == fromStation.code) stops <*> find (\stop -> stop.stopCode == toStation.code) stops)
    <&> \(startStop, endStop) ->
      do
        let startStation = DStation startStop.stopCode startStop.stopName (Just startStop.stopPoint.lat) (Just startStop.stopPoint.lon) startStopType (Just startStop.sequenceNum) Nothing routeColor
            endStation = DStation endStop.stopCode endStop.stopName (Just endStop.stopPoint.lat) (Just endStop.stopPoint.lon) endStopType (Just endStop.sequenceNum) Nothing routeColor
            intermediateStations =
              sortOn (.sequenceNum) (filter (\stop -> stop.sequenceNum > startStop.sequenceNum && stop.sequenceNum < endStop.sequenceNum) stops)
                <&> (\stop -> DStation stop.stopCode stop.stopName (Just stop.stopPoint.lat) (Just stop.stopPoint.lon) INTERMEDIATE (Just stop.sequenceNum) Nothing routeColor)
        [startStation] ++ intermediateStations ++ [endStation]
