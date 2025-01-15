{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.PartnerOrganizationFRFS
  ( API,
    handler,
  )
where

import qualified API.Types.UI.FRFSTicketService as DFRFSTypes
import qualified BecknV2.FRFS.Enums as Spec
import qualified BecknV2.FRFS.Utils as Utils
import qualified Domain.Action.UI.FRFSTicketService as DFRFSTicketService
import qualified Domain.Action.UI.PartnerOrganizationFRFS as DPOFRFS
import Domain.Types.FRFSQuote as Quote
import qualified Domain.Types.FRFSSearch as DFRFSSearch
import qualified Domain.Types.FRFSTicketBooking as DFTB
import qualified Domain.Types.PartnerOrgConfig as DPOC
import qualified Domain.Types.PartnerOrgStation as DPOS
import Domain.Types.PartnerOrganization
import Environment
import EulerHS.Prelude hiding (map)
import qualified ExternalBPP.CallAPI as CallExternalBPP
import Kernel.Beam.Functions as B
import qualified Kernel.External.Maps as Maps
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as DB
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import qualified Kernel.Types.Logging as Log
import Kernel.Utils.Common hiding (withLogTag)
import Kernel.Utils.SlidingWindowLimiter (checkSlidingWindowLimitWithOptions)
import qualified Lib.JourneyLeg.Types as JPT
import Servant hiding (route, throwError)
import Storage.Beam.SystemConfigs ()
import qualified Storage.CachedQueries.BecknConfig as QBC
import qualified Storage.CachedQueries.FRFSConfig as CQFC
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.PartnerOrgConfig as CQPOC
import qualified Storage.CachedQueries.PartnerOrgStation as CQPOS
import qualified Storage.CachedQueries.Station as CQS
import qualified Storage.Queries.FRFSQuote as QQuote
import qualified Storage.Queries.FRFSSearch as QSearch
import qualified Storage.Queries.FRFSTicketBooking as QBooking
import qualified Storage.Queries.Route as QRoute
import Tools.Auth
import Tools.Error

type API =
  "frfs"
    :> ( "partnerOrganization"
           :> PartnerOrganizationAPIKey
           :> ( "upsertPersonAndGetFare"
                  :> ReqBody '[JSON] DPOFRFS.GetFareReq
                  :> Post '[JSON] DPOFRFS.GetFareResp
                  :<|> "getConfig"
                    :> ( "fromStation"
                           :> Capture "fromGMMStationId" (Id DPOS.PartnerOrgStation)
                           :> "toStation"
                           :> Capture "toGMMStationId" (Id DPOS.PartnerOrgStation)
                           :> Get '[JSON] DPOFRFS.GetConfigResp
                       )
                  :<|> "getFareV2"
                    :> ReqBody '[JSON] DPOFRFS.GetFareReqV2
                    :> Post '[JSON] DPOFRFS.GetFareRespV2
                  :<|> "partnerQuoteConfirm"
                    :> ReqBody '[JSON] DPOFRFS.PartnerQuoteConfirmReq
                    :> Post '[JSON] DPOFRFS.PartnerQuoteConfirmRes
              )
           :<|> "shareTicketInfo"
             :> Capture "ticketBookingId" (Id DFTB.FRFSTicketBooking)
             :> Get '[JSON] DPOFRFS.ShareTicketInfoResp
       )

handler :: FlowServer API
handler =
  handlerForAPIKey
    :<|> shareTicketInfo
  where
    handlerForAPIKey pOrg =
      upsertPersonAndGetFare pOrg
        :<|> getConfigByStationIds pOrg
        :<|> getFareV2 pOrg
        :<|> partnerQuoteConfirm pOrg

upsertPersonAndGetFare :: PartnerOrganization -> DPOFRFS.GetFareReq -> FlowHandler DPOFRFS.GetFareResp
upsertPersonAndGetFare partnerOrg req = withFlowHandlerAPI . withLogTag $ do
  checkRateLimit partnerOrg.orgId getFareHitsCountKey

  fromStation <- B.runInReplica $ CQS.findByStationCodeAndMerchantOperatingCityId req.fromStationCode req.cityId >>= fromMaybeM (StationDoesNotExist $ "StationCode:" +|| req.fromStationCode ||+ "cityId:" +|| req.cityId.getId ||+ "")
  toStation <- B.runInReplica $ CQS.findByStationCodeAndMerchantOperatingCityId req.toStationCode req.cityId >>= fromMaybeM (StationDoesNotExist $ "StationCode:" +|| req.toStationCode ||+ "cityId:" +|| req.cityId.getId ||+ "")
  let merchantId = fromStation.merchantId
  unless (merchantId == partnerOrg.merchantId) $
    throwError . InvalidRequest $ "apiKey of partnerOrgId:" +|| partnerOrg.orgId ||+ " not valid for merchantId:" +|| merchantId ||+ ""

  route <-
    maybe
      (pure Nothing)
      ( \routeCode' -> do
          route' <- B.runInReplica $ QRoute.findByRouteCode routeCode' >>= fromMaybeM (RouteNotFound routeCode')
          return $ Just route'
      )
      req.routeCode
  pOrgCfg <- B.runInReplica $ CQPOC.findByIdAndCfgType partnerOrg.orgId DPOC.REGISTRATION >>= fromMaybeM (PartnerOrgConfigNotFound partnerOrg.orgId.getId $ show DPOC.REGISTRATION)
  regPOCfg <- DPOC.getRegistrationConfig pOrgCfg.config

  let mbRegCoordinates = mkLatLong fromStation.lat fromStation.lon
  (personId, token) <- DPOFRFS.upsertPersonAndGetToken partnerOrg.orgId regPOCfg req.cityId merchantId mbRegCoordinates req
  merchantOperatingCity <- CQMOC.findById req.cityId >>= fromMaybeM (MerchantOperatingCityNotFound req.cityId.getId)

  Log.withLogTag ("FRFS:GetFare:PersonId:" <> personId.getId) $ do
    let frfsSearchReq = buildFRFSSearchReq fromStation.code toStation.code (route <&> (.code)) req.numberOfPassengers Nothing
        frfsVehicleType = fromStation.vehicleType
    res <- DFRFSTicketService.postFrfsSearchHandler (Just personId, merchantId) (Just merchantOperatingCity.city) frfsVehicleType frfsSearchReq req.partnerOrgTransactionId (Just partnerOrg.orgId) Nothing Nothing Nothing

    return $ DPOFRFS.GetFareResp {searchId = res.searchId, ..}
  where
    withLogTag = Log.withLogTag ("FRFS:UpsertPersonAndGetFare:PartnerOrgId:" <> getId partnerOrg.orgId)

    getFareHitsCountKey :: Text
    getFareHitsCountKey = "BAP:FRFS:PartnerOrgId:" <> getId partnerOrg.orgId <> ":GetFare:hitsCount"

    buildFRFSSearchReq :: Text -> Text -> Maybe Text -> Int -> Maybe JPT.JourneySearchData -> DFRFSTypes.FRFSSearchAPIReq
    buildFRFSSearchReq fromStationCode toStationCode routeCode quantity journeySearchData = DFRFSTypes.FRFSSearchAPIReq {..}

getConfigByStationIds :: PartnerOrganization -> Id DPOS.PartnerOrgStation -> Id DPOS.PartnerOrgStation -> FlowHandler DPOFRFS.GetConfigResp
getConfigByStationIds partnerOrg fromGMMStationId toGMMStationId = withFlowHandlerAPI . withLogTag $ do
  void $ checkRateLimit partnerOrg.orgId getConfigHitsCountKey

  DPOFRFS.getConfigByStationIds partnerOrg fromGMMStationId toGMMStationId
  where
    withLogTag = Log.withLogTag ("FRFS:GetConfig:PartnerOrgId:" <> getId partnerOrg.orgId)

    getConfigHitsCountKey :: Text
    getConfigHitsCountKey = "BAP:FRFS:PartnerOrgId:" <> getId partnerOrg.orgId <> ":GetConfig:hitsCount"

shareTicketInfo :: Id DFTB.FRFSTicketBooking -> FlowHandler DPOFRFS.ShareTicketInfoResp
shareTicketInfo bookingId = withFlowHandlerAPI . withLogTag $ do
  DPOFRFS.shareTicketInfo bookingId
  where
    withLogTag = Log.withLogTag ("FRFS:ShareTicketInfo:TicketBookingId:" <> bookingId.getId)

checkRateLimit ::
  ( Redis.HedisFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    DB.EsqDBReplicaFlow m r
  ) =>
  Id PartnerOrganization ->
  Text ->
  m ()
checkRateLimit partnerOrgId apiHitsCountKey = Log.withLogTag "checkRateLimit" $ do
  pOrgCfg <- B.runInReplica $ CQPOC.findByIdAndCfgType partnerOrgId DPOC.RATE_LIMIT >>= fromMaybeM (PartnerOrgConfigNotFound partnerOrgId.getId $ show DPOC.RATE_LIMIT)
  rateLimitPOCfg <- DPOC.getRateLimitConfig pOrgCfg.config
  checkSlidingWindowLimitWithOptions apiHitsCountKey rateLimitPOCfg.rateLimitOptions

getFareV2 :: PartnerOrganization -> DPOFRFS.GetFareReqV2 -> FlowHandler DPOFRFS.GetFareRespV2
getFareV2 partnerOrg req = withFlowHandlerAPI . Log.withLogTag "getFareV2" $ do
  fromStation <- B.runInReplica $ CQS.findByStationCodeAndMerchantOperatingCityId req.fromStationCode req.cityId >>= fromMaybeM (StationDoesNotExist $ "StationCode:" +|| req.fromStationCode ||+ "cityId:" +|| req.cityId.getId ||+ "")
  toStation <- B.runInReplica $ CQS.findByStationCodeAndMerchantOperatingCityId req.toStationCode req.cityId >>= fromMaybeM (StationDoesNotExist $ "StationCode:" +|| req.toStationCode ||+ "cityId:" +|| req.cityId.getId ||+ "")
  frfsConfig <- CQFC.findByMerchantOperatingCityId req.cityId >>= fromMaybeM (InternalError $ "FRFS config not found for merchant operating city Id " <> show req.cityId)
  let merchantId = fromStation.merchantId
      frfsVehicleType = fromStation.vehicleType
  merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  bapConfig <- QBC.findByMerchantIdDomainAndVehicle merchantId (show Spec.FRFS) (Utils.frfsVehicleCategoryToBecknVehicleCategory frfsVehicleType) >>= fromMaybeM (InternalError $ "Beckn Config not found for merchantId- " <> merchantId.getId)
  route <-
    maybe
      (pure Nothing)
      ( \routeCode' -> do
          route' <- B.runInReplica $ QRoute.findByRouteCode routeCode' >>= fromMaybeM (RouteNotFound routeCode')
          return $ Just route'
      )
      req.routeCode
  merchantOperatingCity <- CQMOC.findById fromStation.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantOperatingCityId- " <> show fromStation.merchantOperatingCityId)
  searchReqId <- generateGUID
  now <- getCurrentTime
  let searchReq =
        DFRFSSearch.FRFSSearch
          { id = searchReqId,
            vehicleType = frfsVehicleType,
            merchantId,
            merchantOperatingCityId = fromStation.merchantOperatingCityId,
            createdAt = now,
            updatedAt = now,
            quantity = 1,
            fromStationId = fromStation.id,
            toStationId = toStation.id,
            routeId = route <&> (.id),
            riderId = "partnerOrg_rider_id",
            partnerOrgTransactionId = req.partnerOrgTransactionId,
            partnerOrgId = Just partnerOrg.orgId,
            journeyLegInfo = Nothing,
            frequency = Nothing,
            isOnSearchReceived = Nothing,
            lineColor = Nothing,
            lineColorCode = Nothing,
            ..
          }
  let validTill = addUTCTime (intToNominalDiffTime 900) now
  let isroundTripAllowed = frfsConfig.roundTripTicketLimit > 0
  singleJourneyQuotes <-
    CQPOS.findCachedQuoteByFromToProviderIdAndQuoteType fromStation.id toStation.id (fromMaybe "" frfsConfig.providerId) SingleJourney >>= \case
      Nothing -> pure []
      Just a -> do
        quoteId <- generateGUID
        let quote =
              Quote.FRFSQuote
                { Quote._type = SingleJourney,
                  Quote.bppItemId = "partnerOrg_bpp_item_id",
                  Quote.bppSubscriberId = "partnerOrg_bpp_subscriber_id",
                  Quote.bppSubscriberUrl = "partnerOrg_bpp_subscriber_url",
                  Quote.fromStationId = fromStation.id,
                  Quote.id = quoteId,
                  Quote.price = a.price,
                  Quote.providerDescription = Nothing,
                  Quote.providerId = fromMaybe "" frfsConfig.providerId,
                  Quote.providerName = fromMaybe "" frfsConfig.providerName,
                  Quote.quantity = 1,
                  Quote.riderId = "partnerOrg_rider_id",
                  Quote.searchId = searchReqId,
                  Quote.stationsJson = a.stationsJson,
                  Quote.routeStationsJson = Nothing,
                  Quote.discountsJson = Nothing,
                  Quote.toStationId = toStation.id,
                  Quote.validTill,
                  Quote.vehicleType = frfsVehicleType,
                  Quote.merchantId = merchantId,
                  Quote.merchantOperatingCityId = fromStation.merchantOperatingCityId,
                  Quote.partnerOrgId = Just partnerOrg.orgId,
                  Quote.partnerOrgTransactionId = req.partnerOrgTransactionId,
                  Quote.createdAt = now,
                  Quote.updatedAt = now,
                  Quote.bppDelayedInterest = Nothing,
                  Quote.discountedTickets = Nothing,
                  Quote.eventDiscountAmount = Nothing,
                  Quote.oldCacheDump = Nothing
                }
        return [quote]

  returnJourneyQuotes <-
    if isroundTripAllowed
      then do
        CQPOS.findCachedQuoteByFromToProviderIdAndQuoteType fromStation.id toStation.id (fromMaybe "" frfsConfig.providerId) ReturnJourney >>= \case
          Nothing -> pure []
          Just a -> do
            quoteId <- generateGUID
            let quote =
                  Quote.FRFSQuote
                    { Quote._type = ReturnJourney,
                      Quote.bppItemId = "partnerOrg_bpp_item_id",
                      Quote.bppSubscriberId = "partnerOrg_bpp_subscriber_id",
                      Quote.bppSubscriberUrl = "partnerOrg_bpp_subscriber_url",
                      Quote.fromStationId = fromStation.id,
                      Quote.id = quoteId,
                      Quote.price = a.price,
                      Quote.providerDescription = Nothing,
                      Quote.providerId = fromMaybe "" frfsConfig.providerId,
                      Quote.providerName = fromMaybe "" frfsConfig.providerName,
                      Quote.quantity = 1,
                      Quote.riderId = "partnerOrg_rider_id",
                      Quote.searchId = searchReqId,
                      Quote.stationsJson = a.stationsJson,
                      Quote.routeStationsJson = Nothing,
                      Quote.discountsJson = Nothing,
                      Quote.toStationId = toStation.id,
                      Quote.validTill,
                      Quote.vehicleType = frfsVehicleType,
                      Quote.merchantId = merchantId,
                      Quote.merchantOperatingCityId = fromStation.merchantOperatingCityId,
                      Quote.partnerOrgId = Just partnerOrg.orgId,
                      Quote.partnerOrgTransactionId = req.partnerOrgTransactionId,
                      Quote.createdAt = now,
                      Quote.updatedAt = now,
                      Quote.bppDelayedInterest = Nothing,
                      Quote.discountedTickets = Nothing,
                      Quote.eventDiscountAmount = Nothing,
                      Quote.oldCacheDump = Nothing
                    }
            return [quote]
      else return []
  let quotes = singleJourneyQuotes ++ returnJourneyQuotes
  QQuote.createMany quotes
  quoteRes <- mapM mkQuoteRes quotes
  fork ("FRFS Search: " <> searchReqId.getId) $ do
    QSearch.create searchReq
    CallExternalBPP.search merchant merchantOperatingCity bapConfig searchReq
  return $
    DPOFRFS.GetFareRespV2
      { searchId = searchReqId,
        quotes = quoteRes
      }

partnerQuoteConfirm :: PartnerOrganization -> DPOFRFS.PartnerQuoteConfirmReq -> FlowHandler DPOFRFS.PartnerQuoteConfirmRes
partnerQuoteConfirm partnerOrg req = withFlowHandlerAPI . Log.withLogTag "partnerQuoteConfirm" $ do
  search <- QSearch.findById req.searchId >>= fromMaybeM (SearchRequestDoesNotExist req.searchId.getId)
  if isJust search.isOnSearchReceived
    then do
      quote <- QQuote.findById req.quoteId >>= fromMaybeM (InternalError $ "Cannot find quote" <> req.quoteId.getId)
      fromStation <- CQS.findById quote.fromStationId >>= fromMaybeM (StationDoesNotExist $ "StationId: " <> quote.fromStationId.getId)
      toStation <- CQS.findById quote.toStationId >>= fromMaybeM (StationDoesNotExist $ "StationId: " <> quote.toStationId.getId)
      pOrgCfg <- B.runInReplica $ CQPOC.findByIdAndCfgType partnerOrg.orgId DPOC.REGISTRATION >>= fromMaybeM (PartnerOrgConfigNotFound partnerOrg.orgId.getId $ show DPOC.REGISTRATION)
      regPOCfg <- DPOC.getRegistrationConfig pOrgCfg.config
      let getFareReq =
            DPOFRFS.GetFareReq
              { DPOFRFS.fromStationCode = fromStation.code,
                DPOFRFS.toStationCode = toStation.code,
                DPOFRFS.routeCode = Nothing,
                DPOFRFS.numberOfPassengers = req.numberOfPassengers,
                DPOFRFS.mobileCountryCode = req.mobileCountryCode,
                DPOFRFS.mobileNumber = req.mobileNumber,
                DPOFRFS.identifierType = req.identifierType,
                DPOFRFS.partnerOrgTransactionId = Nothing,
                DPOFRFS.cityId = fromStation.merchantOperatingCityId
              }
      let mbRegCoordinates = mkLatLong fromStation.lat fromStation.lon
      (personId, token) <- DPOFRFS.upsertPersonAndGetToken partnerOrg.orgId regPOCfg fromStation.merchantOperatingCityId fromStation.merchantId mbRegCoordinates getFareReq
      QSearch.updateRiderIdById personId req.searchId
      QQuote.updateManyRiderIdBySearchId personId req.searchId
      bookingRes <- DFRFSTicketService.postFrfsQuoteConfirm (Just personId, fromStation.merchantId) quote.id
      let isFareChanged = isJust quote.oldCacheDump
      QBooking.updateisFareChangedByQuoteId (Just isFareChanged) req.quoteId
      let body = DPOFRFS.PartnerQuoteConfirmResBody {bookingStatusRes = bookingRes{isFareChanged = Just isFareChanged}, token}
      return
        DPOFRFS.PartnerQuoteConfirmRes
          { body = Just body,
            onSearchStatus = DPOFRFS.RECEIVED
          }
    else
      return
        DPOFRFS.PartnerQuoteConfirmRes
          { onSearchStatus = DPOFRFS.STILL_WAITING,
            body = Nothing
          }

mkLatLong :: Maybe Double -> Maybe Double -> Maybe Maps.LatLong
mkLatLong mbLat mbLon = case (mbLat, mbLon) of
  (Just lat, Just lon) -> Just $ Maps.LatLong {..}
  _ -> Nothing

mkQuoteRes :: (MonadFlow m) => Quote.FRFSQuote -> m DFRFSTypes.FRFSQuoteAPIRes
mkQuoteRes quote = do
  (stations :: [DFRFSTypes.FRFSStationAPI]) <- decodeFromText quote.stationsJson & fromMaybeM (InternalError "Invalid stations jsons from db")
  let routeStations :: Maybe [DFRFSTypes.FRFSRouteStationsAPI] = decodeFromText =<< quote.routeStationsJson
  let discounts :: Maybe [DFRFSTypes.FRFSDiscountRes] = decodeFromText =<< quote.discountsJson
  return $
    DFRFSTypes.FRFSQuoteAPIRes
      { quoteId = quote.id,
        _type = quote._type,
        price = quote.price.amount,
        priceWithCurrency = mkPriceAPIEntity quote.price,
        quantity = quote.quantity,
        validTill = quote.validTill,
        vehicleType = quote.vehicleType,
        discountedTickets = quote.discountedTickets,
        eventDiscountAmount = quote.eventDiscountAmount,
        ..
      }
