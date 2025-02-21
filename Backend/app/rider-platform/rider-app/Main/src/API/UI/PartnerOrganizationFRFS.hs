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
import BecknV2.FRFS.Utils
import qualified Domain.Action.UI.FRFSTicketService as DFRFSTicketService
import qualified Domain.Action.UI.PartnerOrganizationFRFS as DPOFRFS
import Domain.Types.FRFSRouteDetails
import qualified Domain.Types.FRFSTicketBooking as DFTB
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.PartnerOrgConfig as DPOC
import Domain.Types.PartnerOrganization
import Environment
import EulerHS.Prelude hiding (map)
import Kernel.Beam.Functions as B
import qualified Kernel.Storage.Esqueleto as DB
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import qualified Kernel.Types.Logging as Log
import Kernel.Utils.Common hiding (withLogTag)
import Kernel.Utils.SlidingWindowLimiter (checkSlidingWindowLimitWithOptions)
import qualified Lib.JourneyLeg.Types as JPT
import Servant hiding (route, throwError)
import Storage.Beam.SystemConfigs ()
import qualified Storage.CachedQueries.IntegratedBPPConfig as QIBC
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.PartnerOrgConfig as CQPOC
import qualified Storage.CachedQueries.Station as CQS
import qualified Storage.Queries.FRFSQuote as QQuote
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
                           :> Capture "fromGMMStationId" Text
                           :> "toStation"
                           :> Capture "toGMMStationId" Text
                           :> Get '[JSON] DPOFRFS.GetConfigResp
                       )
                  :<|> "getFareV2"
                    :> ReqBody '[JSON] DPOFRFS.GetFareReqV2
                    :> Post '[JSON] DPOFRFS.GetFareRespV2
                  :<|> "upsertPersonAndQuoteConfirm"
                    :> ReqBody '[JSON] DPOFRFS.UpsertPersonAndQuoteConfirmReq
                    :> Post '[JSON] DPOFRFS.UpsertPersonAndQuoteConfirmRes
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
        :<|> upsertPersonAndQuoteConfirm pOrg

upsertPersonAndGetFare :: PartnerOrganization -> DPOFRFS.GetFareReq -> FlowHandler DPOFRFS.GetFareResp
upsertPersonAndGetFare partnerOrg req = withFlowHandlerAPI . withLogTag $ do
  checkRateLimit partnerOrg.orgId getFareHitsCountKey

  let vehicleType = fromMaybe Spec.METRO req.vehicleType
  merchantOperatingCity <- CQMOC.findById req.cityId >>= fromMaybeM (MerchantOperatingCityNotFound req.cityId.getId)
  integratedBPPConfig <- QIBC.findByDomainAndCityAndVehicleCategory (show Spec.FRFS) merchantOperatingCity.id (frfsVehicleCategoryToBecknVehicleCategory vehicleType) DIBC.PARTNERORG >>= fromMaybeM (IntegratedBPPConfigNotFound $ "MerchantOperatingCityId:" +|| merchantOperatingCity.id.getId ||+ "Domain:" +|| Spec.FRFS ||+ "Vehicle:" +|| frfsVehicleCategoryToBecknVehicleCategory vehicleType ||+ "Platform Type:" +|| DIBC.PARTNERORG ||+ "")

  fromStation <- B.runInReplica $ CQS.findByStationCodeAndIntegratedBPPConfigId req.fromStationCode integratedBPPConfig.id >>= fromMaybeM (StationDoesNotExist $ "StationCode:" +|| req.fromStationCode ||+ "integratedBPPConfigId:" +|| integratedBPPConfig.id.getId ||+ "")
  toStation <- B.runInReplica $ CQS.findByStationCodeAndIntegratedBPPConfigId req.toStationCode integratedBPPConfig.id >>= fromMaybeM (StationDoesNotExist $ "StationCode:" +|| req.toStationCode ||+ "integratedBPPConfigId:" +|| integratedBPPConfig.id.getId ||+ "")
  let merchantId = fromStation.merchantId
  unless (merchantId == partnerOrg.merchantId) $
    throwError . InvalidRequest $ "apiKey of partnerOrgId:" +|| partnerOrg.orgId ||+ " not valid for merchantId:" +|| merchantId ||+ ""

  route <-
    maybe
      (pure Nothing)
      ( \routeCode' -> do
          route' <- B.runInReplica $ QRoute.findByRouteCode routeCode' integratedBPPConfig.id >>= fromMaybeM (RouteNotFound routeCode')
          return $ Just route'
      )
      req.routeCode
  pOrgCfg <- B.runInReplica $ CQPOC.findByIdAndCfgType partnerOrg.orgId DPOC.REGISTRATION >>= fromMaybeM (PartnerOrgConfigNotFound partnerOrg.orgId.getId $ show DPOC.REGISTRATION)
  regPOCfg <- DPOC.getRegistrationConfig pOrgCfg.config

  let mbRegCoordinates = DPOFRFS.mkLatLong fromStation.lat fromStation.lon
  (personId, token) <- DPOFRFS.upsertPersonAndGetToken partnerOrg.orgId regPOCfg req.cityId merchantId mbRegCoordinates req

  Log.withLogTag ("FRFS:GetFare:PersonId:" <> personId.getId) $ do
    let frfsSearchReq = buildFRFSSearchReq fromStation.code toStation.code (route <&> (.code)) req.numberOfPassengers Nothing
        frfsVehicleType = fromStation.vehicleType
        frfsRouteDetails =
          [ FRFSRouteDetails
              { routeCode = route <&> (.code),
                startStationCode = fromStation.code,
                endStationCode = toStation.code
              }
          ]
    res <- DFRFSTicketService.postFrfsSearchHandler (Just personId, merchantId) (Just merchantOperatingCity.city) frfsVehicleType frfsSearchReq frfsRouteDetails req.partnerOrgTransactionId (Just partnerOrg.orgId) [] DIBC.PARTNERORG
    return $ DPOFRFS.GetFareResp {searchId = res.searchId, ..}
  where
    withLogTag = Log.withLogTag ("FRFS:UpsertPersonAndGetFare:PartnerOrgId:" <> getId partnerOrg.orgId)

    getFareHitsCountKey :: Text
    getFareHitsCountKey = "BAP:FRFS:PartnerOrgId:" <> getId partnerOrg.orgId <> ":GetFare:hitsCount"

    buildFRFSSearchReq :: Text -> Text -> Maybe Text -> Int -> Maybe JPT.JourneySearchData -> DFRFSTypes.FRFSSearchAPIReq
    buildFRFSSearchReq fromStationCode toStationCode routeCode quantity journeySearchData = DFRFSTypes.FRFSSearchAPIReq {..}

getConfigByStationIds :: PartnerOrganization -> Text -> Text -> FlowHandler DPOFRFS.GetConfigResp
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
getFareV2 partnerOrg req = withFlowHandlerAPI . withLogTag $ do
  checkRateLimit partnerOrg.orgId getFareV2HitsCountKey

  let vehicleType = fromMaybe Spec.METRO req.vehicleType
  merchantOperatingCity <- CQMOC.findById req.cityId >>= fromMaybeM (MerchantOperatingCityNotFound req.cityId.getId)
  integratedBPPConfig <-
    QIBC.findByDomainAndCityAndVehicleCategory (show Spec.FRFS) merchantOperatingCity.id (frfsVehicleCategoryToBecknVehicleCategory vehicleType) DIBC.PARTNERORG
      >>= fromMaybeM (IntegratedBPPConfigNotFound $ "MerchantOperatingCityId:" +|| merchantOperatingCity.id.getId ||+ "Domain:" +|| Spec.FRFS ||+ "Vehicle:" +|| frfsVehicleCategoryToBecknVehicleCategory vehicleType ||+ "Platform Type:" +|| DIBC.PARTNERORG ||+ "")

  fromStation <- B.runInReplica $ CQS.findByStationCodeAndIntegratedBPPConfigId req.fromStationCode integratedBPPConfig.id >>= fromMaybeM (StationDoesNotExist $ "StationCode:" +|| req.fromStationCode ||+ "integratedBPPConfigId:" +|| integratedBPPConfig.id.getId ||+ "")
  toStation <- B.runInReplica $ CQS.findByStationCodeAndIntegratedBPPConfigId req.toStationCode integratedBPPConfig.id >>= fromMaybeM (StationDoesNotExist $ "StationCode:" +|| req.fromStationCode ||+ "integratedBPPConfigId:" +|| integratedBPPConfig.id.getId ||+ "")
  let merchantId = fromStation.merchantId
  unless (merchantId == partnerOrg.merchantId) $
    throwError . InvalidRequest $ "apiKey of partnerOrgId:" +|| partnerOrg.orgId ||+ " not valid for merchantId:" +|| merchantId ||+ ""

  DPOFRFS.getFareV2 partnerOrg fromStation toStation req.partnerOrgTransactionId req.routeCode
  where
    withLogTag = Log.withLogTag ("FRFS:GetFareV2:PartnerOrgId:" <> partnerOrg.orgId.getId)

    getFareV2HitsCountKey :: Text
    getFareV2HitsCountKey = "BAP:FRFS:PartnerOrgId:" <> partnerOrg.orgId.getId <> ":GetFareV2:hitsCount"

upsertPersonAndQuoteConfirm :: PartnerOrganization -> DPOFRFS.UpsertPersonAndQuoteConfirmReq -> FlowHandler DPOFRFS.UpsertPersonAndQuoteConfirmRes
upsertPersonAndQuoteConfirm partnerOrg req = withFlowHandlerAPI . withLogTag $ do
  checkRateLimit partnerOrg.orgId partnerQuoteConfirmHitsCountKey
  quote <- QQuote.findById req.quoteId >>= fromMaybeM (FRFSQuoteNotFound req.quoteId.getId)
  let merchantId = quote.merchantId
  unless (merchantId == partnerOrg.merchantId) $
    throwError . InvalidRequest $ "apiKey of partnerOrgId:" +|| partnerOrg.orgId ||+ " not valid for merchantId:" +|| merchantId ||+ ""

  DPOFRFS.upsertPersonAndQuoteConfirm partnerOrg req
  where
    withLogTag = Log.withLogTag ("FRFS:UpsertPersonAndQuoteConfirm:PartnerOrgId:" <> partnerOrg.orgId.getId <> " FRFS:UpsertPersonAndQuoteConfirm:SearchId:" <> req.searchId.getId)
    partnerQuoteConfirmHitsCountKey :: Text
    partnerQuoteConfirmHitsCountKey = "BAP:FRFS:PartnerOrgId:" <> partnerOrg.orgId.getId <> ":UpsertPersonAndQuoteConfirm:hitsCount"
