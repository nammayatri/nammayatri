module API.UI
  ( API,
    handler,
    uiApiPrefix,
  )
where

import qualified API.Action.UI.AttractionRecommend as AttractionRecommend
import qualified API.Action.UI.BBPS as BBPS
import qualified API.Action.UI.CRIS as CRIS
import qualified API.Action.UI.Cac as Cac
import qualified API.Action.UI.CancellationChargesWaiveOff as CancellationChargesWaiveOff
import qualified API.Action.UI.CancellationReasons as CancellationReasons
import qualified API.Action.UI.CustomerReferral as CustomerReferral
import qualified API.Action.UI.DeletedPerson as DeletedPerson
import qualified API.Action.UI.Dispatcher as Dispatcher
import qualified API.Action.UI.EDCMachine as EDCMachine
import qualified API.Action.UI.EditLocation as EditLocation
import qualified API.Action.UI.EstimateBP as EstimateBP
import qualified API.Action.UI.FRFSTicketService as FRFSTicketService
import qualified API.Action.UI.FavouriteDriver as FavouriteDriver
import qualified API.Action.UI.FinanceInvoice as FinanceInvoice
import qualified API.Action.UI.FollowRide as FollowRide
import qualified API.Action.UI.Insurance as Insurance
import qualified API.Action.UI.Invoice as Invoice
import qualified API.Action.UI.Loyalty as Loyalty
import qualified API.Action.UI.Metrics as Metrics
import qualified API.Action.UI.Miscellaneous as Miscellaneous
import qualified API.Action.UI.MultimodalConfirm as MultimodalConfirm
import qualified API.Action.UI.NearbyBuses as NearbyBuses
import qualified API.Action.UI.NearbyDrivers as NearbyDrivers
import qualified API.Action.UI.NyRegularSubscription as NYRegular
import qualified API.Action.UI.PartnerBookingStatement as PartnerBookingStatement
import qualified API.Action.UI.PassDetails as PassDetails
import qualified API.Action.UI.PickupInstructions as PickupInstructions
import qualified API.Action.UI.Places as Places
import qualified API.Action.UI.PriceBreakup as PriceBreakup
import qualified API.Action.UI.Rewards as Rewards
import qualified API.Action.UI.RidePayment as RidePayment
import qualified API.Action.UI.RiderLocation as RiderLocation
import qualified API.Action.UI.SVP as SVP
import qualified API.Action.UI.SocialLogin as SocialLogin
import qualified API.Action.UI.Sos as SosApi
import qualified API.Action.UI.TicketKapture as TicketKapture
import qualified API.Action.UI.TicketService as TicketService
import qualified API.Action.UI.TrackRoute as TrackRoute
import qualified API.Action.UI.TriggerFCM as TriggerFCM
import qualified API.Action.UI.ZendeskSdkToken as ZendeskSdkToken
import qualified API.UI.AadhaarVerification as AadhaarVerification
import qualified API.UI.AddBaggage as AddBaggage
import qualified API.UI.AppInstalls as AppInstalls
import qualified API.UI.Booking as Booking
import qualified API.UI.Call as Call
import qualified API.UI.CallEvent as CallEvent
import qualified API.UI.Cancel as Cancel
import qualified API.UI.CancelSearch as CancelSearch
import qualified API.UI.CancellationReason as CancellationReason
import qualified API.UI.ChangeServiceTier as ChangeServiceTier
import qualified API.UI.Confirm as Confirm
import qualified API.UI.Disability as Disability
import qualified API.UI.FeedbackForm as FeedbackForm
import qualified API.UI.Frontend as Frontend
import qualified API.UI.GoogleTranslate as GoogleTranslateProxy
import qualified API.UI.HotSpot as HotSpot
import qualified API.UI.Issue as Issue
import qualified API.UI.Maps as MapsProxy
import qualified API.UI.ParkingBooking as ParkingBooking
import qualified API.UI.PartnerOrganizationFRFS as PartnerOrgFRFS
import qualified API.UI.Pass as Pass
import qualified API.UI.Payment as Payment
import qualified API.UI.PersonStats as PersonStats
import qualified API.UI.Profile as Profile
import qualified API.UI.Quote as Quote
import qualified API.UI.Rating as Rating
import qualified API.UI.Registration as Registration
import qualified API.UI.RentalsIntercityCache as RentalsIntercityCache
import qualified API.UI.Ride as Ride
import qualified API.UI.Route as Route
import qualified API.UI.SavedReqLocation as SavedReqLocation
import qualified API.UI.Search as Search
import qualified API.UI.Select as Select
import qualified API.UI.Serviceability as Serviceability
import qualified API.UI.Sos as Sos
import qualified API.UI.Support as Support
import qualified API.UI.Whatsapp as Whatsapp
import qualified Data.Text as T
import qualified Database.Persist.Sql as Persist
import qualified Database.Redis as Redis
import qualified Database.Redis.Cluster as RedisCluster
import Environment
import EulerHS.Prelude
import GHC.TypeLits (symbolVal)
import Servant
import System.Timeout (timeout)

-- for multi-cloud proxy
type UIAPIPrefix = "v2"

uiApiPrefix :: Text
uiApiPrefix = T.pack $ symbolVal (Proxy @UIAPIPrefix)

type API =
  UIAPIPrefix
    :> ( Get '[JSON] Text
           :<|> Registration.API
           :<|> Profile.API
           :<|> RidePayment.API
           :<|> Payment.API
           :<|> Payment.S2SAPI
           :<|> Loyalty.API
           :<|> Search.API
           :<|> Select.API
           :<|> Quote.API
           :<|> Confirm.API
           :<|> Booking.API
           :<|> CancellationReasons.API
           :<|> Cancel.API
           :<|> CancelSearch.API
           :<|> Ride.API
           :<|> Call.API
           :<|> Support.API
           :<|> Route.API
           :<|> Serviceability.API
           :<|> Rating.API
           :<|> FeedbackForm.API
           :<|> MapsProxy.API
           :<|> GoogleTranslateProxy.API
           :<|> CancellationReason.API
           :<|> CancellationChargesWaiveOff.API
           :<|> SavedReqLocation.API
           :<|> Frontend.API
           :<|> Whatsapp.API
           :<|> Sos.API
           :<|> CallEvent.API
           :<|> AppInstalls.API
           :<|> PersonStats.API
           :<|> HotSpot.API
           :<|> Disability.API
           :<|> AadhaarVerification.API
           :<|> Issue.API
           :<|> TicketService.API
           :<|> FinanceInvoice.API
           :<|> Invoice.API
           :<|> PriceBreakup.API
           :<|> FollowRide.API
           :<|> SosApi.API
           :<|> FRFSTicketService.API
           :<|> Cac.API
           :<|> CustomerReferral.API
           :<|> DeletedPerson.API
           :<|> EditLocation.API
           :<|> SocialLogin.API
           :<|> EstimateBP.API
           :<|> FavouriteDriver.API
           :<|> PartnerOrgFRFS.API
           :<|> TriggerFCM.API
           :<|> MultimodalConfirm.API
           :<|> TrackRoute.API
           :<|> BBPS.API
           :<|> RentalsIntercityCache.API
           :<|> Miscellaneous.API
           :<|> NearbyDrivers.API
           :<|> NearbyBuses.API
           :<|> Places.API
           :<|> CRIS.API
           :<|> TicketKapture.API
           :<|> Insurance.API
           :<|> Metrics.API
           :<|> PickupInstructions.API
           :<|> NYRegular.API
           :<|> Rewards.API
           :<|> AttractionRecommend.API
           :<|> RiderLocation.API
           :<|> Pass.API
           :<|> ParkingBooking.API
           :<|> Dispatcher.API
           :<|> EDCMachine.API
           :<|> PartnerBookingStatement.API
           :<|> PassDetails.API
           :<|> SVP.API
           :<|> ChangeServiceTier.API
           :<|> AddBaggage.API
           :<|> ZendeskSdkToken.API
       )

-- Healthcheck for every datastore connection the app holds, so a pod with any
-- broken connection (e.g. a stale cluster shard map caching a dead node) fails
-- its probe and gets restarted instead of serving errors. For clustered redis
-- we PING every master of the cached shard map: a plain PING only reaches the
-- shard owning slot 0 and would miss dead nodes for other slots.
allConnectionsHealthCheck :: FlowHandler Text
allConnectionsHealthCheck = do
  env <- asks (.appEnv)
  let redisEnvs =
        [ ("clusterRedis", Just env.hedisClusterEnv),
          ("secondaryClusterRedis", env.secondaryHedisClusterEnv)
        ]
      dbEnvs = [("db", env.esqDBEnv), ("replicaDb", env.esqDBReplicaEnv)]
  dbErrors <- liftIO $ catMaybes <$> forM dbEnvs (\(name, dbEnv) -> runCheck name (checkDb dbEnv))
  redisErrors <- liftIO $ catMaybes <$> forM redisEnvs (\(name, mbHedisEnv) -> maybe (pure Nothing) (runCheck name . checkRedis) mbHedisEnv)
  case dbErrors <> redisErrors of
    [] -> pure "Healthy"
    errors -> do
      let msg = "HealthCheck failed: " <> intercalate ", " errors
      liftIO $ putStrLn @String msg
      throwM err503 {errBody = encodeUtf8 msg}
  where
    perCheckTimeoutUs = 2000000
    runCheck :: String -> IO Bool -> IO (Maybe String)
    runCheck name check = do
      (result :: Either SomeException (Maybe Bool)) <- try $ timeout perCheckTimeoutUs check
      pure $ case result of
        Right (Just True) -> Nothing
        Right (Just False) -> Just (name <> ": check failed")
        Right Nothing -> Just (name <> ": timed out")
        Left err -> Just (name <> ": " <> show err)
    checkDb dbEnv = do
      (result :: [Persist.Single Int]) <- Persist.runSqlPool (Persist.rawSql "SELECT 1" []) dbEnv.connPool
      pure $ not (null result)
    checkRedis hedisEnv = case hedisEnv.hedisConnection of
      Redis.ClusteredConnection _ clusterConn -> do
        replies <- RedisCluster.requestMasterNodes clusterConn ["PING"]
        pure $ not (null replies) && all isPong replies
      nonClusteredConn -> do
        res <- Redis.runRedis nonClusteredConn Redis.ping
        pure $ res == Right Redis.Pong
    isPong = \case
      Redis.SingleLine "PONG" -> True
      _ -> False

handler :: FlowServer API
handler =
  allConnectionsHealthCheck
    :<|> Registration.handler
    :<|> Profile.handler
    :<|> RidePayment.handler
    :<|> Payment.handler
    :<|> Payment.handlerS2S
    :<|> Loyalty.handler
    :<|> Search.handler
    :<|> Select.handler
    :<|> Quote.handler
    :<|> Confirm.handler
    :<|> Booking.handler
    :<|> CancellationReasons.handler
    :<|> Cancel.handler
    :<|> CancelSearch.handler
    :<|> Ride.handler
    :<|> Call.handler
    :<|> Support.handler
    :<|> Route.handler
    :<|> Serviceability.handler
    :<|> Rating.handler
    :<|> FeedbackForm.handler
    :<|> MapsProxy.handler
    :<|> GoogleTranslateProxy.handler
    :<|> CancellationReason.handler
    :<|> CancellationChargesWaiveOff.handler
    :<|> SavedReqLocation.handler
    :<|> Frontend.handler
    :<|> Whatsapp.handler
    :<|> Sos.handler
    :<|> CallEvent.handler
    :<|> AppInstalls.handler
    :<|> PersonStats.handler
    :<|> HotSpot.handler
    :<|> Disability.handler
    :<|> AadhaarVerification.handler
    :<|> Issue.handler
    :<|> TicketService.handler
    :<|> FinanceInvoice.handler
    :<|> Invoice.handler
    :<|> PriceBreakup.handler
    :<|> FollowRide.handler
    :<|> SosApi.handler
    :<|> FRFSTicketService.handler
    :<|> Cac.handler
    :<|> CustomerReferral.handler
    :<|> DeletedPerson.handler
    :<|> EditLocation.handler
    :<|> SocialLogin.handler
    :<|> EstimateBP.handler
    :<|> FavouriteDriver.handler
    :<|> PartnerOrgFRFS.handler
    :<|> TriggerFCM.handler
    :<|> MultimodalConfirm.handler
    :<|> TrackRoute.handler
    :<|> BBPS.handler
    :<|> RentalsIntercityCache.handler
    :<|> Miscellaneous.handler
    :<|> NearbyDrivers.handler
    :<|> NearbyBuses.handler
    :<|> Places.handler
    :<|> CRIS.handler
    :<|> TicketKapture.handler
    :<|> Insurance.handler
    :<|> Metrics.handler
    :<|> PickupInstructions.handler
    :<|> NYRegular.handler
    :<|> Rewards.handler
    :<|> AttractionRecommend.handler
    :<|> RiderLocation.handler
    :<|> Pass.handler
    :<|> ParkingBooking.handler
    :<|> Dispatcher.handler
    :<|> EDCMachine.handler
    :<|> PartnerBookingStatement.handler
    :<|> PassDetails.handler
    :<|> SVP.handler
    :<|> ChangeServiceTier.handler
    :<|> AddBaggage.handler
    :<|> ZendeskSdkToken.handler
