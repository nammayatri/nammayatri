module SharedLogic.Offer where

import qualified Data.Aeson as A
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as Person
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Payment.Interface.Types as Payment
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.CacheFlow
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.JourneyModule.Types as JL
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import Lib.Yudhishthira.Storage.Beam.BeamFlow
import qualified Lib.Yudhishthira.Tools.Utils as LYTU
import qualified Lib.Yudhishthira.Types as LYT
import Storage.Beam.Payment ()
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRC
import qualified Storage.Queries.Person as QPerson
import qualified Tools.DynamicLogic as TDL
import Tools.Error
import qualified Tools.Payment as TPayment

data CumulativeOfferResp = CumulativeOfferResp
  { offerTitle :: Text,
    offerDescription :: Text,
    offerSponsoredBy :: [Text],
    offerIds :: [Text]
  }
  deriving (Generic, Show, Read)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CumulativeOfferReq = CumulativeOfferReq
  { offerListResp :: Payment.OfferListResp,
    extraParams :: [JL.LegInfo]
  }
  deriving (Generic, Show, FromJSON, ToJSON)

-------------------------------------------------------------------------------------------------------
----------------------------------- Fetch Offers List With Caching ------------------------------------
-------------------------------------------------------------------------------------------------------

invalidateOfferListCache :: (MonadFlow m, CacheFlow m r, EncFlow m r, ServiceFlow m r) => Person.Person -> Id DMOC.MerchantOperatingCity -> DOrder.PaymentServiceType -> Price -> m ()
invalidateOfferListCache person merchantOperatingCityId paymentServiceType price = do
  riderConfig <- QRC.findByMerchantOperatingCityId merchantOperatingCityId Nothing >>= fromMaybeM (RiderConfigDoesNotExist merchantOperatingCityId.getId)
  _req <- mkOfferListReq person price
  let version = fromMaybe "N/A" riderConfig.offerListCacheVersion
      key = makeOfferListCacheKey person version paymentServiceType
  Redis.withCrossAppRedis $ Redis.del key

offerListCache :: (MonadFlow m, CacheFlow m r, EncFlow m r, ServiceFlow m r) => Id Merchant.Merchant -> Id Person.Person -> Id DMOC.MerchantOperatingCity -> DOrder.PaymentServiceType -> Price -> m Payment.OfferListResp
offerListCache merchantId personId merchantOperatingCityId paymentServiceType price = do
  person <- QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  riderConfig <- QRC.findByMerchantOperatingCityId merchantOperatingCityId Nothing >>= fromMaybeM (RiderConfigDoesNotExist merchantOperatingCityId.getId)
  req <- mkOfferListReq person price
  let version = fromMaybe "N/A" riderConfig.offerListCacheVersion
      key = makeOfferListCacheKey person version paymentServiceType
  Redis.withCrossAppRedis $ do
    Redis.get key >>= \case
      Just a -> return a
      Nothing ->
        ( \resp -> do
            Redis.setExp key resp (31 * 86400) -- Cache for 31 days
            return resp
        )
          =<< TPayment.offerList merchantId merchantOperatingCityId Nothing paymentServiceType (Just person.id.getId) person.clientSdkVersion req

mkCumulativeOfferResp :: (MonadFlow m, EncFlow m r, BeamFlow m r) => Id DMOC.MerchantOperatingCity -> Payment.OfferListResp -> [JL.LegInfo] -> m (Maybe CumulativeOfferResp)
mkCumulativeOfferResp merchantOperatingCityId offerListResp legInfos = do
  now <- getCurrentTime
  (logics, _) <- TDL.getAppDynamicLogic (cast merchantOperatingCityId) (LYT.CUMULATIVE_OFFER_POLICY) now Nothing Nothing
  if null logics
    then do
      logInfo "No cumulative offer logic found."
      pure Nothing
    else do
      logInfo $ "Running cumulative offer logic with " <> show (length logics) <> " rules"
      result <- LYTU.runLogics logics (CumulativeOfferReq offerListResp legInfos)
      case A.fromJSON result.result :: A.Result CumulativeOfferResp of
        A.Success logicResult -> do
          logInfo $ "Cumulative offer logic result: " <> show logicResult
          pure $ Just logicResult
        A.Error err -> do
          logError $ "Failed to parse cumulative offer logic result: " <> show err
          pure Nothing

mkOfferListReq :: (MonadFlow m, EncFlow m r) => Person.Person -> Price -> m Payment.OfferListReq
mkOfferListReq person price = do
  now <- getCurrentTime
  email <- mapM decrypt person.email
  let offerOrder = Payment.OfferOrder {orderId = Nothing, amount = price.amount, currency = price.currency}
      customerReq = Payment.OfferCustomer {customerId = person.id.getId, email = email, mobile = Nothing}
  return
    Payment.OfferListReq
      { order = offerOrder,
        customer = Just customerReq,
        -- These are used as filters for Driver, not required as of now, can be made generic in future.
        planId = "dummy-not-required",
        registrationDate = addUTCTime 19800 now,
        dutyDate = addUTCTime 19800 now,
        paymentMode = "dummy-not-required",
        numOfRides = 0,
        offerListingMetric = Nothing
      }

makeOfferListCacheKey :: Person.Person -> Text -> DOrder.PaymentServiceType -> Text
makeOfferListCacheKey person version serviceType = "OfferList:CId" <> person.id.getId <> ":V-" <> version <> ":ST-" <> show serviceType
