{-# OPTIONS_GHC -Wno-orphans #-}

module SharedLogic.Offer where

import qualified Data.Aeson as A
import qualified Data.Text as T
import Data.Time (utctDay)
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as Person
import qualified Domain.Types.PersonStats as DPS
import Domain.Types.Yudhishthira ()
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Payment.Interface.Types as Payment
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import Kernel.Storage.Clickhouse.Config
import Kernel.Storage.Esqueleto.Config
import Kernel.Types.CacheFlow
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.JourneyModule.Types as JL
import qualified Lib.Payment.Domain.Action as DPayment
import Lib.Payment.Domain.Types.Offer as DOffer
import qualified Lib.Payment.Domain.Types.OfferStats as DOfferStats
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import qualified Lib.Payment.Domain.Types.PersonDailyOfferStats as DPDOS
import Lib.Payment.Storage.Queries.Offer as QOffer
import qualified Lib.Payment.Storage.Queries.OfferStats as QOfferStats
import qualified Lib.Payment.Storage.Queries.PersonDailyOfferStats as QPersonDailyOfferStats
import Lib.Yudhishthira.Storage.Beam.BeamFlow
import qualified Lib.Yudhishthira.Tools.DebugLog as LYDL
import qualified Lib.Yudhishthira.Types as LYT
import qualified Lib.Yudhishthira.TypesTH as YTH
import qualified SharedLogic.Utils as SLUtils
import Storage.Beam.Payment ()
import Storage.ConfigPilot.Config.RiderConfig (RiderDimensions (..))
import Storage.ConfigPilot.Interface.Types (getConfig)
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.PersonStats as QPersonStats
import qualified Tools.DynamicLogic as TDL
import Tools.Error
import qualified Tools.Payment as TPayment

data CumulativeOfferRespI = CumulativeOfferRespI
  { offerTitle :: Text,
    offerDescription :: Text,
    offerSponsoredBy :: [Text],
    offerIds :: [Text],
    offerListResp :: Payment.OfferListResp
  }
  deriving (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CumulativeOfferResp = CumulativeOfferResp
  { offerTitle :: Text,
    offerDescription :: Text,
    offerSponsoredBy :: [Text],
    offerIds :: [Text],
    offerListResp :: [OfferRespAPIEntity]
  }
  deriving (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data CumulativeOfferReq = CumulativeOfferReq
  { offerListResp :: Payment.OfferListResp,
    extraParams :: [JL.LegInfo]
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data OfferRespAPIEntity = OfferRespAPIEntity
  { offerId :: Text,
    offerTitle :: Maybe Text,
    offerDescription :: Maybe Text,
    offerTnc :: Maybe Text,
    offerSponsoredBy :: Maybe Text,
    offerCode :: Text,
    amountSaved :: HighPrecMoney,
    postOfferAmount :: HighPrecMoney
  }
  deriving (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data OffersRespAPIEntity = OffersRespAPIEntity
  { offers :: [OfferRespAPIEntity],
    totalAmountSaved :: HighPrecMoney,
    totalPostOfferAmount :: HighPrecMoney
  }
  deriving (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data OfferEligibilityInput = OfferEligibilityInput
  { personOfferStats :: [DOfferStats.OfferStats],
    staticPersonOfferStats :: [DOfferStats.OfferStats],
    deviceOfferStats :: [DOfferStats.OfferStats],
    personDailyOfferStats :: Maybe DPDOS.PersonDailyOfferStats,
    personStats :: Maybe DPS.PersonStats
  }
  deriving (Generic, Show, ToJSON, FromJSON)

$(YTH.generateGenericDefault ''OfferEligibilityInput)

-------------------------------------------------------------------------------------------------------
----------------------------------- Fetch Offers List With Caching ------------------------------------
-------------------------------------------------------------------------------------------------------
isDomainOffersEnabled :: DOrder.PaymentServiceType -> Bool
isDomainOffersEnabled paymentServiceType = paymentServiceType `elem` [DOrder.OnlineRideHailing, DOrder.RideHailing]

invalidateOfferListCache :: (MonadFlow m, CacheFlow m r, EncFlow m r, ServiceFlow m r, EsqDBReplicaFlow m r, EsqDBFlow m r) => Person.Person -> Id DMOC.MerchantOperatingCity -> DOrder.PaymentServiceType -> Price -> m ()
invalidateOfferListCache person merchantOperatingCityId paymentServiceType price = do
  riderConfig <- getConfig (RiderDimensions {merchantOperatingCityId = merchantOperatingCityId.getId}) >>= fromMaybeM (RiderConfigDoesNotExist merchantOperatingCityId.getId)
  req <- mkOfferListReq person price
  let customerId = fromMaybe person.id.getId (req.customer <&> (.customerId))
      version = fromMaybe "N/A" riderConfig.offerListCacheVersion
  DPayment.invalidateOfferListCacheService customerId version paymentServiceType

offerListCache :: (MonadFlow m, CacheFlow m r, EncFlow m r, ServiceFlow m r, EsqDBReplicaFlow m r) => Id Merchant.Merchant -> Id Person.Person -> Id DMOC.MerchantOperatingCity -> DOrder.PaymentServiceType -> Price -> m Payment.OfferListResp
offerListCache merchantId personId merchantOperatingCityId paymentServiceType price = do
  person <- QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  riderConfig <- getConfig (RiderDimensions {merchantOperatingCityId = merchantOperatingCityId.getId}) >>= fromMaybeM (RiderConfigDoesNotExist merchantOperatingCityId.getId)
  req <- mkOfferListReq person price
  let customerId = fromMaybe person.id.getId (req.customer <&> (.customerId))
      version = fromMaybe "N/A" riderConfig.offerListCacheVersion
  if isDomainOffersEnabled paymentServiceType
    then do
      let domainOfferCall = \_ -> do
            personOfferStats <- QOfferStats.findAllByEntityIdAndEntityType personId.getId DOfferStats.Person
            staticPersonOfferStats <- do
              personPhone <- mapM decrypt person.mobileNumber
              case personPhone of
                Just phone -> do
                  staticId <- SLUtils.getStaticCustomerId person phone
                  if staticId /= personId.getId
                    then QOfferStats.findAllByEntityIdAndEntityType staticId DOfferStats.StaticPerson
                    else pure []
                Nothing -> pure []
            deviceOfferStats <- case person.deviceId of
              Just did | not (T.null did) -> QOfferStats.findAllByEntityIdAndEntityType did DOfferStats.Device
              _ -> pure []
            -- offerStats <- QOfferStats.findAllByEntityIdAndEntityType offerId.getId DOfferStats.Offer
            today <- utctDay <$> getCurrentTime
            mbPersonDailyOfferStats <- QPersonDailyOfferStats.findByPersonIdAndDate personId.getId today
            mbPersonStats <- QPersonStats.findByPersonId personId
            let domainContext =
                  Just $
                    A.toJSON
                      OfferEligibilityInput
                        { personOfferStats = personOfferStats,
                          staticPersonOfferStats = staticPersonOfferStats,
                          deviceOfferStats = deviceOfferStats,
                          personDailyOfferStats = mbPersonDailyOfferStats,
                          personStats = mbPersonStats
                        }
            DPayment.listDomainOffers merchantId.getId merchantOperatingCityId.getId price.amount price.currency domainContext
      DPayment.offerListService customerId version paymentServiceType (6 * 3600) False domainOfferCall req
    else do
      let offerListCall = TPayment.offerList merchantId merchantOperatingCityId Nothing paymentServiceType (Just customerId) person.clientSdkVersion
      DPayment.offerListService customerId version paymentServiceType (31 * 86400) True offerListCall req

selectedOfferListCache :: (MonadFlow m, CacheFlow m r, EncFlow m r, ServiceFlow m r, EsqDBReplicaFlow m r) => Id Merchant.Merchant -> Id Person.Person -> Id DMOC.MerchantOperatingCity -> DOrder.PaymentServiceType -> Price -> Text -> m Payment.OfferListResp
selectedOfferListCache merchantId personId merchantOperatingCityId paymentServiceType price offerId = do
  resp <- offerListCache merchantId personId merchantOperatingCityId paymentServiceType price
  let filteredOffers = filter (\o -> o.offerId == offerId) resp.offerResp
      filteredCombo =
        resp.bestOfferCombination <&> \combo ->
          Payment.BestOfferCombination
            { offers = filter (\o -> o.offerId == offerId) combo.offers,
              orderBreakup = combo.orderBreakup
            }
  pure
    Payment.OfferListResp
      { offerResp = filteredOffers,
        bestOfferCombination = filteredCombo
      }

getSelectedOfferDetails :: (MonadFlow m, CacheFlow m r, EncFlow m r, ServiceFlow m r, EsqDBReplicaFlow m r) => Id Merchant.Merchant -> Id Person.Person -> Id DMOC.MerchantOperatingCity -> DOrder.PaymentServiceType -> Price -> Text -> m (Maybe (OfferRespAPIEntity, DPayment.ComputedOfferAmount))
getSelectedOfferDetails merchantId personId merchantOperatingCityId paymentServiceType price offerId = do
  resp <- selectedOfferListCache merchantId personId merchantOperatingCityId paymentServiceType price offerId
  case listToMaybe resp.offerResp of
    Nothing -> pure Nothing
    Just offer -> do
      mbComputed <- getOfferAmount (Id offerId) price.amount
      case mbComputed of
        Nothing -> pure Nothing
        Just computed -> pure $ Just (mkOfferRespAPIEntity offer, computed)

getOfferAmount :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id DOffer.Offer -> HighPrecMoney -> m (Maybe DPayment.ComputedOfferAmount)
getOfferAmount offerId amount = do
  mbOffer <- QOffer.findById offerId
  case mbOffer of
    Just offer -> do
      let computed = DPayment.computeOfferAmount offer amount
      logInfo $ "Offer " <> offerId.getId <> " applied: amount=" <> show amount <> " postOffer=" <> show computed.postOfferAmount <> " discount=" <> show computed.discountAmount
      pure $ Just computed
    Nothing -> do
      logError $ "Offer not found: " <> offerId.getId <> ", proceeding with full amount"
      pure Nothing

mkCumulativeOfferResp :: (MonadFlow m, EncFlow m r, BeamFlow m r, ClickhouseFlow m r) => Id DMOC.MerchantOperatingCity -> Payment.OfferListResp -> [JL.LegInfo] -> m (Maybe CumulativeOfferResp)
mkCumulativeOfferResp merchantOperatingCityId offerListRes legInfos = do
  now <- getCurrentTime
  (logics, _) <- TDL.getAppDynamicLogic (cast merchantOperatingCityId) LYT.CUMULATIVE_OFFER_POLICY now Nothing Nothing
  if null logics
    then do
      logInfo "No cumulative offer logic found."
      pure Nothing
    else do
      logInfo $ "Running cumulative offer logic with " <> show (length logics) <> " rules"
      result <- LYDL.runLogicsWithDebugLog LYDL.Rider (cast merchantOperatingCityId) LYT.CUMULATIVE_OFFER_POLICY logics (CumulativeOfferReq offerListRes legInfos)
      case A.fromJSON result.result :: A.Result CumulativeOfferRespI of
        A.Success logicResult -> do
          logInfo $ "Cumulative offer logic result: " <> show logicResult
          pure $ Just $ mkCumulativeOfferRespFromI logicResult
        A.Error err -> do
          logError $ "Failed to parse cumulative offer logic result: " <> show err
          pure Nothing
  where
    mkCumulativeOfferRespFromI :: CumulativeOfferRespI -> CumulativeOfferResp
    mkCumulativeOfferRespFromI CumulativeOfferRespI {..} = do
      CumulativeOfferResp
        { offerListResp = map mkOfferRespAPIEntity offerListResp.offerResp,
          ..
        }

mkOfferRespAPIEntity :: Payment.OfferResp -> OfferRespAPIEntity
mkOfferRespAPIEntity Payment.OfferResp {..} = do
  OfferRespAPIEntity
    { offerId = offerId,
      offerTitle = offerDescription.title,
      offerDescription = offerDescription.description,
      offerTnc = offerDescription.tnc,
      offerSponsoredBy = offerDescription.sponsoredBy,
      offerCode = offerCode,
      amountSaved = discountAmount,
      postOfferAmount = finalOrderAmount
    }

mkOfferListReq :: (MonadFlow m, EncFlow m r, EsqDBReplicaFlow m r, CacheFlow m r, EsqDBFlow m r) => Person.Person -> Price -> m Payment.OfferListReq
mkOfferListReq person price = do
  now <- getCurrentTime
  email <- mapM decrypt person.email
  personPhone <- person.mobileNumber & fromMaybeM (PersonFieldNotPresent "mobileNumber") >>= decrypt
  staticCustomerId <- SLUtils.getStaticCustomerId person personPhone
  let offerOrder = Payment.OfferOrder {orderId = Nothing, amount = price.amount, currency = price.currency}
      customerReq = Payment.OfferCustomer {customerId = staticCustomerId, email = email, mobile = Nothing}
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
