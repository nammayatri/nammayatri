module Domain.Action.UI.DriverIncentiveCoins
  ( getCoinsIncentiveConfig,
    getCoinsIncentiveRideCount,
  )
where

import qualified API.Types.UI.DriverIncentiveCoins as API
import qualified Crypto.Hash as Hash
import Data.Aeson (encode)
import qualified Data.ByteString as BS
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import qualified Domain.Types.Coins.CoinsConfig as CoinsConfig
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as SP
import qualified Domain.Types.TransporterConfig as DTC
import qualified Domain.Types.VehicleCategory as DTV
import qualified Domain.Types.VehicleVariant as VecVariant
import Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Beam.Functions as B
import Kernel.Types.Error
import Kernel.Types.Id
import qualified Kernel.Types.TimeBound as TB
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getConfig, getOneConfig)
import qualified Lib.DriverCoins.Coins as Coins
import qualified Lib.DriverCoins.IncentiveMetrics as IncentiveMetrics
import qualified Lib.DriverCoins.Types as DCT
import qualified Lib.Yudhishthira.Types as LYT
import Servant (Header, Headers, addHeader)
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.CoinsConfig as CQCoinsConfig
import Storage.ConfigPilot.Config.CoinsConfig (CoinsConfigDimensions (..))
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.Coins.CoinsConfig as SQCC
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.Vehicle as QVeh
import Tools.Error

data CoinConfigWithTimeBounds = CoinConfigWithTimeBounds
  { coinsConfig :: CoinsConfig.CoinsConfig,
    timeBounds :: TB.TimeBound
  }

data SelectedIncentiveConfig = SelectedIncentiveConfig
  { selected :: CoinsConfig.CoinsConfig,
    ridesThreshold :: Int
  }

getCoinsIncentiveConfig ::
  ( Maybe (Id SP.Person),
    Id DM.Merchant,
    Id DMOC.MerchantOperatingCity
  ) ->
  Maybe Text ->
  Flow (Headers '[Header "ETag" Text] [API.DriverIncentiveCoinConfigItem])
getCoinsIncentiveConfig (mbPersonId, merchantId, merchantOpCityId) mbIfNoneMatch = do
  (transporterConfig, _driverId, vehCategory, driver) <- loadDriverContext mbPersonId merchantId merchantOpCityId
  let mocId = merchantOpCityId.getId
      eventFunctionKey =
        case parseFirstIncentiveRidesCompletedThreshold driver.driverTag of
          Just (eventFunction, _) -> T.pack (show eventFunction)
          Nothing -> "None"
  mbCachedETag <- CQCoinsConfig.getDriverIncentiveConfigHash mocId vehCategory eventFunctionKey
  case (mbIfNoneMatch, mbCachedETag) of
    (Just clientETag, Just cachedETag)
      | clientETag == cachedETag ->
        throwError DriverIncentiveCoinConfigNotModified
    _ -> do
      items <- buildConfigItems transporterConfig merchantId merchantOpCityId vehCategory driver.driverTag
      let eTag = computeDriverIncentiveConfigETag items
      CQCoinsConfig.setDriverIncentiveConfigHash mocId vehCategory eventFunctionKey eTag
      pure $ addHeader eTag items

getCoinsIncentiveRideCount ::
  ( Maybe (Id SP.Person),
    Id DM.Merchant,
    Id DMOC.MerchantOperatingCity
  ) ->
  Flow API.DriverIncentiveRideCountRes
getCoinsIncentiveRideCount (mbPersonId, merchantId, merchantOpCityId) = do
  (transporterConfig, driverId, vehCategory, driver) <- loadDriverContext mbPersonId merchantId merchantOpCityId
  mbSelected <- selectIncentiveConfig transporterConfig merchantId merchantOpCityId vehCategory driver.driverTag
  case mbSelected of
    Nothing ->
      pure $
        API.DriverIncentiveRideCountRes
          { dayValidRideCount = 0,
            timeBoundValidRideCount = Nothing,
            progressValidRideCount = 0
          }
    Just SelectedIncentiveConfig {selected} -> do
      localTime <- getLocalCurrentTime transporterConfig.timeDiffFromUtc
      dayValidRideCount <- fromMaybe 0 <$> Coins.getValidRideCountByDriverIdKey driverId
      let mbWindowKey =
            case selected.timeBounds of
              Just tb | tb /= TB.Unbounded ->
                case IncentiveMetrics.mkIncentiveWindowKey localTime tb of
                  windowKey@(IncentiveMetrics.TimeBoundWindow _) -> Just windowKey
                  IncentiveMetrics.DayWindow -> Nothing
              _ -> Nothing
      timeBoundValidRideCount <-
        case mbWindowKey of
          Just windowKey -> Just . fromMaybe 0 <$> Coins.getValidRideCountByDriverIdWindowKey driverId windowKey
          Nothing -> pure Nothing
      pure $
        API.DriverIncentiveRideCountRes
          { dayValidRideCount = dayValidRideCount,
            timeBoundValidRideCount = timeBoundValidRideCount,
            progressValidRideCount = fromMaybe dayValidRideCount timeBoundValidRideCount
          }

loadDriverContext ::
  Maybe (Id SP.Person) ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Flow (DTC.TransporterConfig, Id SP.Person, DTV.VehicleCategory, SP.Person)
loadDriverContext mbPersonId merchantId merchantOpCityId = do
  driverId <- mbPersonId & fromMaybeM (PersonNotFound "No person id passed")
  transporterConfig <-
    getOneConfig
      (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId})
      (Just (SCTC.findByMerchantOpCityId merchantOpCityId Nothing))
      >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  unless transporterConfig.coinFeature $
    throwError $ CoinServiceUnavailable merchantId.getId
  driver <- B.runInReplica $ Person.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  vehCategory <-
    QVeh.findById driverId
      >>= fromMaybeM (DriverWithoutVehicle driverId.getId)
      <&> (\vehicle -> VecVariant.castVehicleVariantToVehicleCategory vehicle.variant)
  pure (transporterConfig, driverId, vehCategory, driver)

buildConfigItems ::
  DTC.TransporterConfig ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  DTV.VehicleCategory ->
  Maybe [LYT.TagNameValueExpiry] ->
  Flow [API.DriverIncentiveCoinConfigItem]
buildConfigItems transporterConfig merchantId merchantOpCityId vehCategory driverTag = do
  mbSelected <- selectIncentiveConfig transporterConfig merchantId merchantOpCityId vehCategory driverTag
  pure $ maybe [] (\SelectedIncentiveConfig {..} -> [toConfigItem selected ridesThreshold]) mbSelected

selectIncentiveConfig ::
  DTC.TransporterConfig ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  DTV.VehicleCategory ->
  Maybe [LYT.TagNameValueExpiry] ->
  Flow (Maybe SelectedIncentiveConfig)
selectIncentiveConfig transporterConfig merchantId merchantOpCityId vehCategory driverTag = do
  case parseFirstIncentiveRidesCompletedThreshold driverTag of
    Nothing -> pure Nothing
    Just (eventFunction, ridesThreshold) -> do
      activeConfigs <-
        getConfig
          ( CoinsConfigDimensions
              { merchantOptCityId = merchantOpCityId.getId,
                merchantId = Just merchantId.getId,
                active = Just True,
                vehicleCategory = Just vehCategory,
                eventFunction = Nothing,
                serviceTierType = Nothing,
                eventName = Nothing,
                tripCategoryType = Nothing,
                configId = Nothing
              }
          )
          (Just (SQCC.getActiveCoinConfigs merchantId merchantOpCityId vehCategory))
      localTime <- getLocalCurrentTime transporterConfig.timeDiffFromUtc
      let matchingConfigs =
            filter
              ( \cc ->
                  cc.active
                    && cc.eventName == "EndRide"
                    && cc.eventFunction == eventFunction
              )
              activeConfigs
          timeBoundCandidates =
            [ CoinConfigWithTimeBounds cc tb
              | cc <- matchingConfigs,
                Just tb <- [cc.timeBounds],
                tb /= TB.Unbounded
            ]
          matchedTimeBound = TB.findBoundedDomain timeBoundCandidates localTime
          selectedConfigs =
            case matchedTimeBound of
              matched@(_ : _) -> (.coinsConfig) <$> matched
              [] ->
                case timeBoundCandidates of
                  candidate : _ -> [candidate.coinsConfig]
                  [] -> filter (\cc -> fromMaybe TB.Unbounded cc.timeBounds == TB.Unbounded) matchingConfigs
      pure $ SelectedIncentiveConfig <$> listToMaybe selectedConfigs <*> pure ridesThreshold

toConfigItem :: CoinsConfig.CoinsConfig -> Int -> API.DriverIncentiveCoinConfigItem
toConfigItem selected ridesThreshold =
  API.DriverIncentiveCoinConfigItem
    { id = selected.id,
      eventFunction = selected.eventFunction,
      eventName = selected.eventName,
      coins = selected.coins,
      expirationAt = selected.expirationAt,
      active = selected.active,
      vehicleCategory = selected.vehicleCategory,
      tripCategoryType = selected.tripCategoryType,
      serviceTierType = selected.serviceTierType,
      timeBounds = selected.timeBounds,
      ridesThreshold = ridesThreshold
    }

computeDriverIncentiveConfigETag :: [API.DriverIncentiveCoinConfigItem] -> Text
computeDriverIncentiveConfigETag items =
  T.cons '"' (T.pack (show (Hash.hashWith Hash.SHA256 (BS.toStrict (encode items))))) `T.snoc` '"'

-- | First segment after Incentive# only (before any "&").
-- Returns threshold only when that segment is DriverIncentiveCohortRidesCompleted N.
parseFirstIncentiveRidesCompletedThreshold :: Maybe [LYT.TagNameValueExpiry] -> Maybe (DCT.DriverCoinsFunctionType, Int)
parseFirstIncentiveRidesCompletedThreshold =
  (=<<) firstRidesCompleted . listToMaybe . concatMap parseFirstIncentiveSegment . fromMaybe []
  where
    parseFirstIncentiveSegment (LYT.TagNameValueExpiry rawTagText) =
      case T.splitOn "#" rawTagText of
        ("Incentive" : tagValueText : _) ->
          case map T.strip (T.splitOn "&" tagValueText) of
            (firstSeg : _) | not (T.null firstSeg) -> maybeToList (readMaybe (T.unpack firstSeg))
            _ -> []
        _ -> []
    firstRidesCompleted = \case
      DCT.DriverIncentiveCohortRidesCompleted n -> Just (DCT.DriverIncentiveCohortRidesCompleted n, n)
      _ -> Nothing
