module Domain.Action.UI.DriverIncentiveCoins
  ( getCoinsIncentiveConfig,
    getCoinsIncentiveRideCount,
  )
where

import qualified API.Types.UI.DriverIncentiveCoins as API
import qualified Crypto.Hash as Hash
import Data.Aeson (encode)
import qualified Data.ByteString as BS
import Data.List (nub)
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
  mbCachedETag <- CQCoinsConfig.getDriverIncentiveConfigHash mocId vehCategory
  case (mbIfNoneMatch, mbCachedETag) of
    (Just clientETag, Just cachedETag)
      | clientETag == cachedETag ->
        throwError DriverIncentiveCoinConfigNotModified
    _ -> do
      items <- buildConfigItems transporterConfig merchantId merchantOpCityId vehCategory driver.driverTag
      let eTag = computeDriverIncentiveConfigETag items
      CQCoinsConfig.setDriverIncentiveConfigHash mocId vehCategory eTag
      pure $ addHeader eTag items

getCoinsIncentiveRideCount ::
  ( Maybe (Id SP.Person),
    Id DM.Merchant,
    Id DMOC.MerchantOperatingCity
  ) ->
  Flow API.DriverIncentiveRideCountRes
getCoinsIncentiveRideCount (mbPersonId, merchantId, merchantOpCityId) = do
  (transporterConfig, driverId, vehCategory, driver) <- loadDriverContext mbPersonId merchantId merchantOpCityId
  -- Same selection as incentiveConfig (tagged cohort OR RidesCompleted fallback).
  selectedConfigs <- selectIncentiveConfigs transporterConfig merchantId merchantOpCityId vehCategory driver.driverTag
  -- Day key exists for any driver who completed a valid ride; missing Redis → 0.
  dayValidRideCount <- fromMaybe 0 <$> Coins.getValidRideCountByDriverIdKey driverId
  localTime <- getLocalCurrentTime transporterConfig.timeDiffFromUtc
  timeBoundValidRideCount <-
    case listToMaybe selectedConfigs of
      Nothing -> pure Nothing
      Just SelectedIncentiveConfig {selected} ->
        case selected.timeBounds of
          Just tb | tb /= TB.Unbounded ->
            case IncentiveMetrics.mkIncentiveWindowKey localTime tb of
              windowKey@(IncentiveMetrics.TimeBoundWindow _) ->
                Just . fromMaybe 0 <$> Coins.getValidRideCountByDriverIdWindowKey driverId windowKey
              IncentiveMetrics.DayWindow -> pure Nothing
          _ -> pure Nothing
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
  selectedConfigs <- selectIncentiveConfigs transporterConfig merchantId merchantOpCityId vehCategory driverTag
  pure $ map (\SelectedIncentiveConfig {..} -> toConfigItem selected ridesThreshold) selectedConfigs

selectIncentiveConfigs ::
  DTC.TransporterConfig ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  DTV.VehicleCategory ->
  Maybe [LYT.TagNameValueExpiry] ->
  Flow [SelectedIncentiveConfig]
selectIncentiveConfigs transporterConfig merchantId merchantOpCityId vehCategory driverTag = do
  activeConfigs <- fetchActiveCoinConfigs merchantId merchantOpCityId vehCategory
  localTime <- getLocalCurrentTime transporterConfig.timeDiffFromUtc
  let taggedEventFunctions = parseAllIncentiveRidesCompletedThresholds driverTag
  if not (null taggedEventFunctions)
    then
      pure $
        mapMaybe
          ( \(eventFunction, ridesThreshold) ->
              SelectedIncentiveConfig
                <$> preferConfigForEventFunction localTime activeConfigs eventFunction
                <*> pure ridesThreshold
          )
          taggedEventFunctions
    else
      let ridesCompletedConfigs =
            filter
              ( \cc ->
                  cc.active
                    && cc.eventName == "EndRide"
                    && isRidesCompletedFunction cc.eventFunction
              )
              activeConfigs
          eventFunctions = nub $ map (.eventFunction) ridesCompletedConfigs
       in pure $
            mapMaybe
              ( \eventFunction -> do
                  selected <- preferConfigForEventFunction localTime activeConfigs eventFunction
                  ridesThreshold <- ridesThresholdFromEventFunction eventFunction
                  pure $ SelectedIncentiveConfig selected ridesThreshold
              )
              eventFunctions

fetchActiveCoinConfigs ::
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  DTV.VehicleCategory ->
  Flow [CoinsConfig.CoinsConfig]
fetchActiveCoinConfigs merchantId merchantOpCityId vehCategory =
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

-- | Prefer an in-window timebound row; else any bounded row for that eventFunction;
-- else unbounded / null. Nothing if no matching EndRide config.
preferConfigForEventFunction ::
  UTCTime ->
  [CoinsConfig.CoinsConfig] ->
  DCT.DriverCoinsFunctionType ->
  Maybe CoinsConfig.CoinsConfig
preferConfigForEventFunction localTime activeConfigs eventFunction =
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
   in listToMaybe selectedConfigs

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

isRidesCompletedFunction :: DCT.DriverCoinsFunctionType -> Bool
isRidesCompletedFunction = \case
  DCT.RidesCompleted _ -> True
  _ -> False

ridesThresholdFromEventFunction :: DCT.DriverCoinsFunctionType -> Maybe Int
ridesThresholdFromEventFunction = \case
  DCT.DriverIncentiveCohortRidesCompleted n -> Just n
  DCT.RidesCompleted n -> Just n
  _ -> Nothing

-- | All DriverIncentiveCohortRidesCompleted segments after Incentive# (split on "&").
parseAllIncentiveRidesCompletedThresholds :: Maybe [LYT.TagNameValueExpiry] -> [(DCT.DriverCoinsFunctionType, Int)]
parseAllIncentiveRidesCompletedThresholds =
  mapMaybe asRidesCompleted . concatMap parseIncentiveSegments . fromMaybe []
  where
    parseIncentiveSegments (LYT.TagNameValueExpiry rawTagText) =
      case T.splitOn "#" rawTagText of
        ("Incentive" : tagValueText : _) ->
          mapMaybe (readMaybe . T.unpack) $
            filter (not . T.null) $
              map T.strip (T.splitOn "&" tagValueText)
        _ -> []
    asRidesCompleted = \case
      DCT.DriverIncentiveCohortRidesCompleted n -> Just (DCT.DriverIncentiveCohortRidesCompleted n, n)
      _ -> Nothing
