module Domain.Action.Dashboard.Management.CoinsConfig
  ( putCoinsConfigUpdate,
    postCoinsConfigCreate,
    getCoinsConfigList,
  )
where

import qualified API.Types.ProviderPlatform.Management.CoinsConfig as Common
import Data.List (intersect, nub)
import qualified Data.Text as Text
import Data.Time.Calendar.WeekDate (toWeekDate)
import qualified Domain.Types.Coins.CoinsConfig as DTCC
import qualified Domain.Types.Merchant
import Domain.Types.Translations (Translations (..))
import qualified Domain.Types.VehicleCategory as DTV
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions as B
import Kernel.Types.APISuccess (APISuccess (Success))
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Error (GenericError (InvalidRequest))
import qualified Kernel.Types.Id as ID
import qualified Kernel.Types.TimeBound as TB
import qualified Kernel.Utils.Common as UC
import qualified Lib.DriverCoins.Types as DCT
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.CachedQueries.CoinsConfig as CQConfig
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.Coins.CoinsConfig as QConfig
import qualified Storage.Queries.Coins.CoinsConfigExtra as QCoinsConfigExtra
import Storage.Queries.Translations (create)
import Storage.Queries.TranslationsExtra (isTranslationExist)

incentiveCohortWindowConflictMsg :: Text
incentiveCohortWindowConflictMsg = "Only 1 incentive cohort window is allowed per day currently."

getCoinsConfigList ::
  ID.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Maybe Int ->
  Maybe Int ->
  Maybe Text ->
  Maybe DTV.VehicleCategory ->
  Environment.Flow Common.CoinsConfigListRes
getCoinsConfigList merchantShortId opCity mbLimit mbOffset mbEventName mbVehicleCategory = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let limit = fromMaybe 10 mbLimit
      offset = fromMaybe 0 mbOffset
  configs <-
    B.runInReplica $
      QCoinsConfigExtra.findAllByMerchantOptCityIdWithLimitOffset merchantOpCityId mbEventName mbVehicleCategory (Just limit) (Just offset)
  pure $
    Common.CoinsConfigListRes
      { configs = map buildCoinsConfigListItem configs
      }

buildCoinsConfigListItem :: DTCC.CoinsConfig -> Common.CoinsConfigListItem
buildCoinsConfigListItem coinsConfig =
  Common.CoinsConfigListItem
    { entriesId = ID.cast coinsConfig.id,
      eventFunction = coinsConfig.eventFunction,
      eventName = coinsConfig.eventName,
      coins = coinsConfig.coins,
      expirationAt = coinsConfig.expirationAt,
      active = coinsConfig.active,
      vehicleCategory = coinsConfig.vehicleCategory,
      tripCategoryType = coinsConfig.tripCategoryType,
      timeBounds = fromMaybe TB.Unbounded coinsConfig.timeBounds
    }

putCoinsConfigUpdate ::
  ID.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Common.UpdateReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
putCoinsConfigUpdate _merchantShortId _opCity req = do
  let findCoinsConfig = QConfig.findById $ ID.cast req.entriesId
  coinsConfig <- findCoinsConfig >>= UC.fromMaybeM (InvalidRequest "Coins config does not exist")
  let updatedConfig =
        coinsConfig
          { DTCC.active = req.active,
            DTCC.expirationAt = req.expirationAt,
            DTCC.coins = req.coins,
            DTCC.timeBounds = req.timeBounds
          }
  validateIncentiveCohortTimeBoundWindow updatedConfig (Just coinsConfig.id)
  QConfig.updateCoinEntries req
  clearCache coinsConfig
  pure Success

postCoinsConfigCreate ::
  ID.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Common.CreateCoinsConfigReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postCoinsConfigCreate merchantShortId opCity req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  uuid <- UC.generateGUIDText
  (newCoinsConfig, eventMessageLs) <- case req of
    Common.NewCoinsConfig (Common.NewCoinsConfigReq {..}) ->
      let newConfig =
            DTCC.CoinsConfig
              { id = ID.Id uuid,
                merchantId = merchant.id.getId,
                merchantOptCityId = merchantOpCityId.getId,
                vehicleCategory = Just vehicleCategory,
                timeBounds = timeBounds,
                ..
              }
       in pure (newConfig, eventMessages)
    Common.DuplicateCoinsConfig (Common.DuplicateCoinsConfigsReq {..}) -> do
      let findCoinsConfig = QConfig.findById $ ID.cast entriesId
      coinsConfig <- findCoinsConfig >>= UC.fromMaybeM (InvalidRequest "Coins config does not exist")
      let duplicatedConfig = coinsConfig {DTCC.id = ID.Id uuid, DTCC.eventFunction = eventFunction}
      pure (duplicatedConfig, eventMessages)
  validateIncentiveCohortTimeBoundWindow newCoinsConfig Nothing
  QConfig.createCoinEntries newCoinsConfig
  clearCache newCoinsConfig
  processingTranslations newCoinsConfig eventMessageLs
  pure Success

clearCache :: DTCC.CoinsConfig -> Environment.Flow ()
clearCache coinsConfig = do
  let clearCacheForVehicleCategory = CQConfig.clearCache coinsConfig.eventName coinsConfig.eventFunction (ID.Id coinsConfig.merchantOptCityId)
  whenJust coinsConfig.vehicleCategory (\vc -> clearCacheForVehicleCategory vc coinsConfig.serviceTierType (fromMaybe DCT.DynamicOfferTrip coinsConfig.tripCategoryType))

processingTranslations :: DTCC.CoinsConfig -> [Common.EventMessage] -> Environment.Flow ()
processingTranslations coinsConfig eventMessageLs = do
  let messageKey = Text.strip $ coinsConfig.eventName <> "_" <> show coinsConfig.eventFunction
  mapM_
    ( \(Common.EventMessage msg lang) -> do
        translationExist <- isTranslationExist messageKey lang
        unless translationExist $ do
          uuid <- UC.generateGUIDText
          now <- UC.getCurrentTime
          create $
            Translations
              { createdAt = now,
                id = ID.Id uuid,
                language = lang,
                message = msg,
                messageKey = messageKey,
                updatedAt = now
              }
    )
    eventMessageLs

-- | Timebound uniqueness for incentive cohort configs (same city/vehicle/trip):
-- * DriverIncentiveCohortRidesCompleted N — conflict only with the exact same
--   eventFunction (same threshold N) sharing a weekday.
-- * DriverIncentiveCohortMetrics _ — conflict with any other Metrics config
--   sharing a weekday (thresholds ignored; only one Metrics window per day).
validateIncentiveCohortTimeBoundWindow ::
  DTCC.CoinsConfig ->
  Maybe (ID.Id DTCC.CoinsConfig) ->
  Environment.Flow ()
validateIncentiveCohortTimeBoundWindow candidate mbExcludeId = do
  case (candidate.active, nonUnboundedTimeBound candidate.timeBounds) of
    (True, Just candidateTb) ->
      case incentiveCohortConflictKind candidate.eventFunction of
        Nothing -> pure ()
        Just conflictKind -> do
          existing <-
            QCoinsConfigExtra.findActiveConfigsByCityVehicleTrip
              (ID.Id candidate.merchantOptCityId)
              candidate.vehicleCategory
              candidate.tripCategoryType
          let conflicting =
                filter
                  ( \cc ->
                      maybe True (cc.id /=) mbExcludeId
                        && matchesIncentiveCohortConflict conflictKind candidate.eventFunction cc.eventFunction
                        && maybe False (incentiveCohortDaysOverlap candidateTb) (nonUnboundedTimeBound cc.timeBounds)
                  )
                  existing
          unless (null conflicting) $
            UC.throwError (InvalidRequest incentiveCohortWindowConflictMsg)
    _ -> pure ()

data IncentiveCohortConflictKind
  = ExactEventFunction
  | AnyDriverIncentiveCohortMetrics

incentiveCohortConflictKind :: DCT.DriverCoinsFunctionType -> Maybe IncentiveCohortConflictKind
incentiveCohortConflictKind = \case
  DCT.DriverIncentiveCohortRidesCompleted _ -> Just ExactEventFunction
  DCT.DriverIncentiveCohortMetrics _ -> Just AnyDriverIncentiveCohortMetrics
  _ -> Nothing

matchesIncentiveCohortConflict ::
  IncentiveCohortConflictKind ->
  DCT.DriverCoinsFunctionType ->
  DCT.DriverCoinsFunctionType ->
  Bool
matchesIncentiveCohortConflict ExactEventFunction candidateEf otherEf = otherEf == candidateEf
matchesIncentiveCohortConflict AnyDriverIncentiveCohortMetrics _ otherEf =
  case otherEf of
    DCT.DriverIncentiveCohortMetrics _ -> True
    _ -> False

nonUnboundedTimeBound :: Maybe TB.TimeBound -> Maybe TB.TimeBound
nonUnboundedTimeBound = \case
  Just tb | tb /= TB.Unbounded -> Just tb
  _ -> Nothing

-- | Conflict if both timebounds occupy any of the same weekdays (Mon=1 .. Sun=7).
incentiveCohortDaysOverlap :: TB.TimeBound -> TB.TimeBound -> Bool
incentiveCohortDaysOverlap tb1 tb2 =
  not . null $ intersect (occupiedWeekDays tb1) (occupiedWeekDays tb2)

occupiedWeekDays :: TB.TimeBound -> [Int]
occupiedWeekDays = \case
  TB.Unbounded -> []
  TB.BoundedByWeekday peaks -> nub $ weekDaysWithPeaks peaks
  TB.BoundedByDay days ->
    nub
      [ dow
        | (day, peaks) <- days,
          not (null peaks),
          let (_, _, dow) = toWeekDate day
      ]

weekDaysWithPeaks :: TB.BoundedPeaks -> [Int]
weekDaysWithPeaks peaks =
  concat
    [ [1 | not (null peaks.monday)],
      [2 | not (null peaks.tuesday)],
      [3 | not (null peaks.wednesday)],
      [4 | not (null peaks.thursday)],
      [5 | not (null peaks.friday)],
      [6 | not (null peaks.saturday)],
      [7 | not (null peaks.sunday)]
    ]
