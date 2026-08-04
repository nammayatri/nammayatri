module SharedLogic.FRFSPassOverride
  ( OverrideBenefitConfig (..),
    OverrideBenefit (..),
    PercentageSaving (..),
    FixedSaving (..),
    ApplicablePass (..),
    PassOption (..),
    mkPassOptionAPIEntity,
    passOptionsForQuote,
    maxTripCountFromPass,
    registerHasPass,
    clearHasPass,
    checkHasPass,
    getFRFSOverrideApplicablePassesByPersonId,
    applyOverrideBenefit,
    parseOverrideBenefitConfig,
    ConsumeResult (..),
    getRemainingTripCount,
    consumeTrip,
    refundTrip,
  )
where

import qualified API.Types.UI.FRFSTicketService as FRFSTicketServiceAPI
import qualified BecknV2.FRFS.Enums as Spec
import qualified Data.Aeson as A
import qualified Data.Time as T
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.Pass as DPass
import qualified Domain.Types.Person as DP
import qualified Domain.Types.PurchasedPass as DPurchasedPass
import qualified Domain.Types.PurchasedPassPayment as DPPP
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.JSON (constructorsWithSnakeCase)
import qualified Storage.CachedQueries.Pass as CQPass
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.PersonExtra as QPersonExtra
import qualified Storage.Queries.PurchasedPassPayment as QPurchasedPassPayment

newtype OverrideBenefitConfig = OverrideBenefitConfig
  { overrideBenefits :: [OverrideBenefit]
  }
  deriving (Generic, Show)

data OverrideBenefit = OverrideBenefit
  { percentageSaving :: Maybe PercentageSaving,
    fixedSaving :: Maybe FixedSaving,
    maximumTripCount :: Maybe Int,
    maxTicketQuantityPerOverride :: Maybe Int
  }
  deriving (Generic, Show)

data PercentageSaving = PercentageSaving
  { applicableValue :: HighPrecMoney,
    enabled :: Maybe Bool
  }
  deriving (Generic, Show)

data FixedSaving = FixedSaving
  { applicableValue :: HighPrecMoney,
    currencyType :: Maybe Currency,
    enabled :: Maybe Bool
  }
  deriving (Generic, Show)

instance FromJSON OverrideBenefitConfig where
  parseJSON = A.genericParseJSON constructorsWithSnakeCase

instance ToJSON OverrideBenefitConfig where
  toJSON = A.genericToJSON constructorsWithSnakeCase

instance FromJSON OverrideBenefit where
  parseJSON = A.genericParseJSON constructorsWithSnakeCase

instance ToJSON OverrideBenefit where
  toJSON = A.genericToJSON constructorsWithSnakeCase

instance FromJSON PercentageSaving where
  parseJSON = A.genericParseJSON constructorsWithSnakeCase

instance ToJSON PercentageSaving where
  toJSON = A.genericToJSON constructorsWithSnakeCase

instance FromJSON FixedSaving where
  parseJSON = A.genericParseJSON constructorsWithSnakeCase

instance ToJSON FixedSaving where
  toJSON = A.genericToJSON constructorsWithSnakeCase

data ApplicablePass = ApplicablePass
  { purchasedPassPayment :: DPPP.PurchasedPassPayment,
    pass :: DPass.Pass,
    benefit :: OverrideBenefit,
    availableTripCount :: Maybe Int
  }

parseOverrideBenefitConfig :: A.Value -> Maybe OverrideBenefitConfig
parseOverrideBenefitConfig value = case A.fromJSON value of
  A.Success config -> Just config
  A.Error _ -> Nothing

getFRFSOverrideApplicablePassesByPersonId ::
  (CacheFlow m r, EsqDBFlow m r) =>
  DIBC.IntegratedBPPConfig ->
  DP.Person ->
  Spec.VehicleCategory ->
  UTCTime ->
  m [ApplicablePass]
getFRFSOverrideApplicablePassesByPersonId integratedBPPConfig person vehicleType tripTime = do
  let tripDay = T.utctDay tripTime
  if integratedBPPConfig.passOverrideApplicable /= Just True
    then pure []
    else do
      hasPass <- checkHasPass person tripDay
      if not hasPass
        then pure []
        else do
          payments <-
            QPurchasedPassPayment.findAllByPersonIdAndStatuses
              Nothing
              Nothing
              person.id
              [DPurchasedPass.Active, DPurchasedPass.PreBooked]
          catMaybes <$> mapM (toApplicablePass vehicleType tripDay) payments

toApplicablePass ::
  (CacheFlow m r, EsqDBFlow m r) =>
  Spec.VehicleCategory ->
  T.Day ->
  DPPP.PurchasedPassPayment ->
  m (Maybe ApplicablePass)
toApplicablePass vehicleType tripDay payment
  | not withinValidity = pure Nothing
  | otherwise = case payment.passId of
    Nothing -> pure Nothing
    Just passId ->
      CQPass.findById passId >>= \case
        Nothing -> pure Nothing
        Just pass
          | not (isOverridePass pass) -> pure Nothing
          | otherwise -> case mbBenefit pass of
            Nothing -> pure Nothing
            Just benefit -> do
              availableTripCount <- getRemainingTripCount payment
              if maybe False (<= 0) availableTripCount
                then pure Nothing
                else pure . Just $ ApplicablePass {purchasedPassPayment = payment, pass = pass, benefit = benefit, availableTripCount = availableTripCount}
  where
    withinValidity = payment.startDate <= tripDay && payment.endDate >= tripDay
    isOverridePass pass =
      pass.vehicleType == vehicleType
        && pass.frfsPriceOverrideApplicable == Just True
    mbBenefit pass = do
      configJson <- pass.overrideBenefitConfigJson
      config <- parseOverrideBenefitConfig configJson
      listToMaybe config.overrideBenefits

maxTripCountFromPass :: DPass.Pass -> Maybe Int
maxTripCountFromPass pass = do
  configJson <- pass.overrideBenefitConfigJson
  config <- parseOverrideBenefitConfig configJson
  benefit <- listToMaybe config.overrideBenefits
  benefit.maximumTripCount

data PassOption = PassOption
  { purchasedPassPaymentId :: Id DPPP.PurchasedPassPayment,
    passId :: Id DPass.Pass,
    passName :: Maybe Text,
    overriddenPrice :: Price,
    availableTripCount :: Maybe Int
  }
  deriving (Show, Generic)

passOptionsForQuote :: [ApplicablePass] -> Maybe Spec.ServiceTierType -> Int -> Price -> [PassOption]
passOptionsForQuote applicablePasses mbServiceTier quantity basePrice =
  [ PassOption
      { purchasedPassPaymentId = applicablePass.purchasedPassPayment.id,
        passId = applicablePass.pass.id,
        passName = applicablePass.purchasedPassPayment.passName,
        overriddenPrice = overridden,
        availableTripCount = applicablePass.availableTripCount
      }
    | applicablePass <- applicablePasses,
      coversTier applicablePass,
      coversQuantity applicablePass,
      let overridden = applyOverrideBenefit applicablePass.benefit basePrice,
      overridden.amount < basePrice.amount
  ]
  where
    coversTier applicablePass =
      maybe False (`elem` applicablePass.pass.applicableVehicleServiceTiers) mbServiceTier
    coversQuantity applicablePass =
      quantity <= fromMaybe 1 applicablePass.benefit.maxTicketQuantityPerOverride

applyOverrideBenefit :: OverrideBenefit -> Price -> Price
applyOverrideBenefit benefit basePrice
  | Just percentage <- benefit.percentageSaving,
    isEnabled percentage.enabled,
    percentage.applicableValue > 0 =
    discounted $ basePrice.amount * percentage.applicableValue / 100
  | Just fixed <- benefit.fixedSaving,
    isEnabled fixed.enabled,
    fixed.applicableValue > 0 =
    discounted fixed.applicableValue
  | otherwise = basePrice
  where
    isEnabled = fromMaybe False
    discounted saving = mkPrice (Just basePrice.currency) (max 0 (basePrice.amount - saving))

data ConsumeResult
  = Consumed Int
  | Unmetered
  | Exhausted
  deriving (Show, Eq)

makeTripCountKey :: Id DPPP.PurchasedPassPayment -> Text
makeTripCountKey paymentId = "availableTripCount:" <> paymentId.getId

tripCountTtl :: DPPP.PurchasedPassPayment -> UTCTime -> Int
tripCountTtl payment now =
  let secondsTillEnd = max 0 . round $ diffUTCTime (T.UTCTime (T.addDays 1 payment.endDate) 0) now
   in secondsTillEnd + (7 * 24 * 60 * 60)

getRemainingTripCount :: (MonadFlow m, Redis.HedisFlow m r) => DPPP.PurchasedPassPayment -> m (Maybe Int)
getRemainingTripCount payment = case payment.availableTripCount of
  Nothing -> pure Nothing
  Just allowance -> do
    let key = makeTripCountKey payment.id
    Redis.safeGet key >>= \case
      Just remaining -> pure (Just remaining)
      Nothing -> do
        now <- getCurrentTime
        Redis.setExp key allowance (tripCountTtl payment now)
        logInfo $ "FRFSPassOverride: seeded tripCount paymentId=" <> payment.id.getId <> " allowance=" <> show allowance
        pure (Just allowance)

consumeTrip :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => DPPP.PurchasedPassPayment -> m ConsumeResult
consumeTrip payment = case payment.availableTripCount of
  Nothing -> pure Unmetered
  Just _ -> do
    void $ getRemainingTripCount payment
    let key = makeTripCountKey payment.id
    remaining <- Redis.decr key
    if remaining < 0
      then do
        void $ Redis.incr key
        logWarning $ "FRFSPassOverride:consumeTrip EXHAUSTED paymentId=" <> payment.id.getId
        pure Exhausted
      else do
        QPurchasedPassPayment.updateAvailableTripCountById (Just (fromIntegral remaining)) payment.id
        pure $ Consumed (fromIntegral remaining)

refundTrip :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => DPPP.PurchasedPassPayment -> Int -> m ()
refundTrip payment grantedAllowance = when (isJust payment.availableTripCount) $ do
  let key = makeTripCountKey payment.id
  current <- fromMaybe grantedAllowance <$> Redis.safeGet key
  when (current < grantedAllowance) $ do
    remaining <- Redis.incr key
    QPurchasedPassPayment.updateAvailableTripCountById (Just (fromIntegral remaining)) payment.id

makeHasPassKey :: Id DP.Person -> Text
makeHasPassKey personId = "FRFSPassOverride:HasPass-" <> personId.getId

hasPassTtlBuffer :: Int
hasPassTtlBuffer = 7 * 24 * 60 * 60

registerHasPass :: (CacheFlow m r, EsqDBFlow m r) => Id DP.Person -> T.Day -> m ()
registerHasPass personId endDate = do
  now <- getCurrentTime
  let key = makeHasPassKey personId
      secondsTillEnd = max 0 . round $ diffUTCTime (T.UTCTime (T.addDays 1 endDate) 0) now
      newTtl = secondsTillEnd + hasPassTtlBuffer
  existingTtl <- fromInteger <$> Redis.ttl key
  Redis.setExp key True (max newTtl existingTtl)
  mbPerson <- QPerson.findById personId
  let currentTill = mbPerson >>= (.hasPassTill)
  when (maybe True (< endDate) currentTill) $ QPersonExtra.updateHasPassTill personId endDate

clearHasPass :: (CacheFlow m r) => Id DP.Person -> m ()
clearHasPass personId = Redis.del (makeHasPassKey personId)

checkHasPass :: (CacheFlow m r) => DP.Person -> T.Day -> m Bool
checkHasPass person today =
  Redis.safeGet (makeHasPassKey person.id) >>= \case
    Just hasLivePass -> pure hasLivePass
    Nothing -> pure $ maybe False (>= today) person.hasPassTill

mkPassOptionAPIEntity :: PassOption -> FRFSTicketServiceAPI.FRFSPassOptionAPIEntity
mkPassOptionAPIEntity passOption =
  FRFSTicketServiceAPI.FRFSPassOptionAPIEntity
    { purchasedPassPaymentId = passOption.purchasedPassPaymentId,
      passId = passOption.passId,
      passName = passOption.passName,
      overriddenPrice = mkPriceAPIEntity passOption.overriddenPrice,
      availableTripCount = passOption.availableTripCount
    }
