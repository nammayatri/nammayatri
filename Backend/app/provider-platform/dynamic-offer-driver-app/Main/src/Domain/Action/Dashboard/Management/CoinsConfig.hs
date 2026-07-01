module Domain.Action.Dashboard.Management.CoinsConfig
  ( putCoinsConfigUpdate,
    postCoinsConfigCreate,
    getCoinsConfigList,
  )
where

import qualified API.Types.ProviderPlatform.Management.CoinsConfig as Common
import qualified Data.Text as Text
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
import qualified Kernel.Utils.Common as UC
import qualified Lib.DriverCoins.Types as DCT
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.CachedQueries.CoinsConfig as CQConfig
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.Coins.CoinsConfig as QConfig
import qualified Storage.Queries.Coins.CoinsConfigExtra as QCoinsConfigExtra
import Storage.Queries.Translations (create)
import Storage.Queries.TranslationsExtra (isTranslationExist)

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
      tripCategoryType = coinsConfig.tripCategoryType
    }

putCoinsConfigUpdate ::
  ID.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Common.UpdateReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
putCoinsConfigUpdate _merchantShortId _opCity req = do
  let findCoinsConfig = QConfig.findById $ ID.cast req.entriesId
  coinsConfig <- findCoinsConfig >>= UC.fromMaybeM (InvalidRequest "Coins config does not exist")
  QConfig.updateCoinEntries req
  clearCache coinsConfig
  pure Success

postCoinsConfigCreate ::
  ID.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Common.CreateCoinsConfigReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
postCoinsConfigCreate _merchantShortId _opCity req = do
  uuid <- UC.generateGUIDText
  (newCoinsConfig, eventMessageLs) <- case req of
    Common.NewCoinsConfig (Common.NewCoinsConfigReq {..}) ->
      let newConfig = DTCC.CoinsConfig {id = ID.Id uuid, vehicleCategory = Just vehicleCategory, ..}
       in pure (newConfig, eventMessages)
    Common.DuplicateCoinsConfig (Common.DuplicateCoinsConfigsReq {..}) -> do
      let findCoinsConfig = QConfig.findById $ ID.cast entriesId
      coinsConfig <- findCoinsConfig >>= UC.fromMaybeM (InvalidRequest "Coins config does not exist")
      let duplicatedConfig = coinsConfig {DTCC.id = ID.Id uuid, DTCC.eventFunction = eventFunction}
      pure (duplicatedConfig, eventMessages)
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
