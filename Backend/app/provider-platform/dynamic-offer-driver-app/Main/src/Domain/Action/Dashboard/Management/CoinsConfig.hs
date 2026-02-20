{-# LANGUAGE OverloadedLabels #-}

module Domain.Action.Dashboard.Management.CoinsConfig
  ( putCoinsConfigUpdate,
    postCoinsConfigCreate,
  )
where

import qualified API.Types.ProviderPlatform.Management.CoinsConfig as Common
import Control.Lens ((.~))
import Data.Generics.Labels ()
import qualified Data.Text as Text
import qualified Domain.Types.Coins.CoinsConfig as DTCC
import qualified Domain.Types.Merchant
import Domain.Types.Translations (Translations (..))
import qualified Environment
import EulerHS.Prelude hiding (id, (.~))
import Kernel.Types.APISuccess (APISuccess (Success))
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Error (GenericError (InvalidRequest))
import qualified Kernel.Types.Id as ID
import qualified Kernel.Utils.Common as UC
import qualified Storage.CachedQueries.CoinsConfig as CQConfig
import qualified Storage.Queries.Coins.CoinsConfig as QConfig
import Storage.Queries.Translations (create)
import Storage.Queries.TranslationsExtra (isTranslationExist)

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
      let duplicatedConfig = coinsConfig & #id .~ ID.Id uuid & #eventFunction .~ eventFunction
      pure (duplicatedConfig, eventMessages)
  QConfig.createCoinEntries newCoinsConfig
  clearCache newCoinsConfig
  processingTranslations newCoinsConfig eventMessageLs
  pure Success

clearCache :: DTCC.CoinsConfig -> Environment.Flow ()
clearCache coinsConfig = do
  let clearCacheWithoutVehicleCategory = CQConfig.clearCache coinsConfig.eventName coinsConfig.eventFunction (ID.Id coinsConfig.merchantOptCityId)
  whenJust coinsConfig.vehicleCategory clearCacheWithoutVehicleCategory

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
