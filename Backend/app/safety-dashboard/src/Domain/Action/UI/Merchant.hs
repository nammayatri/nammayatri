{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Domain.Action.UI.Merchant where

import API.Types.UI.Merchant
import qualified "dashboard-helper-api" Dashboard.SafetyPlatform as Safety
import qualified Data.Text as T (unpack)
import Domain.Types.MerchantConfigs
import "lib-dashboard" Domain.Types.Person.API as AP
import qualified Domain.Types.Transaction as DT
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (id)
import Kernel.External.Encryption (decrypt, getDbHash)
import qualified Kernel.Types.APISuccess
import Kernel.Utils.Common
import Network.URI (isURI)
import qualified SharedLogic.Transaction as T
import Storage.Beam.CommonInstances ()
import "lib-dashboard" Storage.Queries.Merchant as QMerchant
import "lib-dashboard" Storage.Queries.MerchantAccess as QMCA
import Storage.Queries.MerchantConfigs as QMC
import qualified "lib-dashboard" Storage.Queries.Person as QP
import qualified "lib-dashboard" Storage.CachedQueries.Role as CQRole
import qualified "lib-dashboard" Tools.Auth as Auth
import Tools.Error
import "lib-dashboard" Tools.Error

buildTransaction ::
  ( MonadFlow m
  ) =>
  Safety.SafetyEndpoint ->
  Auth.TokenInfo ->
  Text ->
  m DT.Transaction
buildTransaction endpoint tokenInfo = T.buildTransactionForSafetyDashboard (DT.SafetyAPI endpoint) (Just tokenInfo)

postSetMerchantConfig :: Auth.TokenInfo -> API.Types.UI.Merchant.SetMerchantConfigReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postSetMerchantConfig tokenInfo req = do
  transaction <- buildTransaction Safety.SetMerchantConfigEndpoint tokenInfo (encodeToText req)
  T.withTransactionStoring transaction $ do
    merchant <- QMerchant.findById tokenInfo.merchantId >>= fromMaybeM (MerchantNotFound tokenInfo.merchantId.getId)
    merchantConfigs <- QMC.findByMerchantId $ Just merchant.id
    unless (isURI $ T.unpack req.webhookUrl) $ throwError InvalidWebhookUrl
    case merchantConfigs of
      Nothing -> do
        merchantConfig <- buildMerchantConfig tokenInfo merchant.shortId.getShortId req
        QMC.create merchantConfig
        return Kernel.Types.APISuccess.Success
      Just config -> do
        merchantConfig <- buildUpdateMerchantConfig req config
        QMC.updateByPrimaryKey merchantConfig
        return Kernel.Types.APISuccess.Success

getMerchantUserList :: Auth.TokenInfo -> Environment.Flow API.Types.UI.Merchant.MerchantUserList
getMerchantUserList tokenInfo = do
  merchant <- QMerchant.findById tokenInfo.merchantId >>= fromMaybeM (MerchantNotFound tokenInfo.merchantId.getId)
  allMerchantUsers <- QMCA.findAllUserAccountForMerchant tokenInfo.merchantId
  let personIds = map (.personId) allMerchantUsers
  allPersons <- QP.findAllByIds personIds
  decryptedPersons <- mapM decrypt allPersons
  personEntityList <-
    mapM
      ( \decPerson -> do
          role <- CQRole.findById decPerson.roleId >>= fromMaybeM (RoleNotFound decPerson.roleId.getId)
          pure $ AP.makePersonAPIEntity decPerson role [merchant.shortId] Nothing
      )
      decryptedPersons
  return $ MerchantUserList {merchantUserList = personEntityList}

postMerchantUserResetPassword :: Auth.TokenInfo -> API.Types.UI.Merchant.ResetPasswordReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postMerchantUserResetPassword tokenInfo req = do
  transaction <- buildTransaction Safety.ResetPasswordEndpoint tokenInfo (encodeToText req)
  T.withTransactionStoring transaction $ do
    person <- QP.findByEmail req.email >>= fromMaybeM (PersonNotFound req.email)
    void $ QMCA.findByPersonIdAndMerchantIdAndCity person.id tokenInfo.merchantId tokenInfo.city >>= fromMaybeM AccessDenied
    newHash <- getDbHash req.newPassword
    QP.updatePersonPassword person.id newHash
    return Kernel.Types.APISuccess.Success

getMerchantGetWebhookConfig :: Auth.TokenInfo -> Environment.Flow Domain.Types.MerchantConfigs.MerchantConfigs
getMerchantGetWebhookConfig tokenInfo = do
  merchant <- QMerchant.findById tokenInfo.merchantId >>= fromMaybeM (MerchantNotFound tokenInfo.merchantId.getId)
  merchantConfig <- QMC.findByMerchantId $ Just merchant.id
  case merchantConfig of
    Just config -> return config
    Nothing -> throwError MerchantConfigNotFound

postMerchantWebhookConfigPreference :: Auth.TokenInfo -> API.Types.UI.Merchant.WebHookConfigPreferenceReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postMerchantWebhookConfigPreference tokenInfo req = do
  transaction <- buildTransaction Safety.SetMerchantConfigEndpoint tokenInfo (encodeToText req)
  T.withTransactionStoring transaction $ do
    merchant <- QMerchant.findById tokenInfo.merchantId >>= fromMaybeM (MerchantNotFound tokenInfo.merchantId.getId)
    merchantConfig <- QMC.findByMerchantId $ Just merchant.id
    case merchantConfig of
      Just config -> do
        QMC.updateRequestWebHookById req.preference config.id
        return Kernel.Types.APISuccess.Success
      Nothing -> throwError MerchantConfigNotFound

buildMerchantConfig :: Auth.TokenInfo -> Text -> API.Types.UI.Merchant.SetMerchantConfigReq -> Environment.Flow Domain.Types.MerchantConfigs.MerchantConfigs
buildMerchantConfig tokenInfo merchantShortId req = do
  now <- getCurrentTime
  id_ <- generateGUID
  return
    Domain.Types.MerchantConfigs.MerchantConfigs
      { id = id_,
        merchantShortId = merchantShortId,
        webHookHeaders = req.webHookHeaders,
        webHookUrl = req.webhookUrl,
        merchantId = Just tokenInfo.merchantId,
        requestWebHook = True,
        createdAt = now,
        updatedAt = now
      }

buildUpdateMerchantConfig :: API.Types.UI.Merchant.SetMerchantConfigReq -> Domain.Types.MerchantConfigs.MerchantConfigs -> Environment.Flow Domain.Types.MerchantConfigs.MerchantConfigs
buildUpdateMerchantConfig req config = do
  return
    Domain.Types.MerchantConfigs.MerchantConfigs
      { id = config.id,
        merchantShortId = config.merchantShortId,
        webHookHeaders = req.webHookHeaders,
        webHookUrl = req.webhookUrl,
        merchantId = config.merchantId,
        requestWebHook = config.requestWebHook,
        createdAt = config.createdAt,
        updatedAt = config.updatedAt
      }
