module Domain.Action.Dashboard.Conductor.Registration
  ( postConductorRegister,
    postConductorBulkRegister,
  )
where

import qualified API.Types.ProviderPlatform.Conductor.Registration as Common
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.Merchant as DM
import Environment
import Kernel.External.Encryption (getDbHash, unDbHash)
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.GtfsDataServer.Flow as NandiFlow
import qualified Lib.GtfsDataServer.Types as NandiTypes
import SharedLogic.IntegratedBPPConfig (findIntegratedBPPConfig, getGimsBaseUrl)
import Storage.CachedQueries.Merchant as QMerchant
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Text.Hex as Hex
import Tools.Error

postConductorRegister ::
  ShortId DM.Merchant ->
  Context.City ->
  Common.ConductorRegisterReq ->
  Flow Common.ConductorRegisterResp
postConductorRegister merchantShortId opCity req = do
  (baseUrl, gtfsId) <- getGimsConfig merchantShortId opCity
  registerConductor baseUrl gtfsId req

postConductorBulkRegister ::
  ShortId DM.Merchant ->
  Context.City ->
  Common.ConductorBulkRegisterReq ->
  Flow Common.ConductorBulkRegisterResp
postConductorBulkRegister merchantShortId opCity req = do
  (baseUrl, gtfsId) <- getGimsConfig merchantShortId opCity
  results <- mapM (registerConductorBestEffort baseUrl gtfsId) req.conductors
  return Common.ConductorBulkRegisterResp {results}

-- Helpers

getGimsConfig :: ShortId DM.Merchant -> Context.City -> Flow (BaseUrl, Text)
getGimsConfig merchantShortId opCity = do
  merchant <- QMerchant.findByShortId merchantShortId >>= fromMaybeM (MerchantNotFound merchantShortId.getShortId)
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  integratedBPPConfig <- findIntegratedBPPConfig Nothing merchantOpCity.id "BUS" DIBC.APPLICATION
  baseUrl <- getGimsBaseUrl integratedBPPConfig
  return (baseUrl, integratedBPPConfig.feedKey)

registerConductor :: BaseUrl -> Text -> Common.ConductorRegisterReq -> Flow Common.ConductorRegisterResp
registerConductor baseUrl gtfsId req = do
  (emailHashHex, passwordHashHex) <- hashCredentials req.email req.password
  gimsResp <-
    NandiFlow.gimsEmployeeRegister
      baseUrl
      gtfsId
      NandiTypes.GimsEmployeeRegisterReq
        { token_no = req.operatorBadgeToken,
          email_hash = emailHashHex,
          password_hash = passwordHashHex,
          first_name = req.firstName
        }
  return Common.ConductorRegisterResp {success = gimsResp.success, operatorBadgeToken = req.operatorBadgeToken}

registerConductorBestEffort :: BaseUrl -> Text -> Common.ConductorRegisterReq -> Flow Common.ConductorBulkRegisterResult
registerConductorBestEffort baseUrl gtfsId req = do
  outcome <- try @_ @SomeException $ do
    (emailHashHex, passwordHashHex) <- hashCredentials req.email req.password
    NandiFlow.gimsEmployeeRegisterE
      baseUrl
      gtfsId
      NandiTypes.GimsEmployeeRegisterReq
        { token_no = req.operatorBadgeToken,
          email_hash = emailHashHex,
          password_hash = passwordHashHex,
          first_name = req.firstName
        }
  case outcome of
    Left ex -> do
      logError $ "Conductor registration failed for " <> req.operatorBadgeToken <> ": " <> show ex
      return Common.ConductorBulkRegisterResult {operatorBadgeToken = req.operatorBadgeToken, success = False, errorMessage = Just (show ex)}
    Right (Right resp) -> return Common.ConductorBulkRegisterResult {operatorBadgeToken = req.operatorBadgeToken, success = resp.success, errorMessage = Nothing}
    Right (Left errMsg) -> return Common.ConductorBulkRegisterResult {operatorBadgeToken = req.operatorBadgeToken, success = False, errorMessage = Just errMsg}

hashCredentials :: Text -> Text -> Flow (Text, Text)
hashCredentials email password = do
  emailDbHash <- getDbHash email
  passwordDbHash <- getDbHash password
  let emailHashHex = Hex.encodeHex (unDbHash emailDbHash)
      passwordHashHex = Hex.encodeHex (unDbHash passwordDbHash)
  return (emailHashHex, passwordHashHex)
