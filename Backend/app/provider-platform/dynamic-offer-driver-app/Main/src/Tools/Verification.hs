{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.Verification
  ( module Reexport,
    verifyDLAsync,
    verifyBankAccountAsync,
    verifyGstAsync,
    verifyRC,
    validateImage,
    extractRCImage,
    extractDLImage,
    extractPanImage,
    extractGSTImage,
    extractAadhaarImage,
    validateFaceImage,
    verifySdkResp,
    getTask,
    nameCompare,
    getDigiLockerFile,
    pullDigiLockerDrivingLicense,
    fetchAndExtractVerifiedDL,
    fetchAndExtractVerifiedPan,
    fetchAndExtractVerifiedAadhaar,
    getVerifiedAadhaarXML,
  )
where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantServiceConfig as DMSC
import Domain.Types.MerchantServiceUsageConfig
import qualified Kernel.External.Tokenize.Interface as TI
import qualified Kernel.External.Tokenize.Interface.Types as TIFT
import qualified Kernel.External.Tokenize.Types as TT
import Kernel.External.Types (ServiceFlow)
import Kernel.External.Verification as Reexport hiding
  ( extractAadhaarImage,
    extractDLImage,
    extractGSTImage,
    extractPanImage,
    extractRCImage,
    fetchAndExtractVerifiedAadhaar,
    fetchAndExtractVerifiedDL,
    fetchAndExtractVerifiedPan,
    getTask,
    getVerifiedAadhaarXML,
    nameCompare,
    searchAgent,
    validateFaceImage,
    validateImage,
    verifyBankAccountAsync,
    verifyDLAsync,
    verifyGstAsync,
    verifyRC,
    verifySdkResp,
  )
import qualified Kernel.External.Verification as Verification
import qualified Kernel.External.Verification.Digilocker.Types as DigiTypes
import Kernel.External.Verification.Interface.InternalScripts
import qualified Kernel.External.Verification.Types as VT
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Beam.GovtDataRC ()
import qualified Storage.Cac.MerchantServiceUsageConfig as CQMSUC
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC
import Tools.Error
import Tools.Metrics (CoreMetrics)

verifyDLAsync ::
  ServiceFlow m r =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  VerifyDLAsyncReq ->
  m VerifyDLAsyncResp
verifyDLAsync _ merchantOpCityId req = do
  merchantServiceUsageConfig <-
    CQMSUC.findByMerchantOpCityId merchantOpCityId Nothing
      >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOpCityId.getId)
  fromMaybeM (InternalError $ "Providers not configured in the priority list !!!!!" <> show merchantServiceUsageConfig.verificationProvidersPriorityList) (listToMaybe merchantServiceUsageConfig.verificationProvidersPriorityList) >>= \provider -> callService merchantOpCityId provider Verification.verifyDLAsync req -- TODO: Using first element of priority list as of now would be soon replacing this with a proper fallback implementation.

verifyBankAccountAsync ::
  ServiceFlow m r =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  VerifyBankAccountAsyncReq ->
  m VerifyBankAccountAsyncResp
verifyBankAccountAsync _ merchantOpCityId req = do
  merchantServiceUsageConfig <-
    CQMSUC.findByMerchantOpCityId merchantOpCityId Nothing
      >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOpCityId.getId)
  fromMaybeM (InternalError $ "Providers not configured in the priority list !!!!!" <> show merchantServiceUsageConfig.verificationProvidersPriorityList) (listToMaybe merchantServiceUsageConfig.verificationProvidersPriorityList) >>= \provider -> callService merchantOpCityId provider Verification.verifyBankAccountAsync req -- TODO: Using first element of priority list as of now would be soon replacing this with a proper fallback implementation.

verifyGstAsync ::
  ServiceFlow m r =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  VerifyGstAsyncReq ->
  m VerifyGstAsyncResp
verifyGstAsync _ merchantOpCityId req = do
  merchantServiceUsageConfig <-
    CQMSUC.findByMerchantOpCityId merchantOpCityId Nothing
      >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOpCityId.getId)
  fromMaybeM (InternalError $ "Providers not configured in the priority list !!!!!" <> show merchantServiceUsageConfig.verificationProvidersPriorityList) (listToMaybe merchantServiceUsageConfig.verificationProvidersPriorityList) >>= \provider -> callService merchantOpCityId provider Verification.verifyGstAsync req

verifyRC ::
  ( ServiceFlow m r,
    CoreMetrics m,
    HasField "ttenTokenCacheExpiry" r Seconds
  ) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Maybe [VerificationService] ->
  VerifyRCReq ->
  m RCRespWithRemPriorityList
verifyRC _ merchantOptCityId mbRemPriorityList req = do
  merchantServiceUsageConfig <-
    CQMSUC.findByMerchantOpCityId merchantOptCityId Nothing
      >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOptCityId.getId)

  let configuredTotoList = fromMaybe [VT.Idfy] merchantServiceUsageConfig.totoVerificationPriorityList
  let isTtenVerification =
        if isJust req.udinNo
          then do
            case listToMaybe configuredTotoList of
              Just VT.Tten -> True
              _ -> False
          else False

  mbToken' <-
    if isTtenVerification
      then do
        when (isNothing req.udinNo) $
          throwError $ InvalidRequest "UDIN number is required for TTEN verification"

        token <- getOrCreateTtenToken merchantOptCityId
        return (Just token)
      else return Nothing
  let reqWithToken = req {token = mbToken'}
  let finalPriorityList =
        fromMaybe
          ( if isTtenVerification
              then fromMaybe [VT.Tten] merchantServiceUsageConfig.totoVerificationPriorityList
              else merchantServiceUsageConfig.verificationProvidersPriorityList
          )
          mbRemPriorityList
  Verification.verifyRC (getServiceConfig merchantOptCityId) finalPriorityList reqWithToken
  where
    getOrCreateTtenToken :: (ServiceFlow m r, CoreMetrics m, HasField "ttenTokenCacheExpiry" r Seconds) => Id DMOC.MerchantOperatingCity -> m Text
    getOrCreateTtenToken merchantOpCityId = do
      let tokenCacheKey = makeTtenTokenCacheKey merchantOpCityId
      mbToken <- Redis.withCrossAppRedis $ Redis.safeGet tokenCacheKey
      case mbToken of
        Just cachedToken -> do
          return cachedToken
        Nothing -> do
          tokenServiceConfigJSON <- getTtenTokenizationServiceConfig merchantOpCityId
          tokenResp <-
            withTryCatch "tokenizeTtenCertificate" $
              TI.tokenize
                tokenServiceConfigJSON
                (TIFT.TokenizationReq {expiry = Nothing, code = Nothing, codeVerifier = Nothing})
          case tokenResp of
            Left err -> throwError $ InvalidRequest $ show err
            Right tokenResp' -> do
              now <- getCurrentTime
              ttenTokenCacheExpiry <- asks (.ttenTokenCacheExpiry)
              let expiryTime = fromMaybe (addUTCTime (secondsToNominalDiffTime ttenTokenCacheExpiry) now) tokenResp'.expiresAt
                  expirySeconds = round $ diffUTCTime expiryTime now
              Redis.withCrossAppRedis $ Redis.setExp tokenCacheKey tokenResp'.token expirySeconds
              return tokenResp'.token

    getTtenTokenizationServiceConfig :: (ServiceFlow m r, CoreMetrics m) => Id DMOC.MerchantOperatingCity -> m TIFT.TokenizationServiceConfig
    getTtenTokenizationServiceConfig mocid = do
      merchantServiceConfig <-
        CQMSC.findByServiceAndCity
          (DMSC.TokenizationService TT.Tten)
          mocid
          >>= fromMaybeM
            ( MerchantServiceConfigNotFound mocid.getId "tokenization" $
                show (DMSC.TokenizationService TT.Tten)
            )
      case merchantServiceConfig.serviceConfig of
        DMSC.TokenizationServiceConfig vsc -> return vsc
        _ -> throwError $ InternalError "Unknown Service Config"

    makeTtenTokenCacheKey :: Id DMOC.MerchantOperatingCity -> Text
    makeTtenTokenCacheKey opCityId = "TtenToken:MerchantOpCityId-" <> opCityId.getId

getServiceConfig :: ServiceFlow m r => Id DMOC.MerchantOperatingCity -> VerificationService -> m VerificationServiceConfig
getServiceConfig merchantOptCityId cfg = case cfg of
  GovtData -> return $ GovtDataConfig {}
  _ -> do
    merchantServiceConfig <- CQMSC.findByServiceAndCity (DMSC.VerificationService cfg) merchantOptCityId >>= fromMaybeM (MerchantServiceConfigNotFound merchantOptCityId.getId "verification" $ show cfg)
    case merchantServiceConfig.serviceConfig of
      DMSC.VerificationServiceConfig vsc -> return vsc
      _ -> throwError $ InternalError "Unknown Service Config"

validateImage ::
  ServiceFlow m r =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  ValidateImageReq ->
  m ValidateImageResp
validateImage = runWithServiceConfig Verification.validateImage (.verificationService)

validateFaceImage ::
  ServiceFlow m r =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  FaceValidationReq ->
  m FaceValidationRes
validateFaceImage = runWithServiceConfig Verification.validateFaceImage (.faceVerificationService)

extractRCImage ::
  ServiceFlow m r =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  ExtractRCImageReq ->
  m ExtractRCImageResp
extractRCImage = runWithServiceConfig Verification.extractRCImage (.verificationService)

extractPanImage ::
  ServiceFlow m r =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  ExtractPanImage ->
  m ExtractedPanImageResp
extractPanImage = runWithServiceConfig Verification.extractPanImage (.verificationService)

extractGSTImage ::
  ServiceFlow m r =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  ExtractGSTImage ->
  m ExtractedGSTImageResp
extractGSTImage = runWithServiceConfig Verification.extractGSTImage (.verificationService)

extractAadhaarImage ::
  ServiceFlow m r =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  ExtractAadhaarImageReq ->
  m ExtractAadhaarImageRes
extractAadhaarImage = runWithServiceConfig Verification.extractAadhaarImage (.verificationService)

nameCompare ::
  ServiceFlow m r =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  NameCompareReq ->
  m NameCompareResp
nameCompare = runWithServiceConfig Verification.nameCompare (.verificationService)

extractDLImage ::
  ServiceFlow m r =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  ExtractDLImageReq ->
  m ExtractDLImageResp
extractDLImage = runWithServiceConfig Verification.extractDLImage (.verificationService)

verifySdkResp ::
  ServiceFlow m r =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  VerifySdkDataReq ->
  m VerifySdkDataResp
verifySdkResp = runWithServiceConfig Verification.verifySdkResp (.sdkVerificationService)

getTask ::
  ServiceFlow m r =>
  Id DMOC.MerchantOperatingCity ->
  VerificationService ->
  GetTaskReq ->
  (Text -> Maybe Text -> Text -> m ()) ->
  m GetTaskResp
getTask merchantOpCityId config req updateResp = do
  merchantServiceConfig <-
    CQMSC.findByServiceAndCity (DMSC.VerificationService config) merchantOpCityId
      >>= fromMaybeM (InternalError $ "No verification service provider configured for the merchant, merchantOpCityId:" <> merchantOpCityId.getId <> " Service : " <> T.pack (show config))
  case merchantServiceConfig.serviceConfig of
    DMSC.VerificationServiceConfig vsc -> Verification.getTask vsc req updateResp
    _ -> throwError $ InternalError "Unknown Service Config"

runWithServiceConfig ::
  ServiceFlow m r =>
  (VerificationServiceConfig -> req -> m resp) ->
  (MerchantServiceUsageConfig -> VerificationService) ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  req ->
  m resp
runWithServiceConfig func getCfg _merchantId merchantOpCityId req = do
  merchantServiceUsageConfig <-
    CQMSUC.findByMerchantOpCityId merchantOpCityId Nothing
      >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOpCityId.getId)
  callService merchantOpCityId (getCfg merchantServiceUsageConfig) func req

callService :: ServiceFlow m r => Id DMOC.MerchantOperatingCity -> VerificationService -> (VerificationServiceConfig -> req -> m resp) -> req -> m resp
callService merchantOpCityId vsc func req = do
  merchantServiceConfig <-
    CQMSC.findByServiceAndCity (DMSC.VerificationService vsc) merchantOpCityId
      >>= fromMaybeM (InternalError $ "No verification service provider configured for the merchant, merchantOpCityId:" <> merchantOpCityId.getId <> " Service : " <> T.pack (show vsc))
  case merchantServiceConfig.serviceConfig of
    DMSC.VerificationServiceConfig vsc' -> func vsc' req
    _ -> throwError $ InternalError "Unknown Service Config"

-- DigiLocker specific functions

-- | Get file (PDF) from DigiLocker for S3 storage
getDigiLockerFile ::
  ServiceFlow m r =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  DigiTypes.DigiLockerGetFileReq ->
  m BSL.ByteString
getDigiLockerFile _merchantId merchantOpCityId req = do
  logInfo $
    "Tools.Verification.getDigiLockerFile - Request: merchantOpCityId="
      <> merchantOpCityId.getId
      <> ", uri="
      <> req.uri
  resp <- callService merchantOpCityId DigiLocker Verification.getFile req
  logInfo $
    "Tools.Verification.getDigiLockerFile - Response bytes: "
      <> show (BSL.length resp)
  pure resp

-- | Pull Driving License from DigiLocker (requires DL number)
pullDigiLockerDrivingLicense ::
  ServiceFlow m r =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  DigiTypes.DigiLockerPullDrivingLicenseReq ->
  m DigiTypes.DigiLockerPullDocumentResponse
pullDigiLockerDrivingLicense _merchantId merchantOpCityId req =
  callService merchantOpCityId DigiLocker Verification.pullDrivingLicense req

-- | Fetch and extract verified Driving License from DigiLocker
-- Combines XML fetching and parsing in one call
fetchAndExtractVerifiedDL ::
  ServiceFlow m r =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  DigiTypes.DigiLockerExtractDLReq ->
  m ExtractedDigiLockerDLResp
fetchAndExtractVerifiedDL _merchantId merchantOpCityId req =
  callService merchantOpCityId DigiLocker Verification.fetchAndExtractVerifiedDL req

-- | Fetch and extract verified PAN card from DigiLocker
-- Combines XML fetching and parsing in one call
fetchAndExtractVerifiedPan ::
  ServiceFlow m r =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  DigiTypes.DigiLockerExtractPanReq ->
  m ExtractedDigiLockerPanResp
fetchAndExtractVerifiedPan _merchantId merchantOpCityId req =
  callService merchantOpCityId DigiLocker Verification.fetchAndExtractVerifiedPan req

-- | Fetch and extract verified Aadhaar from DigiLocker
-- Combines XML fetching and parsing in one call
-- Note: Aadhaar uses different endpoint, doesn't require URI
fetchAndExtractVerifiedAadhaar ::
  ServiceFlow m r =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  DigiTypes.DigiLockerExtractAadhaarReq ->
  m ExtractedDigiLockerAadhaarResp
fetchAndExtractVerifiedAadhaar _merchantId merchantOpCityId req =
  callService merchantOpCityId DigiLocker Verification.fetchAndExtractVerifiedAadhaar req

-- | Get verified Aadhaar XML from DigiLocker (raw XML for S3 storage)
-- Returns raw XML text that can be stored in Image table
-- Note: Aadhaar doesn't support getFile API, so we get XML directly
getVerifiedAadhaarXML ::
  ServiceFlow m r =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  DigiTypes.DigiLockerExtractAadhaarReq ->
  m Text
getVerifiedAadhaarXML _merchantId merchantOpCityId req =
  callService merchantOpCityId DigiLocker Verification.getVerifiedAadhaarXML req
