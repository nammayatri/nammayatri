module Domain.Action.UI.CRIS (postCrisGetSDKData, getCrisOtpGeneration, postCrisChangeDevice) where

import qualified API.Types.UI.CRIS
import qualified BecknV2.OnDemand.Enums
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (id)
import ExternalBPP.ExternalAPI.Subway.CRIS.ChangeDevice as ChangeDevice
import ExternalBPP.ExternalAPI.Subway.CRIS.OtpGeneration as OtpGeneration
import ExternalBPP.ExternalAPI.Subway.CRIS.SDKData as GetSDKData
import Kernel.External.Encryption
import qualified Kernel.Prelude
import Kernel.Randomizer
import qualified Kernel.Types.APISuccess
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (fromMaybeM, throwError, withTryCatch)
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import qualified Storage.Queries.Person as QP

postCrisGetSDKData ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Maybe (Kernel.Types.Id.Id DIBC.IntegratedBPPConfig) ->
    API.Types.UI.CRIS.GetSDKDataRequest ->
    Environment.Flow API.Types.UI.CRIS.GetSDKDataResponse
  )
postCrisGetSDKData (mbPersonId, _) mbIntegratedBPPConfigId request = do
  case mbPersonId of
    Just personId -> do
      person <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
      integratedBPPConfig <- SIBC.findIntegratedBPPConfig mbIntegratedBPPConfigId person.merchantOperatingCityId BecknV2.OnDemand.Enums.SUBWAY DIBC.MULTIMODAL
      case integratedBPPConfig.providerConfig of
        DIBC.CRIS config' -> do
          resp <- withTryCatch "getSDKData" $ GetSDKData.getSDKData config' request
          case resp of
            Left _ -> throwError $ InternalError "Error in get-sdk-data!"
            Right sdkData -> do
              case sdkData.respCode of
                0 -> return sdkData
                _ -> throwError $ InternalError "non-zero respCode from get-sdk-data!"
        _ -> throwError $ InternalError "Unimplemented!"
    Nothing -> throwError $ InternalError "Person not found"

getCrisOtpGeneration ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Maybe (Kernel.Types.Id.Id DIBC.IntegratedBPPConfig) ->
    Environment.Flow Kernel.Types.APISuccess.APISuccess
  )
getCrisOtpGeneration (mbPersonId, _) mbIntegratedBPPConfigId = do
  personId <- mbPersonId & fromMaybeM (InvalidRequest "Person Id not found")
  person <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  integratedBPPConfig <- SIBC.findIntegratedBPPConfig mbIntegratedBPPConfigId person.merchantOperatingCityId BecknV2.OnDemand.Enums.SUBWAY DIBC.MULTIMODAL
  case integratedBPPConfig.providerConfig of
    DIBC.CRIS config' -> do
      mobileNumber <- person.mobileNumber & fromMaybeM (InvalidRequest "mobile no. not found")
      imeiNumber <- person.imeiNumber & fromMaybeM (InvalidRequest "imeiNumber not found")
      decryptedMobileNumber <- decrypt mobileNumber
      decryptedImeiNumber <- decrypt imeiNumber
      osType <- person.clientDevice & fromMaybeM (InvalidRequest "clientDevice not found") <&> (.deviceType)
      let request =
            OtpGeneration.CRISOtpGenerationRequest
              { mob = decryptedMobileNumber,
                imei = decryptedImeiNumber,
                otpType = 3,
                osType = show osType
              }
      resp <- withTryCatch "generateOtp:getCrisOtpGeneration" $ OtpGeneration.generateOtp config' request
      case resp of
        Left _ -> throwError $ InternalError "Error in otp-generation!"
        Right sdkData -> do
          case sdkData.respCode of
            0 -> pure Kernel.Types.APISuccess.Success
            _ -> throwError $ InternalError "non-zero respCode from otp-generation!"
    _ -> throwError $ InternalError "Unimplemented!"

postCrisChangeDevice ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Maybe (Kernel.Types.Id.Id DIBC.IntegratedBPPConfig) ->
    API.Types.UI.CRIS.CrisChangeDeviceRequest ->
    Environment.Flow Kernel.Types.APISuccess.APISuccess
  )
postCrisChangeDevice (mbPersonId, _) mbIntegratedBPPConfigId req = do
  personId <- mbPersonId & fromMaybeM (InvalidRequest "Person Id not found")
  person <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  integratedBPPConfig <- SIBC.findIntegratedBPPConfig mbIntegratedBPPConfigId person.merchantOperatingCityId BecknV2.OnDemand.Enums.SUBWAY DIBC.MULTIMODAL
  case integratedBPPConfig.providerConfig of
    DIBC.CRIS config' -> do
      mobileNumber <- person.mobileNumber & fromMaybeM (InvalidRequest "mobile no. not found")
      imeiNumber <- person.imeiNumber & fromMaybeM (InvalidRequest "imeiNumber not found")
      decryptedMobileNumber <- decrypt mobileNumber
      decryptedImeiNumber <- decrypt imeiNumber
      osType <- person.clientDevice & fromMaybeM (InvalidRequest "clientDevice not found") <&> (.deviceType)
      sessionId <- getRandomInRange (1, 1000000 :: Int)
      let request =
            ChangeDevice.CRISChangeDeviceRequest
              { mob = decryptedMobileNumber,
                appCode = config'.appCode,
                imei = decryptedImeiNumber,
                osType = show osType,
                sessionId = sessionId,
                agentAccountId = config'.tpAccountId,
                otp = req.otp
              }
      resp <- withTryCatch "generateOtp:postCrisChangeDevice" $ ChangeDevice.changeDevice config' request
      case resp of
        Left _ -> throwError $ InternalError "Error in change-device!"
        Right sdkData -> do
          case sdkData.respCode of
            0 -> pure Kernel.Types.APISuccess.Success
            _ -> throwError $ InternalError "non-zero respCode from change-device!"
    _ -> throwError $ InternalError "Unimplemented!"
