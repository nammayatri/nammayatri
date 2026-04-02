{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Product.Idfy (verifyDL, verifyRC, verifyPan, verifyGst, verifyPanAadhaarLink, validateImage, extractDLImage, extractRCImage, extractPanImage, extractGSTImage, extractAadhaarImage, nameCompare, configureMock) where

import App.Types
import Common
import EulerHS.Prelude
import qualified Kernel.External.Verification.Interface.Idfy as Idfy
import Kernel.Types.Error
import Kernel.Types.Forkable
import Kernel.Types.GuidLike
import Kernel.Utils.Error.Throwing
import Kernel.Utils.Time
import Tools.FlowHandling
import Types.Common
import Types.Webhook

verifyDL :: Maybe ApiKey -> Maybe AccountId -> Idfy.DLVerificationRequest -> FlowHandler Idfy.IdfySuccess
verifyDL apiKey accountId req = withFlowHandlerAPI $ do
  verifyAuth apiKey accountId
  reqId <- generateGUID
  now <- getCurrentTime
  fork "driver license verificaton callback" $ do
    waitMilliSec <- asks (.callbackWaitTimeMilliSec)
    threadDelayMilliSec waitMilliSec
    void $ buildSuccessDL req reqId now >>= sendDLVerification
  pure $ Idfy.IdfySuccess {request_id = reqId, _a = Nothing}

verifyRC :: Maybe ApiKey -> Maybe AccountId -> Idfy.RCVerificationRequest -> FlowHandler Idfy.IdfySuccess
verifyRC apiKey accountId req = withFlowHandlerAPI $ do
  verifyAuth apiKey accountId
  reqId <- generateGUID
  now <- getCurrentTime
  fork "registration certificate verificaton callback" $ do
    waitMilliSec <- asks (.callbackWaitTimeMilliSec)
    threadDelayMilliSec waitMilliSec
    void $ buildSuccessRC req reqId now >>= sendRCVerification
  pure $ Idfy.IdfySuccess {request_id = reqId, _a = Nothing}

verifyPan :: Maybe ApiKey -> Maybe AccountId -> Idfy.PanVerificationRequest -> FlowHandler Idfy.IdfySuccess
verifyPan apiKey accountId req = withFlowHandlerAPI $ do
  verifyAuth apiKey accountId
  reqId <- generateGUID
  now <- getCurrentTime
  fork "pan verificaton callback" $ do
    waitMilliSec <- asks (.callbackWaitTimeMilliSec)
    threadDelayMilliSec waitMilliSec
    void $ buildSuccessPan req reqId now >>= sendPanVerification
  pure $ Idfy.IdfySuccess {request_id = reqId, _a = Nothing}

verifyGst :: Maybe ApiKey -> Maybe AccountId -> Idfy.GstVerificationRequest -> FlowHandler Idfy.IdfySuccess
verifyGst apiKey accountId req = withFlowHandlerAPI $ do
  verifyAuth apiKey accountId
  reqId <- generateGUID
  now <- getCurrentTime
  fork "gst verificaton callback" $ do
    waitMilliSec <- asks (.callbackWaitTimeMilliSec)
    threadDelayMilliSec waitMilliSec
    void $ buildSuccessGst req reqId now >>= sendGstVerification
  pure $ Idfy.IdfySuccess {request_id = reqId, _a = Nothing}

verifyPanAadhaarLink :: Maybe ApiKey -> Maybe AccountId -> Idfy.PanAadhaarLinkRequest -> FlowHandler Idfy.IdfySuccess
verifyPanAadhaarLink apiKey accountId req = withFlowHandlerAPI $ do
  verifyAuth apiKey accountId
  reqId <- generateGUID
  now <- getCurrentTime
  fork "pan aadhaar link verificaton callback" $ do
    waitMilliSec <- asks (.callbackWaitTimeMilliSec)
    threadDelayMilliSec waitMilliSec
    void $ buildSuccessPanAadhaarLink req reqId now >>= sendPanVerification
  pure $ Idfy.IdfySuccess {request_id = reqId, _a = Nothing}

verifyAuth :: Maybe ApiKey -> Maybe AccountId -> Flow ()
verifyAuth apiKey accountId = do
  clientId <- asks (.accountId)
  clientSecret <- asks (.apiKey)
  unless (Just clientId == accountId && Just clientSecret == apiKey) $ throwError (InvalidRequest "Invalid authentication credentials")

validateImage :: Maybe ApiKey -> Maybe AccountId -> Idfy.ImageValidateRequest -> FlowHandler Idfy.ImageValidateResponse
validateImage apiKey accountId req = withFlowHandlerAPI $ do
  verifyAuth apiKey accountId
  let resp =
        Idfy.ValidateResponse
          { detected_doc_type = req._data.doc_type,
            is_readable = Just True,
            readability = Idfy.ReadabilityBody {confidence = Just 70, dummyField = Nothing} -- currently we check that it was > 60
          }
  buildMeaninglessIdfyResponse $ Just resp

extractDLImage :: Maybe ApiKey -> Maybe AccountId -> Idfy.ImageExtractRequest -> FlowHandler Idfy.DLExtractResponse
extractDLImage apiKey accountId _req = withFlowHandlerAPI $ do
  verifyAuth apiKey accountId
  buildMeaninglessIdfyResponse $
    Just $
      Idfy.ExtractionOutput $
        Idfy.DLExtractionOutput
          { id_number = Just "id_number",
            name_on_card = Just "name_on_card",
            fathers_name = Just "fathers_name",
            date_of_birth = Just "date_of_birth",
            date_of_validity = Just "date_of_validity",
            address = Just "address",
            district = Just "district",
            street_address = Just "street_address",
            pincode = Just "pincode",
            state = Just "state",
            issue_dates = Nothing,
            _type = ["W_CAB"],
            validity = Nothing,
            status = Just "status"
          }

extractRCImage :: Maybe ApiKey -> Maybe AccountId -> Idfy.ImageExtractRequest -> FlowHandler Idfy.RCExtractResponse
extractRCImage apiKey accountId _req = withFlowHandlerAPI $ do
  verifyAuth apiKey accountId
  buildMeaninglessIdfyResponse $
    Just $
      Idfy.ExtractionOutput $
        Idfy.RCExtractionOutput
          { address = Just "address",
            body = Just "body",
            chassis_number = Just "chassis_number",
            _class = Just "_class",
            colour = Just "colour",
            cubic_capacity = Just "cubic_capacity",
            document1_side = Just "document1_side",
            document2_side = Just "document2_side",
            engine_number = Just "engine_number",
            fathers_name = Just "fathers_name",
            fuel = Just "fuel",
            manufacturer = Just "manufacturer",
            manufacturing_date = Just "manufacturing_date",
            model = Just "model",
            owner_name = Just "owner_name",
            registration_date = Just "registration_date",
            registration_number = Just "registration_number",
            rto_district = Just "rto_district",
            state = Just "state",
            wheel_base = Just "wheel_base",
            status = Just "status"
          }

extractPanImage :: Maybe ApiKey -> Maybe AccountId -> Idfy.ImageExtractRequest -> FlowHandler Idfy.PanExtractionResponse
extractPanImage apiKey accountId _req = withFlowHandlerAPI $ do
  verifyAuth apiKey accountId
  docConfig <- asks (.mockDocConfigRef) >>= liftIO . readIORef
  buildMeaninglessIdfyResponse $
    Just $
      Idfy.ExtractionOutput $
        Idfy.PanExtractionOutput
          { age = Nothing,
            date_of_birth = Just "1990-01-01",
            date_of_issue = Nothing,
            fathers_name = Just "FATHER NAME",
            id_number = Just docConfig.panNumber,
            is_scanned = Just True,
            minor = Just False,
            name_on_card = Just "TEST FLEET OWNER",
            pan_type = Just "Individual"
          }

extractGSTImage :: Maybe ApiKey -> Maybe AccountId -> Idfy.ImageExtractRequest -> FlowHandler Idfy.GSTExtractionResponse
extractGSTImage apiKey accountId _req = withFlowHandlerAPI $ do
  verifyAuth apiKey accountId
  docConfig <- asks (.mockDocConfigRef) >>= liftIO . readIORef
  buildMeaninglessIdfyResponse $
    Just $
      Idfy.ExtractionOutput $
        Idfy.GSTExtractionOutput
          { address = Just "123 Test Street",
            constitution_of_business = Just "Private Limited Company",
            date_of_liability = Just "2020-01-01",
            gstin = Just docConfig.gstNumber,
            is_provisional = Just False,
            legal_name = Just "TEST FLEET PVT LTD",
            pan_number = Just docConfig.panNumber,
            trade_name = Just "TEST FLEET",
            type_of_registration = Just "Regular",
            valid_from = Just "2020-01-01",
            valid_upto = Nothing
          }

extractAadhaarImage :: Maybe ApiKey -> Maybe AccountId -> Idfy.AadharVerificationReqest -> FlowHandler Idfy.AadhaarExtractionResponse
extractAadhaarImage apiKey accountId _req = withFlowHandlerAPI $ do
  verifyAuth apiKey accountId
  docConfig <- asks (.mockDocConfigRef) >>= liftIO . readIORef
  buildMeaninglessIdfyResponse $
    Just $
      Idfy.AadhaarResult
        { extraction_output =
            Idfy.AadhaarExtractionOutput
              { address = Just "123 Test Street, Delhi",
                date_of_birth = Just "1990-01-01",
                district = Just "Delhi",
                fathers_name = Just "FATHER NAME",
                gender = Just "MALE",
                house_number = Just "123",
                id_number = Just docConfig.aadhaarNumber,
                is_scanned = Just True,
                name_on_card = Just "TEST FLEET OWNER",
                pincode = Just "110001",
                state = Just "Delhi",
                street_address = Just "Test Street",
                year_of_birth = Just "1990"
              },
          qr_output =
            Idfy.AadhaarQROutput
              { address = Nothing,
                date_of_birth = Nothing,
                district = Nothing,
                gender = Nothing,
                house_number = Nothing,
                id_number = Nothing,
                name_on_card = Nothing,
                pincode = Nothing,
                state = Nothing,
                street_address = Nothing,
                year_of_birth = Nothing
              }
        }

nameCompare :: Maybe ApiKey -> Maybe AccountId -> Idfy.NameCompareRequest -> FlowHandler Idfy.NameCompareResponse
nameCompare apiKey accountId _req = withFlowHandlerAPI $ do
  verifyAuth apiKey accountId
  buildMeaninglessIdfyResponse $
    Just $
      Idfy.NameCompareResponseData
        { match_output = Idfy.NameMatchOutput {name_match = 100}
        }

configureMock :: MockDocConfig -> FlowHandler MockDocConfig
configureMock newConfig = withFlowHandlerAPI $ do
  ref <- asks (.mockDocConfigRef)
  liftIO $ writeIORef ref newConfig
  pure newConfig
