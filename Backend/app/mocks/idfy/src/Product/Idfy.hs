 {-
 Copyright 2022-23, Juspay India Pvt Ltd
 
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License 
 
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program 
 
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of 
 
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Product.Idfy (verifyDL, verifyRC, validateImage, extractDLImage, extractRCImage) where

import App.Types
import Common
import EulerHS.Prelude
import Idfy.Types.Request
import Idfy.Types.Response
import Kernel.Types.Error
import Kernel.Types.Forkable
import Kernel.Types.GuidLike
import Kernel.Utils.Error.Throwing
import Kernel.Utils.Time
import Tools.FlowHandling
import Types.Common
import Types.Webhook

verifyDL :: Maybe ApiKey -> Maybe AccountId -> DLVerificationRequest -> FlowHandler IdfySuccess
verifyDL apiKey accountId req = withFlowHandlerAPI $ do
  verifyAuth apiKey accountId
  reqId <- generateGUID
  now <- getCurrentTime
  fork "driver license verificaton callback" $ do
    waitMilliSec <- asks (.callbackWaitTimeMilliSec)
    threadDelayMilliSec waitMilliSec
    void $ buildSuccessDL req reqId now >>= sendDLVerification
  pure $ IdfySuccess {request_id = reqId, _a = Nothing}

verifyRC :: Maybe ApiKey -> Maybe AccountId -> RCVerificationRequest -> FlowHandler IdfySuccess
verifyRC apiKey accountId req = withFlowHandlerAPI $ do
  verifyAuth apiKey accountId
  reqId <- generateGUID
  now <- getCurrentTime
  fork "registration certificate verificaton callback" $ do
    waitMilliSec <- asks (.callbackWaitTimeMilliSec)
    threadDelayMilliSec waitMilliSec
    void $ buildSuccessRC req reqId now >>= sendRCVerification
  pure $ IdfySuccess {request_id = reqId, _a = Nothing}

verifyAuth :: Maybe ApiKey -> Maybe AccountId -> Flow ()
verifyAuth apiKey accountId = do
  clientId <- asks (.accountId)
  clientSecret <- asks (.apiKey)
  unless (Just clientId == accountId && Just clientSecret == apiKey) $ throwError (InvalidRequest "Invalid authentication credentials")

validateImage :: Maybe ApiKey -> Maybe AccountId -> ImageValidateRequest -> FlowHandler ImageValidateResponse
validateImage apiKey accountId req = withFlowHandlerAPI $ do
  verifyAuth apiKey accountId
  let resp =
        ValidateResponse
          { detected_doc_type = req._data.doc_type,
            is_readable = Just True,
            readability = ReadabilityBody {confidence = Just 70, dummyField = Nothing} -- currently we check that it was > 60
          }
  buildMeaninglessIdfyResponse $ Just resp

extractDLImage :: Maybe ApiKey -> Maybe AccountId -> ImageExtractRequest -> FlowHandler DLExtractResponse
extractDLImage apiKey accountId _req = withFlowHandlerAPI $ do
  verifyAuth apiKey accountId
  buildMeaninglessIdfyResponse $
    Just $
      ExtractionOutput $
        DLExtractionOutput
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

extractRCImage :: Maybe ApiKey -> Maybe AccountId -> ImageExtractRequest -> FlowHandler RCExtractResponse
extractRCImage apiKey accountId _req = withFlowHandlerAPI $ do
  verifyAuth apiKey accountId
  buildMeaninglessIdfyResponse $
    Just $
      ExtractionOutput $
        RCExtractionOutput
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
