{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.Verification.Idfy.WebhookHandler where

import Data.Aeson.Types as DAT
import EulerHS.Prelude
import Kernel.Tools.Metrics.CoreMetrics.Types
import Kernel.Utils.Common
import Kernel.Utils.IOLogging
import Lib.Encryption
import Lib.Verification.Idfy.Auth
import Lib.Verification.Idfy.Config
import Lib.Verification.Idfy.Types.Response

webhookHandler ::
  ( EncFlow m r,
    HasField "isShuttingDown" r (TMVar ()),
    HasField "coreMetrics" r CoreMetricsContainer,
    HasField "loggerEnv" r LoggerEnv
  ) =>
  IdfyCfg ->
  (VerificationResponse -> Text -> m AckResponse) ->
  Maybe Text ->
  Value ->
  m AckResponse
webhookHandler cfg verifyHandler secret val = do
  withLogTag "webhookIdfy" $ do
    let respDump = encodeToText val
    let mResp = fromJSON val
    case mResp of
      DAT.Success (resp :: VerificationResponse) -> do
        void $ verifyAuth cfg secret
        void $ verifyHandler resp respDump
        pure Ack
      DAT.Error err1 -> do
        logInfo $ "Error 1: " <> show err1
        let mRespList = fromJSON val
        case mRespList of
          DAT.Success (respList :: VerificationResponseList) -> do
            let mResp_ = listToMaybe respList
            case mResp_ of
              Just resp_ -> do
                void $ verifyAuth cfg secret
                void $ verifyHandler resp_ respDump
                pure Ack
              Nothing -> pure Ack
          DAT.Error err -> do
            logInfo $ "Error 2: " <> show err
            pure Ack
