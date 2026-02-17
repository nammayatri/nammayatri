{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module ExternalBPP.ExternalAPI.Subway.CRIS.UpdateRDSBalance where

import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Domain.Types.Extra.IntegratedBPPConfig (CRISConfig)
import EulerHS.Prelude hiding (concatMap, find, length, map, null, readMaybe, whenJust)
import qualified EulerHS.Types as ET
import ExternalBPP.ExternalAPI.Subway.CRIS.Auth (callCRISAPI)
import ExternalBPP.ExternalAPI.Subway.CRIS.Encryption (decryptResponseData, encryptPayload)
import ExternalBPP.ExternalAPI.Subway.CRIS.Error (CRISError (..))
import ExternalBPP.ExternalAPI.Subway.CRIS.Types
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.App
import Kernel.Utils.Common
import Servant.API

data CRISBalanceResponse = CRISBalanceResponse
  { currentBal :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data GetRDSBalanceResp = GetRDSBalanceResp
  { balance :: HighPrecMoney
  }
  deriving (Generic, Show, ToJSON, FromJSON)

type GetRDSBalanceAPI =
  "t" :> "uts.cris.in" :> "VCU" :> "1" :> "get_current_balance"
    :> Header "Authorization" Text
    :> Header "Content-Type" Text
    :> Header "appCode" Text
    :> ReqBody '[PlainText] Text
    :> Post '[JSON] EncryptedResponse

getRDSBalanceAPI :: Proxy GetRDSBalanceAPI
getRDSBalanceAPI = Proxy

getRDSBalance ::
  ( CoreMetrics m,
    MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r
  ) =>
  CRISConfig ->
  m GetRDSBalanceResp
getRDSBalance config = do
  let req = object ["tpAccountId" .= (config.tpAccountId :: Int)]
  encryptionKey <- decrypt config.encryptionKey
  decryptionKey <- decrypt config.decryptionKey
  let jsonStr = decodeUtf8 $ LBS.toStrict $ encode req
  payload <- encryptPayload jsonStr encryptionKey
  encryptedResponse <- callCRISAPI config getRDSBalanceAPI (eulerClientFn payload) "getRDSBalance"

  crisBalanceResponse :: CRISBalanceResponse <- case eitherDecode (encode encryptedResponse) of
    Left err -> throwError (CRISError $ "Failed to parse encrypted getRDSBalance Resp: " <> T.pack (show err))
    Right encResp -> do
      logInfo $ "getRDSBalance Resp Code: " <> responseCode encResp
      if encResp.responseCode == "0"
        then do
          case decryptResponseData (responseData encResp) decryptionKey of
            Left err -> throwError (CRISError $ "Failed to decrypt getRDSBalance Resp: " <> T.pack err)
            Right decryptedJson -> do
              logInfo $ "getRDSBalance Decrypted Resp: " <> decryptedJson
              case eitherDecode (LBS.fromStrict $ TE.encodeUtf8 decryptedJson) of
                Left err -> throwError (CRISError $ "Failed to decode getRDSBalance Resp: " <> T.pack (show err))
                Right balanceResp -> pure balanceResp
        else throwError (CRISError $ "Non-zero response code in getRDSBalance Resp: " <> encResp.responseCode <> " " <> encResp.responseData)

  let mbBalance = readMaybe @HighPrecMoney . T.unpack $ crisBalanceResponse.currentBal
  currentBalance <- mbBalance & fromMaybeM (CRISError $ "Failed to parse balance amount: " <> show crisBalanceResponse.currentBal)

  pure $ GetRDSBalanceResp {balance = currentBalance}
  where
    eulerClientFn req token =
      let client = ET.client getRDSBalanceAPI
       in client (Just $ "Bearer " <> token) (Just "application/json") (Just "CUMTA") req
