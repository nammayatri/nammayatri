module External.CCAvenue.SplitPayout
  ( SplitLeg (..),
    SplitPayoutRequest (..),
    SplitPayoutResult (..),
    createSplitPayout,
  )
where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import EulerHS.Prelude hiding (elem, id)
import qualified EulerHS.Types as ET
import External.CCAvenue.Encryption (decryptResponse, encryptRequest, parseFormFields)
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Error
import Kernel.Utils.Common
import Kernel.Utils.Monitoring.Prometheus.Servant ()
import qualified Network.HTTP.Media as M
import Servant hiding (throwError)

data SplitLeg = SplitLeg
  { splitAmount :: Text,
    subAccId :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data SplitPayoutRequest = SplitPayoutRequest
  { reference_no :: Text,
    split_tdr_charge_type :: Text,
    merComm :: Text,
    split_data_list :: [SplitLeg]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data SplitPayoutResult = SplitPayoutResult
  { success :: Bool,
    ccavenueStatus :: Maybe Text,
    ccavenueMessage :: Maybe Text,
    rawResponse :: Text,
    requestPayload :: A.Value,
    attemptedAt :: UTCTime
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data CreateSplitPayoutResult = CreateSplitPayoutResult
  { status :: Int,
    errorDesc :: Maybe Text,
    errorCode :: Maybe A.Value
  }
  deriving (Show, Eq, Generic)

instance FromJSON CreateSplitPayoutResult where
  parseJSON = A.withObject "CreateSplitPayoutResult" $ \o ->
    CreateSplitPayoutResult
      <$> o A..: "status"
      <*> o A..:? "error_desc"
      <*> o A..:? "error_code"

newtype CreateSplitPayoutResponse = CreateSplitPayoutResponse
  { result :: CreateSplitPayoutResult
  }
  deriving (Show, Eq, Generic)

instance FromJSON CreateSplitPayoutResponse where
  parseJSON = A.withObject "CreateSplitPayoutResponse" $ \o ->
    CreateSplitPayoutResponse <$> o A..: "Create_Split_Payout_Result"

data CCAvenueRaw

instance Accept CCAvenueRaw where
  contentTypes _ =
    NE.fromList
      [ "text" M.// "plain",
        "text" M.// "html",
        "application" M.// "json",
        "application" M.// "x-www-form-urlencoded",
        "*" M.// "*"
      ]

instance MimeUnrender CCAvenueRaw Text where
  mimeUnrender _ = Right . TE.decodeUtf8With TE.lenientDecode . BL.toStrict

type DoWebTransAPI =
  "apis" :> "servlet" :> "DoWebTrans"
    :> ReqBody '[FormUrlEncoded] [(Text, Text)]
    :> Post '[CCAvenueRaw] Text

doWebTransAPI :: Proxy DoWebTransAPI
doWebTransAPI = Proxy

createSplitPayout ::
  ( CoreMetrics m,
    MonadFlow m,
    MonadReader r m,
    HasRequestId r
  ) =>
  BaseUrl ->
  Text ->
  Text ->
  SplitPayoutRequest ->
  m SplitPayoutResult
createSplitPayout gatewayUrl accessCode workingKey req = do
  now <- getCurrentTime
  let payload = toJSON req
  encRequest <-
    either (throwError . InternalError . ("CCAvenue request encryption failed: " <>)) pure $
      encryptRequest workingKey (TE.decodeUtf8 . BL.toStrict $ A.encode req)
  let formBody =
        [ ("enc_request", encRequest),
          ("access_code", accessCode),
          ("command", "createSplitPayout"),
          ("request_type", "JSON"),
          ("version", "1.2")
        ]
  body <-
    callAPI gatewayUrl (ET.client doWebTransAPI formBody) "createSplitPayout" doWebTransAPI
      >>= fromEitherM (ExternalAPICallError (Just "CCAVENUE_CREATE_SPLIT_PAYOUT_API") gatewayUrl)
  let fields = parseFormFields body
      mkResult ok status message raw =
        SplitPayoutResult
          { success = ok,
            ccavenueStatus = status,
            ccavenueMessage = message,
            rawResponse = T.take 2000 raw,
            requestPayload = payload,
            attemptedAt = now
          }
  pure $ case (lookup "enc_error_code" fields, lookup "enc_response" fields) of
    (Just errCode, mbReason)
      | not (T.null errCode) ->
        mkResult False (Just errCode) (mbReason <|> Just "gateway rejected the request") body
    (_, Just encResponse) -> case decryptResponse workingKey encResponse of
      Right decrypted ->
        let (ok, status, message) = interpretDecrypted decrypted
         in mkResult ok status message decrypted
      Left err -> mkResult False Nothing (Just $ err <> "; enc_response=" <> T.take 200 encResponse) body
    _ -> mkResult False Nothing (Just "no enc_response in gateway reply") body

interpretDecrypted :: Text -> (Bool, Maybe Text, Maybe Text)
interpretDecrypted decrypted =
  case A.decodeStrict @CreateSplitPayoutResponse (TE.encodeUtf8 decrypted) of
    Just response ->
      let res = response.result
       in ( res.status == 0,
            Just $ show res.status,
            nonBlank res.errorDesc <|> Just (T.take 500 decrypted)
          )
    Nothing -> (False, Nothing, Just $ T.take 500 decrypted)
  where
    nonBlank = \case
      Just txt | not (T.null (T.strip txt)) -> Just txt
      _ -> Nothing
