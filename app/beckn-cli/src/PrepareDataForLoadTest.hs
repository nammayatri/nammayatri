{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module PrepareDataForLoadTest
  ( prepareDataForLoadTest,
    defaultPrivateKey,
    cleanupData,
    runK6Script,
  )
where

import qualified Beckn.Types.Core.API.Search as API
import qualified Beckn.Types.Core.Context as API
import Beckn.Utils.Example (Example (example))
import qualified Beckn.Utils.SignatureAuth as S
import qualified Data.Aeson as J
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Time.Clock.POSIX as Time
import qualified EulerHS.Language as L
import EulerHS.Prelude
import System.Directory (removeFile)

data RequestForLoadTest = RequestForLoadTest
  { rawRequest :: !Text,
    signature :: !Text
  }
  deriving (Show, ToJSON, FromJSON, Generic)

defaultPrivateKey :: ByteString
defaultPrivateKey = "ftjLZNZ6+QG8KAcNqax3NiX6Cg1bKVVdnbygReTwpFw="

prepareDataForLoadTest :: ByteString -> Int -> Text -> L.Flow ()
prepareDataForLoadTest privateKey nmbOfReq filePath = do
  reqs <- replicateM nmbOfReq $ do
    request <- generateSearchRequest
    now <- L.runIO Time.getPOSIXTime
    pure $ do
      let body = J.encode request
      let headers = [("(created)", ""), ("(expires)", ""), ("digest", "")]
      let signatureParams = S.mkSignatureParams "JUSPAY.MOBILITY.APP.UAT.1" "juspay-mobility-bap-1-key" now 600 S.Ed25519
      signature <- S.sign (Base64.decodeLenient privateKey) signatureParams (LBS.toStrict body) headers
      pure $ RequestForLoadTest (decodeUtf8 body) (decodeUtf8 $ S.encode $ S.SignaturePayload signature signatureParams)
  L.runIO . writeFile (T.unpack filePath) . decodeUtf8 . J.encode . catMaybes $ reqs

cleanupData :: Text -> L.Flow ()
cleanupData = L.runIO . removeFile . T.unpack

runK6Script :: Text -> Text -> L.Flow String
runK6Script url filePath =
  L.runSysCmd $
    "k6 run -e LOAD_TEST_URL="
      <> T.unpack url
      <> " -e FILE_PATH="
      <> T.unpack filePath
      <> " ./dev/load-test/script.js"

generateSearchRequest :: L.Flow API.SearchReq
generateSearchRequest = do
  txnId <- L.generateGUID
  let context = example @API.Context & #_transaction_id .~ txnId
  let intent = example @API.SearchIntent
  pure $ API.SearchReq context intent
