{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module PrepareDataForLoadTest
  ( prepareDataForLoadTest,
    defaultPrivateKey,
  )
where

import qualified Beckn.Types.Core.API.Search as API
import qualified Beckn.Types.Core.Context as API
import Beckn.Utils.Example
import qualified Beckn.Utils.SignatureAuth as S
import qualified Data.Aeson as J
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Time.Clock.POSIX as Time
import qualified EulerHS.Language as L
import EulerHS.Prelude

data RequestForLoadTest = RequestForLoadTest
  { rawRequest :: !Text,
    signature :: !Text
  }
  deriving (Show, ToJSON, FromJSON, Generic)

defaultPrivateKey :: ByteString
defaultPrivateKey = "ftjLZNZ6+QG8KAcNqax3NiX6Cg1bKVVdnbygReTwpFw="

prepareDataForLoadTest :: ByteString -> Int -> L.Flow ()
prepareDataForLoadTest privateKey nmbOfReq = do
  reqs <- replicateM nmbOfReq $ do
    request <- generateSearchRequest
    now <- L.runIO Time.getPOSIXTime
    let body = J.encode request
    let headers = [("(created)", ""), ("(expires)", ""), ("digest", "")]
    let signatureParams = S.mkSignatureParams "JUSPAY.MOBILITY.APP.UAT.1" "juspay-mobility-bap-1-key" now 600 S.Ed25519
    let signature = fromJust $ S.sign (Base64.decodeLenient privateKey) signatureParams (LBS.toStrict body) headers
    pure $ RequestForLoadTest (decodeUtf8 body) (decodeUtf8 $ S.encode $ S.SignaturePayload signature signatureParams)
  L.runIO $ writeFile "./dev/load-test/reqForLoadTest.json" $ decodeUtf8 $ J.encode reqs

generateSearchRequest :: L.Flow API.SearchReq
generateSearchRequest = do
  txnId <- L.generateGUID
  let context = example @API.Context & #_transaction_id .~ txnId
  let intent = example @API.SearchIntent
  pure $ API.SearchReq context intent
