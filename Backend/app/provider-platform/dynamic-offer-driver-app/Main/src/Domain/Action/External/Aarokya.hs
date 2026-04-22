module Domain.Action.External.Aarokya where

import API.External.Aarokya as Aarokya
import Domain.Types.External.Aarokya as Aarokya
import qualified Domain.Types.Extra.MerchantServiceConfig as ExtraMSC
import EulerHS.Prelude
import Kernel.External.Encryption (decrypt)
import Kernel.Types.Error
import Kernel.Utils.Common
import Tools.Metrics

callAarokyaGenerateToken ::
  ( CoreMetrics m,
    EncFlow m r,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  ExtraMSC.AarokyaSdkConfig ->
  AarokyaTokenRequest ->
  m AarokyaTokenResponse
callAarokyaGenerateToken aarokyaCfg req = do
  apiKey <- decrypt aarokyaCfg.apiKey
  let url = aarokyaCfg.url
  res <-
    callAPI url (Aarokya.generateToken (Just apiKey) req) "aarokyaGenerateToken" Aarokya.aarokyaGenerateTokenAPI
      >>= fromEitherM (ExternalAPICallError (Just "AAROKYA_GENERATE_TOKEN_FAILED") url)
  logDebug $ "Aarokya generateToken response received"
  pure res
