{-# LANGUAGE OverloadedStrings #-}

module Domain.Action.Beckn.FRFSSeller.Onboarding
  ( SubscribeReq (..),
    SubscribeRes (..),
    onSubscribe,
    siteVerification,
    verificationPage,
  )
where

import qualified BecknV2.FRFS.Enums as SpecEnums
import qualified BecknV2.OnDemand.Enums as BecknSpec
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text.Encoding as TE
import qualified Domain.Types.BecknConfig as DBC
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import Environment (Flow)
import Kernel.Prelude
import Kernel.Utils.Common
import qualified SharedLogic.FRFSSeller.Common as Common
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import qualified Storage.CachedQueries.BecknConfig as QBC
import qualified Storage.CachedQueries.Merchant as CQM
import Tools.Error
import qualified Tools.OndcOnboarding as Onboarding

data SubscribeReq = SubscribeReq
  { subscriber_id :: Maybe Text,
    challenge :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON)

newtype SubscribeRes = SubscribeRes {answer :: Text}
  deriving (Generic, Show, FromJSON, ToJSON)

onSubscribe :: Text -> SubscribeReq -> Flow SubscribeRes
onSubscribe operator req = do
  integratedBPPConfig <- sellerIntegratedBPPConfig operator
  encPriv <-
    integratedBPPConfig.ondcEncryptionPrivateKey
      & fromMaybeM (InvalidRequest $ "No ondcEncryptionPrivateKey configured for " <> operator)
  registryPub <-
    integratedBPPConfig.ondcRegistryPublicKey
      & fromMaybeM (InvalidRequest $ "No ondcRegistryPublicKey configured for " <> operator)
  case Onboarding.decryptChallenge encPriv registryPub req.challenge of
    Left err -> do
      logError $ "FRFS seller on_subscribe failed for " <> operator <> ": " <> err
      throwError (InvalidRequest $ "Could not answer the challenge: " <> err)
    Right answer -> do
      logInfo $ "FRFS seller on_subscribe answered for " <> operator
      pure (SubscribeRes answer)

-- | What gets signed is the SUBSCRIBER ID, not a request id, because that is what the
-- Go service signs: @ondc_generic_controller.go@ assigns @id.SubscriberId@ to a variable
-- it calls @requestId@ and never reads a query parameter. Kept for parity with the live
-- registration, since these subscribers are already admitted under that behaviour. A
-- @request_id@ parameter still wins when the registry sends one, which is what the ONDC
-- documentation describes.
siteVerification :: Text -> Maybe Text -> Flow C8.ByteString
siteVerification operator mbRequestId = do
  becknConfig <- sellerBecknConfig operator
  let toSign = fromMaybe becknConfig.subscriberId mbRequestId
  signingKey <- asks (.signingKey)
  case Onboarding.signRequestId signingKey toSign of
    Left err -> throwError (InternalError $ "Could not sign the request id: " <> err)
    Right signed -> pure (verificationPage signed)

-- | Separated from the handler so the exact markup can be asserted: the registry parses
-- this page, and a changed attribute order or quoting style fails verification.
verificationPage :: Text -> C8.ByteString
verificationPage signed =
  TE.encodeUtf8 $
    "<html><head><meta name='ondc-site-verification' content='"
      <> signed
      <> "'/></head><body>ONDC Site Verification Page</body></html>"

sellerBecknConfig :: Text -> Flow DBC.BecknConfig
sellerBecknConfig operator = do
  merchant <-
    CQM.findByShortId (Common.operatorMerchantShortId operator)
      >>= fromMaybeM (MerchantDoesNotExist operator)
  QBC.findByMerchantIdDomainAndVehicle merchant.id (show SpecEnums.FRFS) BecknSpec.METRO
    >>= fromMaybeM (BecknConfigNotFound $ "merchantId:" <> merchant.id.getId <> " domain:FRFS vehicle:METRO")

-- | on_subscribe carries no city, so the operator's row is picked out of the across-cities
-- set by merchant. One operator is one merchant, so at most one row matches.
sellerIntegratedBPPConfig :: Text -> Flow DIBC.IntegratedBPPConfig
sellerIntegratedBPPConfig operator = do
  merchant <-
    CQM.findByShortId (Common.operatorMerchantShortId operator)
      >>= fromMaybeM (MerchantDoesNotExist operator)
  configs <- SIBC.findAllIntegratedBPPConfigAcrossCities BecknSpec.METRO DIBC.MULTIMODAL
  find (\config -> config.merchantId == merchant.id) configs
    & fromMaybeM (InvalidRequest $ "No integrated BPP config for merchantId:" <> merchant.id.getId <> " vehicle:METRO")
