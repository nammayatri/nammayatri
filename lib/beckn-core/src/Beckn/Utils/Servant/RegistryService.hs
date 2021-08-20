module Beckn.Utils.Servant.RegistryService where

import Beckn.Types.Error
import Beckn.Types.Id
import qualified Beckn.Types.Monitoring.Prometheus.Metrics as Metrics
import qualified Beckn.Types.Registry.API as RegistryAPI
import Beckn.Types.Registry.Domain (Domain)
import Beckn.Types.Registry.Subscriber (Subscriber)
import Beckn.Utils.Common
import qualified Beckn.Utils.Registry as Registry
import Beckn.Utils.Servant.SignatureAuth (LookupAction, LookupRegistry, registryAuthManagerKey)
import EulerHS.Prelude
import qualified EulerHS.Types as T
import Servant.Client.Core (ClientError)

decodeViaRegistry ::
  ( Metrics.CoreMetrics m,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl, "registryUrl" ::: BaseUrl]
  ) =>
  (ShortId a -> m (Maybe a)) ->
  Domain ->
  LookupAction (LookupRegistry a) m
decodeViaRegistry findOrgByShortId domain signaturePayload = do
  registryUrl <- asks (.registryUrl)
  nwAddress <- asks (.nwAddress)
  let shortId = signaturePayload.params.keyId.subscriberId
  pk <- getPubKeyFromRegistry registryUrl shortId
  logTagDebug "SignatureAuth" $ "Got Signature: " <> show signaturePayload
  org <- findOrgByShortId (ShortId shortId) >>= fromMaybeM OrgNotFound
  return (org, pk, nwAddress)
  where
    getPubKeyFromRegistry registryUrl shortId =
      registryCall registryUrl shortId domain
        <&> listToMaybe
        >>= fromMaybeM (InternalError "Registry didn't provide information on Organization.")
        <&> (.signing_public_key)
        >>= fromMaybeM (InternalError "Registry responded with subscriber without public key.")
        <&> Registry.decodeKey
        >>= fromMaybeM (InternalError "Couldn't decode public key from registry.")

registryCall ::
  (MonadFlow m, Metrics.CoreMetrics m) =>
  BaseUrl ->
  Text ->
  Domain ->
  m [Subscriber]
registryCall url shortId domain = do
  callAPI' (Just registryAuthManagerKey) url (T.client RegistryAPI.lookupAPI request) "lookup"
    >>= fromEitherM (registryCallError url)
  where
    request = RegistryAPI.LookupRequest (Just shortId) Nothing (Just domain) Nothing Nothing

registryCallError :: BaseUrl -> ClientError -> ExternalAPICallError
registryCallError = ExternalAPICallError (Just "REGISTRY_CALL_ERROR")
