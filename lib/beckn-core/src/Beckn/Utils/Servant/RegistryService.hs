module Beckn.Utils.Servant.RegistryService where

import Beckn.Types.Error
import Beckn.Types.Id
import qualified Beckn.Types.Monitoring.Prometheus.Metrics as Metrics
import qualified Beckn.Types.Registry.API as RegistryAPI
import Beckn.Types.Registry.Domain (Domain)
import qualified Beckn.Types.Registry.Routes as Registry
import Beckn.Types.Registry.Subscriber (Subscriber)
import Beckn.Utils.Common
import qualified Beckn.Utils.Registry as Registry
import Beckn.Utils.Servant.SignatureAuth (LookupAction, LookupRegistry)
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
      registryLookup registryUrl shortId domain
        <&> listToMaybe
        >>= fromMaybeM (InternalError "Registry didn't provide information on Organization.")
        <&> (.signing_public_key)
        >>= fromMaybeM (InternalError "Registry responded with subscriber without signing public key.")
        <&> Registry.decodeKey
        >>= fromMaybeM (InternalError "Couldn't decode public key from registry.")

decodeAndGetRegistryEncPubKey ::
  ( Metrics.CoreMetrics m,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl, "registryUrl" ::: BaseUrl]
  ) =>
  (ShortId a -> m (Maybe a)) ->
  Domain ->
  LookupAction (LookupRegistry Text) m
decodeAndGetRegistryEncPubKey findOrgByShortId domain signaturePayload = do
  registryUrl <- asks (.registryUrl)
  nwAddress <- asks (.nwAddress)
  let shortId = signaturePayload.params.keyId.subscriberId
  (signingPubKey, encryptionPubKey) <- getPubKeysFromRegistry registryUrl shortId
  logTagDebug "SignatureAuth" $ "Got Signature: " <> show signaturePayload
  void $ findOrgByShortId (ShortId shortId) >>= fromMaybeM OrgNotFound -- Not sure if we need this
  return (encryptionPubKey, signingPubKey, nwAddress)
  where
    getPubKeysFromRegistry registryUrl shortId = do
      subscriber <-
        registryLookup registryUrl shortId domain
          <&> listToMaybe
          >>= fromMaybeM (InternalError "Registry didn't provide information on Organization.")
      signingPubKey <-
        subscriber.signing_public_key
          & fromMaybeM (InternalError "Registry responded with subscriber without signing public key.")
          <&> Registry.decodeKey
          >>= fromMaybeM (InternalError "Couldn't decode public key from registry.")
      encryptionPubKey <-
        subscriber.encr_public_key
          & fromMaybeM (InternalError "Registry responded with subscriber without encryption public key.")
      pure (signingPubKey, encryptionPubKey)

registryLookup ::
  (MonadFlow m, Metrics.CoreMetrics m) =>
  BaseUrl ->
  Text ->
  Domain ->
  m [Subscriber]
registryLookup url shortId domain = do
  callAPI url (T.client Registry.lookupAPI request) "lookup"
    >>= fromEitherM (registryLookupCallError url)
  where
    request = RegistryAPI.LookupRequest (Just shortId) Nothing (Just domain) Nothing Nothing

registryLookupCallError :: BaseUrl -> ClientError -> ExternalAPICallError
registryLookupCallError = ExternalAPICallError (Just "REGISTRY_CALL_ERROR")
