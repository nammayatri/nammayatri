module Beckn.Utils.Servant.RegistryService where

import Beckn.Types.Error
import Beckn.Types.Id
import qualified Beckn.Types.Monitoring.Prometheus.Metrics as Metrics
import qualified Beckn.Types.Registry.API as RegistryAPI
import qualified Beckn.Types.Registry.Routes as Registry
import Beckn.Types.Registry.Subscriber (Subscriber)
import Beckn.Utils.Common
import qualified Beckn.Utils.Registry as Registry
import Beckn.Utils.Servant.SignatureAuth (AuthenticatingEntity (..), LookupAction, LookupRegistry)
import EulerHS.Prelude
import qualified EulerHS.Types as T
import Servant.Client.Core (ClientError)

decodeViaRegistry ::
  ( Metrics.CoreMetrics m,
    HasFlowEnv m r '["registryUrl" ::: BaseUrl],
    AuthenticatingEntity r
  ) =>
  (ShortId a -> m (Maybe a)) ->
  LookupAction (LookupRegistry a) m
decodeViaRegistry findOrgByShortId signaturePayload = do
  registryUrl <- asks (.registryUrl)
  let shortId = signaturePayload.params.keyId.subscriberId
  let uniqueKeyId = signaturePayload.params.keyId.uniqueKeyId
  pk <- getPubKeyFromRegistry registryUrl uniqueKeyId
  logTagDebug "SignatureAuth" $ "Got Signature: " <> show signaturePayload
  org <- findOrgByShortId (ShortId shortId) >>= fromMaybeM OrgNotFound
  return (org, pk)
  where
    getPubKeyFromRegistry registryUrl uniqueKeyId =
      registryLookup registryUrl uniqueKeyId
        <&> listToMaybe
        >>= fromMaybeM (InternalError "Registry didn't provide information on Organization.")
        <&> (.signing_public_key)
        >>= fromMaybeM (InternalError "Registry responded with subscriber without signing public key.")
        <&> Registry.decodeKey
        >>= fromMaybeM (InternalError "Couldn't decode public key from registry.")

registryLookup ::
  (MonadFlow m, Metrics.CoreMetrics m) =>
  BaseUrl ->
  Text ->
  m [Subscriber]
registryLookup url uniqueKeyId = do
  callAPI url (T.client Registry.lookupAPI request) "lookup"
    >>= fromEitherM (registryLookupCallError url)
  where
    request =
      RegistryAPI.LookupRequest
        { unique_key_id = Just uniqueKeyId,
          subscriber_id = Nothing,
          _type = Nothing,
          domain = Nothing,
          country = Nothing,
          city = Nothing
        }

registryLookupCallError :: BaseUrl -> ClientError -> ExternalAPICallError
registryLookupCallError = ExternalAPICallError (Just "REGISTRY_CALL_ERROR")
