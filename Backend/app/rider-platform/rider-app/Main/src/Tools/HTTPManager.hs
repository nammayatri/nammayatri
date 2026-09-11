module Tools.HTTPManager
  ( prepareCRISHttpManager,
    crisHttpManagerKey,
    prepareMutualTLSHttpManager,
    mutualTLSManagerKey,
  )
where

import qualified Data.ByteString as BS
import qualified Data.HashMap.Internal as HMap
import qualified Data.Text as DT
import Data.X509 (CertificateChain (..), SignedCertificate)
import Data.X509.CertificateStore (CertificateStore, makeCertificateStore)
import qualified Data.X509.Memory as X509Mem
import EulerHS.Prelude
import Network.Connection (TLSSettings (..))
import Network.HTTP.Client as Http
import Network.HTTP.Client.TLS as Http
import qualified Network.TLS as TLS
import qualified Network.TLS.Extra.Cipher as TLS
import System.X509 (getSystemCertificateStore)

prepareCRISHttpManager :: Int -> HMap.HashMap DT.Text Http.ManagerSettings
prepareCRISHttpManager timeout =
  HMap.singleton (DT.pack crisHttpManagerKey) $
    Http.tlsManagerSettings {Http.managerResponseTimeout = Http.responseTimeoutMicro (timeout * 1000)}

crisHttpManagerKey :: String
crisHttpManagerKey = "cris-http-manager"

mutualTLSManagerKey :: Text -> String
mutualTLSManagerKey integration = "mtls-http-manager:" <> DT.unpack integration
{-# INLINE mutualTLSManagerKey #-}

-- | @mbCaPem@ pins the operator's CA when supplied. When it is absent we fall back to the
-- SYSTEM trust store, which is what the Go service does (@kochi_metro.go:90@ builds a
-- tls.Config with no RootCAs, and Go reads that as "use the host roots"). Without the
-- fallback 'TLS.defaultParamsClient' leaves sharedCAStore empty and validateDefault rejects
-- every server certificate, so a missing pin fails every request rather than defaulting to
-- normal verification. KMRL's gateway is DigiCert-issued, so there is no operator CA to pin.
prepareMutualTLSHttpManager ::
  MonadIO m =>
  Text ->
  Int ->
  BS.ByteString ->
  Maybe BS.ByteString ->
  m (Either Text (HMap.HashMap DT.Text Http.ManagerSettings))
prepareMutualTLSHttpManager integration timeout certAndKeyPem mbCaPem = do
  systemStore <- liftIO getSystemCertificateStore
  pure $ buildSettings integration timeout certAndKeyPem mbCaPem systemStore

buildSettings ::
  Text ->
  Int ->
  BS.ByteString ->
  Maybe BS.ByteString ->
  CertificateStore ->
  Either Text (HMap.HashMap DT.Text Http.ManagerSettings)
buildSettings integration timeout certAndKeyPem mbCaPem systemStore = do
  credential <- loadCredential certAndKeyPem
  caStore <- maybe (Right systemStore) loadCaStore mbCaPem
  let base = TLS.defaultParamsClient "" ""
      supported = TLS.clientSupported base
      shared = TLS.clientShared base
      hooks = TLS.clientHooks base
      params =
        base
          { TLS.clientSupported =
              supported
                { TLS.supportedCiphers = TLS.ciphersuite_default,
                  TLS.supportedVersions = [TLS.TLS12]
                },
            TLS.clientShared = shared {TLS.sharedCAStore = caStore},
            TLS.clientHooks = hooks {TLS.onCertificateRequest = \_ -> pure (Just credential)}
          }
  pure $
    HMap.singleton (DT.pack (mutualTLSManagerKey integration)) $
      (Http.mkManagerSettings (TLSSettings params) Nothing)
        { Http.managerResponseTimeout = Http.responseTimeoutMicro (timeout * 1000)
        }

loadCredential :: BS.ByteString -> Either Text TLS.Credential
loadCredential pem =
  case X509Mem.readSignedObjectFromMemory pem :: [SignedCertificate] of
    [] -> Left "client certificate PEM contained no certificate"
    certs -> case X509Mem.readKeyFileFromMemory pem of
      [] -> Left "client certificate PEM contained no private key (cert and key must be in the same blob)"
      (key : _) -> Right (CertificateChain certs, key)

loadCaStore :: BS.ByteString -> Either Text CertificateStore
loadCaStore pem =
  case X509Mem.readSignedObjectFromMemory pem :: [SignedCertificate] of
    [] -> Left "CA PEM contained no certificate"
    certs -> Right (makeCertificateStore certs)
