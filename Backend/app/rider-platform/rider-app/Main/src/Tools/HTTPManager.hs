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
import Data.X509 (Certificate (..), CertificateChain (..), SignedCertificate, getCertificate)
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
  -- | Server hostname. Must be the real host: it is what goes out as SNI, and a
  -- gateway that routes on SNI (Axis does) fails the handshake without it.
  Text ->
  Int ->
  BS.ByteString ->
  Maybe BS.ByteString ->
  m (Either Text (HMap.HashMap DT.Text Http.ManagerSettings))
prepareMutualTLSHttpManager integration host timeout certAndKeyPem mbCaPem = do
  systemStore <- liftIO getSystemCertificateStore
  pure $ buildSettings integration host timeout certAndKeyPem mbCaPem systemStore

buildSettings ::
  Text ->
  Text ->
  Int ->
  BS.ByteString ->
  Maybe BS.ByteString ->
  CertificateStore ->
  Either Text (HMap.HashMap DT.Text Http.ManagerSettings)
buildSettings integration host timeout certAndKeyPem mbCaPem systemStore = do
  credential <- loadCredential certAndKeyPem
  caStore <- maybe (Right systemStore) loadCaStore mbCaPem
  -- The hostname is load-bearing: `TLSSettings` hands these params to the connection
  -- verbatim (unlike TLSSettingsSimple, it does NOT fill the host in), so an empty
  -- name here means no SNI on the wire and no hostname to validate the server cert
  -- against. Axis rejects the handshake outright in that case.
  let base = TLS.defaultParamsClient (DT.unpack host) ""
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
      (key : _) -> Right (CertificateChain (orderLeafFirst certs), key)

-- TLS requires our own certificate first and each subsequent one to certify the
-- previous (RFC 5246 7.4.2), but 'readSignedObjectFromMemory' just preserves PEM
-- order. Axis rejects a chain that leads with their intermediate: it reads entry
-- zero as the client identity and fatals with an opaque handshake_failure.
orderLeafFirst :: [SignedCertificate] -> [SignedCertificate]
orderLeafFirst certs = maybe certs walkFrom (find isLeaf certs)
  where
    subjectOf = certSubjectDN . getCertificate
    issuerOf = certIssuerDN . getCertificate
    without cert = filter ((/= subjectOf cert) . subjectOf)
    isLeaf cert = not $ any (\other -> subjectOf other /= subjectOf cert && issuerOf other == subjectOf cert) certs
    walkFrom leaf = leaf : climb leaf (without leaf certs)
    climb _ [] = []
    climb cert rest = case find ((== issuerOf cert) . subjectOf) rest of
      Nothing -> rest
      Just issuer -> issuer : climb issuer (without issuer rest)

loadCaStore :: BS.ByteString -> Either Text CertificateStore
loadCaStore pem =
  case X509Mem.readSignedObjectFromMemory pem :: [SignedCertificate] of
    [] -> Left "CA PEM contained no certificate"
    certs -> Right (makeCertificateStore certs)
