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

prepareCRISHttpManager :: Int -> HMap.HashMap DT.Text Http.ManagerSettings
prepareCRISHttpManager timeout =
  HMap.singleton (DT.pack crisHttpManagerKey) $
    Http.tlsManagerSettings {Http.managerResponseTimeout = Http.responseTimeoutMicro (timeout * 1000)}

crisHttpManagerKey :: String
crisHttpManagerKey = "cris-http-manager"

mutualTLSManagerKey :: Text -> String
mutualTLSManagerKey integration = "mtls-http-manager:" <> DT.unpack integration
{-# INLINE mutualTLSManagerKey #-}

prepareMutualTLSHttpManager ::
  Text ->
  Int ->
  BS.ByteString ->
  Maybe BS.ByteString ->
  Either Text (HMap.HashMap DT.Text Http.ManagerSettings)
prepareMutualTLSHttpManager integration timeout certAndKeyPem mbCaPem = do
  credential <- loadCredential certAndKeyPem
  caStore <- traverse loadCaStore mbCaPem
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
            TLS.clientShared = maybe shared (\store -> shared {TLS.sharedCAStore = store}) caStore,
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
