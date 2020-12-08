module Beckn.Utils.Registry
  ( decodeKey,
    lookupKey,
    lookupOrg,
  )
where

import Beckn.Types.Storage.Credential
import qualified Data.ByteString.Base64 as Base64
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified EulerHS.Language as L
import EulerHS.Prelude

registry :: [Credential]
registry =
  [ mkCredential "mobility-app-key" "mobility-app",
    mkCredential "fmd-test-app-key" "fmd-test-app",
    mkCredential "juspay-bg-1-key" "JUSPAY.BG.1",
    mkCredential "juspay-mobility-bap-1-key" "JUSPAY.MOBILITY.APP.UAT.1",
    mkCredential "juspay-mobility-bpp-1-key" "JUSPAY.MOBILITY.PROVIDER.UAT.1",
    mkCredential "juspay-bg-1-key" "JUSPAY.BG.1",
    mkCredential "juspay-mock-bap-1-key" "JUSPAY.BAP.MOCK.1",
    mkCredential "juspay-mock-bpp-1-key" "JUSPAY.BPP.MOCK.1",
    mkNSDLCredential "nsdl_bg_1" "NSDL.BG.1"
  ]
  where
    mkCredential keyId orgId =
      Credential keyId orgId Nothing Nothing Nothing examplePubKey examplePrivKey examplePubKey examplePrivKey exampleValidFrom exampleValidTill
    mkNSDLCredential keyId orgId =
      Credential keyId orgId Nothing Nothing Nothing nsdlPubKey nsdlPrivKey nsdlPubKey nsdlPrivKey exampleValidFrom exampleValidTill

    examplePubKey = "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
    examplePrivKey = Just "ftjLZNZ6+QG8KAcNqax3NiX6Cg1bKVVdnbygReTwpFw="
    nsdlPubKey = "RWc4RV9QUCV1yMb3xOPppJOO3Wmlb5E5G8raFj4UmIQ="
    nsdlPrivKey = Just "dmA/OxVM7C8WVZMF0Q7aFarUTp2hNUVZRJytAdG1s0M="
    exampleValidFrom = posixSecondsToUTCTime 1605232000
    exampleValidTill = posixSecondsToUTCTime 1920592000

lookupKey :: L.MonadFlow m => Text -> m (Maybe Credential)
lookupKey k = return $ find (\c -> c ^. #_keyId == k) registry

lookupOrg :: L.MonadFlow m => Text -> m (Maybe Credential)
lookupOrg o = return $ find (\c -> c ^. #_orgId == o) registry

decodeKey :: Text -> Maybe ByteString
decodeKey = rightToMaybe . Base64.decode . encodeUtf8
