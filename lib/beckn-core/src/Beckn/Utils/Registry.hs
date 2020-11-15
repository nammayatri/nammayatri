module Beckn.Utils.Registry
  ( decodeKey,
    lookup,
  )
where

import Beckn.Types.Storage.Credential
import qualified Data.ByteString.Base64 as Base64
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified EulerHS.Language as L
import EulerHS.Prelude

registry :: [Credential]
registry =
  [ mkCredential "test-app-2-key" "test-app",
    mkCredential "fmd-test-app-key" "fmd-test-app",
    mkCredential "juspay-bg-1-key" "JUSPAY.BG.1",
    mkCredential "juspay-mobility-bap-1-key" "JUSPAY.MOBILITY.APP.UAT.1",
    mkCredential "juspay-mobility-bpp-1-key" "JUSPAY.MOBILITY.PROVIDER.UAT.1",
    mkCredential "juspay-bg-1-key" "JUSPAY.BG.1",
    mkCredential "juspay-mock-bap-1-key" "JUSPAY.BAP.MOCK.1",
    mkCredential "juspay-mock-bpp-1-key" "JUSPAY.BPP.MOCK.1"
  ]
  where
    mkCredential keyId orgId =
      Credential keyId orgId Nothing Nothing Nothing examplePubKey examplePrivKey examplePubKey examplePrivKey exampleValidFrom exampleValidTill

    examplePubKey = "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
    examplePrivKey = Just "ftjLZNZ6+QG8KAcNqax3NiX6Cg1bKVVdnbygReTwpFw="
    exampleValidFrom = posixSecondsToUTCTime 1605232000
    exampleValidTill = posixSecondsToUTCTime 1920592000

lookup :: L.MonadFlow m => Text -> m (Maybe Credential)
lookup k = return $ find (\c -> c ^. #_keyId == k) registry

decodeKey :: Text -> Maybe ByteString
decodeKey = rightToMaybe . Base64.decode . encodeUtf8
