{-# LANGUAGE OverloadedLabels #-}

module Beckn.Utils.Registry
  ( decodeKey,
    lookupKey,
    lookupOrg,
  )
where

import Beckn.Types.Credential
import qualified Data.ByteString.Base64 as Base64
import Data.Generics.Labels ()
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
    mkCredential uniqueKeyId shortOrgId =
      Credential uniqueKeyId shortOrgId examplePubKey examplePrivKey
    mkNSDLCredential uniqueKeyId shortOrgId =
      Credential uniqueKeyId shortOrgId nsdlPubKey Nothing

    examplePubKey = "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
    examplePrivKey = Just "ftjLZNZ6+QG8KAcNqax3NiX6Cg1bKVVdnbygReTwpFw="
    nsdlPubKey = "Fhjwaka1Za+ld+7Nms7S0C675r24mZoyWVn8JbYTjSs="

lookupKey :: L.MonadFlow m => Text -> m (Maybe Credential)
lookupKey k = return $ find (\c -> c ^. #uniqueKeyId == k) registry

lookupOrg :: L.MonadFlow m => Text -> m (Maybe Credential)
lookupOrg o = return $ find (\c -> c ^. #shortOrgId == o) registry

decodeKey :: Text -> Maybe ByteString
decodeKey = rightToMaybe . Base64.decode . encodeUtf8
