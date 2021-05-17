{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}

module GenerateKeyPair where

import Beckn.Utils.Registry
import qualified EulerHS.Language as L
import EulerHS.Prelude

data GenerateKeyPairResponse = GenerateKeyPairResponse
  { privateKey :: Text,
    publicKey :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

generateKeyPair :: L.Flow GenerateKeyPairResponse
generateKeyPair = do
  L.logInfo @Text "GenerateKeyPair" "Generating random key pair."
  (privateKeyBS, publicKeyBS) <- L.runIO prepareSigningKeyPair
  pure $
    GenerateKeyPairResponse
      { privateKey = decodeUtf8 privateKeyBS,
        publicKey = decodeUtf8 publicKeyBS
      }
