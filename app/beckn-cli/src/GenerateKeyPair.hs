{-# LANGUAGE DerivingStrategies #-}

module GenerateKeyPair where

import Beckn.Types.Credentials
import qualified Beckn.Utils.SignatureAuth as HttpSig
import qualified EulerHS.Language as L
import EulerHS.Prelude

data GenerateKeyPairResponse = GenerateKeyPairResponse
  { privateKey :: PrivateKey,
    publicKey :: PublicKey
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

generateKeyPair :: L.Flow GenerateKeyPairResponse
generateKeyPair = do
  L.logInfo @Text "GenerateKeyPair" "Generating random key pair."
  L.runIO HttpSig.generateKeyPair <&> uncurry GenerateKeyPairResponse
