{-# LANGUAGE OverloadedLabels #-}

module Beckn.Utils.Registry
  ( decodeKey,
    lookupKey,
    lookupOrg,
    lookupSigningKey,
    prepareSigningKeyPair,
  )
where

import Beckn.Types.Credentials
import qualified Beckn.Utils.SignatureAuth as HttpSig
import qualified Data.ByteString.Base64 as Base64
import Data.Generics.Labels ()
import EulerHS.Prelude

lookupKey :: Text -> [Credential] -> Maybe Credential
lookupKey uniqueKeyId = find (\credential -> credential ^. #uniqueKeyId == uniqueKeyId)

lookupOrg :: Text -> [Credential] -> Maybe Credential
lookupOrg shortOrgId = find (\credential -> credential ^. #shortOrgId == shortOrgId)

lookupSigningKey :: Text -> [SigningKey] -> Maybe SigningKey
lookupSigningKey uniqueKeyId = find (\signingKey -> signingKey ^. #uniqueKeyId == uniqueKeyId)

decodeKey :: Text -> Maybe ByteString
decodeKey = rightToMaybe . Base64.decode . encodeUtf8

prepareSigningKeyPair :: IO (ByteString, ByteString)
prepareSigningKeyPair = do
  (privateKey, publicKey) <- HttpSig.generateKeyPair
  pure (Base64.encode privateKey, Base64.encode publicKey)
