{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for the BHIM AES envelope (PartnerAuth.BHIM.Encryption).
-- Uses the deterministic, pure core (encryptEnvelopeWithIV) so no Flow runtime
-- is needed. The random-IV behaviour is covered by encrypting the same
-- plaintext under two different IVs.
module PartnerAuth.BhimAES (tests) where

import qualified Data.ByteString as BS
import Data.Text (Text)
import PartnerAuth.BHIM.Encryption (decryptEnvelope, encryptEnvelopeWithIV)
import Test.Tasty
import Test.Tasty.HUnit
import Prelude

-- A 32-byte (256-bit) AES key.
testKey :: Text
testKey = "0123456789abcdef0123456789abcdef"

iv1, iv2 :: BS.ByteString
iv1 = BS.pack [0 .. 15]
iv2 = BS.pack [16 .. 31]

samplePlaintext :: Text
samplePlaintext = "{\"token\":\"bhim-test-token-123\"}"

tests :: TestTree
tests =
  testGroup
    "PartnerAuth.BHIM AES envelope"
    [ testCase "round-trips plaintext (encrypt then decrypt)" $ do
        enc <- either (assertFailure . ("encrypt failed: " <>)) pure (encryptEnvelopeWithIV testKey iv1 samplePlaintext)
        dec <- either (assertFailure . ("decrypt failed: " <>)) pure (decryptEnvelope testKey enc)
        dec @?= samplePlaintext,
      testCase "different IVs => different ciphertext, both decrypt back" $ do
        enc1 <- either (assertFailure . ("encrypt1 failed: " <>)) pure (encryptEnvelopeWithIV testKey iv1 samplePlaintext)
        enc2 <- either (assertFailure . ("encrypt2 failed: " <>)) pure (encryptEnvelopeWithIV testKey iv2 samplePlaintext)
        assertBool "ciphertexts should differ for different IVs" (enc1 /= enc2)
        d1 <- either (assertFailure . ("decrypt1 failed: " <>)) pure (decryptEnvelope testKey enc1)
        d2 <- either (assertFailure . ("decrypt2 failed: " <>)) pure (decryptEnvelope testKey enc2)
        d1 @?= samplePlaintext
        d2 @?= samplePlaintext,
      testCase "rejects a non-32-byte key" $
        case encryptEnvelopeWithIV "tooshortkey" iv1 samplePlaintext of
          Left _ -> pure ()
          Right _ -> assertFailure "expected failure for non-32-byte key"
          -- TODO: once BHIM provides an official sample vector
          -- { key, plaintext, ciphertext }, assert
          -- decryptEnvelope key sampleCiphertext == plaintext to confirm
          -- cross-implementation interop (mode / IV layout / padding).
    ]
