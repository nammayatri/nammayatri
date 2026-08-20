{-# LANGUAGE OverloadedStrings #-}

module KMRL.CryptoSpec (tests) where

import qualified Crypto.PubKey.RSA as RSA
import Crypto.Random ()
import qualified Data.ByteString.Base64.URL as B64U
import qualified Data.ByteString.Char8 as C8
import EulerHS.Prelude
import ExternalBPP.ExternalAPI.Metro.KMRL.Crypto (encryptAndSign, verifyAndDecrypt)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  withResource (RSA.generate 256 65537) (const (pure ())) $ \getKeys ->
    testGroup
      "KMRL JWE/JWS envelope"
      [ testCase "emits the same JWE protected header Go does, byte for byte" $ do
          (pub, priv) <- getKeys
          tok <- mkToken pub priv
          jweProtectedOf tok @?= jweProtectedOf goToken,
        testCase "a token we produce round-trips through our own verifier" $ do
          (pub, priv) <- getKeys
          tok <- mkToken pub priv
          case verifyAndDecrypt pub priv tok of
            Left err -> assertFailure ("could not read back our own token: " <> show err)
            Right out -> out @?= plaintext,
        testCase "refuses a tampered signature" $ do
          (pub, priv) <- getKeys
          tok <- mkToken pub priv
          case verifyAndDecrypt pub priv (tamperLast tok) of
            Left _ -> pure ()
            Right _ -> assertFailure "accepted a token whose signature was altered",
        testCase "refuses a tampered ciphertext" $ do
          (pub, priv) <- getKeys
          tok <- mkToken pub priv
          case verifyAndDecrypt pub priv (tamperPayload tok) of
            Left _ -> pure ()
            Right _ -> assertFailure "accepted a token whose encrypted payload was altered",
        testCase "refuses a malformed envelope" $ do
          (pub, priv) <- getKeys
          case verifyAndDecrypt pub priv "not.a.jws.at.all" of
            Left _ -> pure ()
            Right _ -> assertFailure "accepted a malformed token"
      ]

mkToken :: RSA.PublicKey -> RSA.PrivateKey -> IO ByteString
mkToken pub priv =
  encryptAndSign pub priv plaintext
    >>= either (\err -> assertFailure ("could not build a token: " <> show err)) pure

plaintext :: ByteString
plaintext = "{\"stationCode\":\"ALVA\",\"fare\":\"30.00\",\"note\":\"kmrl interop vector\"}"

goToken :: ByteString
goToken = "eyJhbGciOiJSUzI1NiJ9.ZXlKaGJHY2lPaUpTVTBFdFQwRkZVQzB5TlRZaUxDSmxibU1pT2lKQk1qVTJSME5OSWl3aWVtbHdJam9pUkVWR0luMC5wNWxFd3FIbjV3VHNNdTZJZ0VQM1ZQVV9NUEFJLTZTYTJka2NDMHNnVUhySW1MZHRJQ2xkc1d1UUxUcXhxTi1iNUlaa3J4am9IWlIzUXd5R0xSRi1JQ3ZSaE1xNWlUaFlkcTE4MjFEaTEySDl1OGR3RV9MTkhFUnZQSEZvZTc2aUVvLVEyY2RNZVRITHJOQXh2a1FvVkZBbU1WcTZlUHdnZEUtUE95VUkyaTVaUWVoZTI3VlkwYUViT3VXT0FONVg5YzQ0ckpUc281SUdrd012TzBFZ0loUEVPSTVVLVFzUG9TYlN6dFo3eVRZT0hxS3M3a2ZSVkNCaE5sQkhjSEhJWnBGR1pjc3Y4V0c0dmEzd2RINVJtSFVXZWlmLWF3UU9NV2ZBTkJCaVpsZ2lyOTJxWGM0Rnh0TjI1Q1psNVpla0oyR0p2SkZrTkNBNExVa1ROcnlzOUEuRlhIMkdjUzFEQ0hVT1FscS5xWEc4QzljUXBBRm5mMzdCeTZKcmlWVXRGM2dueHdyc2lrZXRGakE0UWxucUdGVmF0MnBtd1ZXVmVlVlE5QU9yWGMyMEt6bzh6WXUzeDNSVkJyTVAybjBuY1FEZkdmMXBXQThmNFEuekFXNHBiSjBEdExSOFFnbWx4cU5hUQ.aktxPH8aoNxP-xPEw5kIY7o6q4jXZGMRE6Ipy9goS2HUZHCtl5U800wy-D-UzoiieB3cmHMwyffTKGiZCzPBBlt_I_S3BheG2zoXKXeoTIv6pCnbdfgS2BI_NZFIpZ0Nla9oHalnpOkN_DJ9Oh6ANIqNNGMMYoMfNGM4EGpITSSydCYc8U4iL_Aa95PGYTJL1eU6micZ0xQmWRHOhe-a2rbW_pc57m3qvegBswBP1xKcfD276Y5FE1pP9tocSdfG-vG3Zv04NffMiNA7bb1-3JMOWAAgfrtYI6y72jN3XnlRQgpP3exjdD9EpPfOV6DqyPBCKUg69w-AkzvoZGLcaQ"

jweProtectedOf :: ByteString -> ByteString
jweProtectedOf tok = case C8.split '.' tok of
  [_, payload, _] -> case C8.split '.' (decodeB64 payload) of
    (protectedSeg : _) -> protectedSeg
    _ -> "<no jwe segments>"
  _ -> "<not a 3-part jws>"

decodeB64 :: ByteString -> ByteString
decodeB64 s = either (const "<bad base64>") id (B64U.decode (s <> C8.replicate ((4 - C8.length s `mod` 4) `mod` 4) '='))

tamperLast :: ByteString -> ByteString
tamperLast bs = case C8.unsnoc bs of
  Just (i, c) -> C8.snoc i (if c == 'A' then 'B' else 'A')
  Nothing -> bs

tamperPayload :: ByteString -> ByteString
tamperPayload bs = case C8.split '.' bs of
  [h, p, s] -> C8.intercalate "." [h, tamperLast p, s]
  _ -> bs
