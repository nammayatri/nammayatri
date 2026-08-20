{-# LANGUAGE OverloadedStrings #-}

module Tools.HTTPManagerSpec (tests) where

import qualified Data.Text as T
import EulerHS.Prelude
import Test.Tasty
import Test.Tasty.HUnit
import Tools.HTTPManager (prepareMutualTLSHttpManager)

tests :: TestTree
tests =
  testGroup
    "mutual-TLS HTTP manager"
    [ testCase "refuses a PEM with no private key, before any connection" $
        rejectedBecause "private key" certOnlyPem,
      testCase "refuses a blob holding no certificate" $
        rejectedBecause "no certificate" "not a pem at all"
    ]

rejectedBecause :: Text -> ByteString -> Assertion
rejectedBecause expected pem =
  case prepareMutualTLSHttpManager "kmrl" 10000 pem Nothing of
    Left err ->
      assertBool
        ("rejected, but for the wrong reason: " <> T.unpack err)
        (expected `T.isInfixOf` err)
    Right _ -> assertFailure "accepted a PEM that cannot authenticate us"

certOnlyPem :: ByteString
certOnlyPem =
  "-----BEGIN CERTIFICATE-----\n\
  \MIIBizCCATGgAwIBAgIUc2vCM4A/KAqQM+E1IMkgqSqGeekwCgYIKoZIzj0EAwIw\n\
  \GzEZMBcGA1UEAwwQa21ybC1jbGllbnQtdGVzdDAeFw0yNjA4MjAxMTQ5MTlaFw0z\n\
  \NjA4MTcxMTQ5MTlaMBsxGTAXBgNVBAMMEGttcmwtY2xpZW50LXRlc3QwWTATBgcq\n\
  \hkjOPQIBBggqhkjOPQMBBwNCAAQb2tJahvoafew5BvIqinrtSpbfX6AlWRYP3L1j\n\
  \uu7IqrPGasTKvQYdR8gfy/tAHMSDSQDPmjoKFmHjKfavXAdko1MwUTAdBgNVHQ4E\n\
  \FgQUAFbd5xyyXW0ZazhenPhVghnPPO0wHwYDVR0jBBgwFoAUAFbd5xyyXW0Zazhe\n\
  \nPhVghnPPO0wDwYDVR0TAQH/BAUwAwEB/zAKBggqhkjOPQQDAgNIADBFAiAcIp5l\n\
  \2tJOpjrTLdE+/kNr63syihagehGagMxhCwJ9yAIhAKjdeMPtA2cBHagDGJEsOHxF\n\
  \9APMZ/FOLD16mrBR8hPz\n\
  \-----END CERTIFICATE-----\n"
