module SignatureAuth
  ( signatureAuthTests,
  )
where

import qualified Beckn.Utils.SignatureAuth as HttpSig
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.CaseInsensitive as CI
import qualified Data.Text as T
import Data.Time.Clock (NominalDiffTime)
import EulerHS.Prelude
import Network.HTTP.Types (methodPost)
import Test.Tasty
import Test.Tasty.HUnit

secretKey :: ByteString
Right secretKey = Base64.decode "faGTaRPYJx8OQ7zbQOlrevtWTmqs+hAJr2tn08Dgx1U="

publicKey :: ByteString
Right publicKey = Base64.decode "oUvUHbL/9ZU4JT0RjcnAvgSXpXtyNTDBWRWDpnlP4N0="

examplePath :: ByteString
examplePath = "/foo?param=value&pet=dog"

exampleMethod :: ByteString
exampleMethod = methodPost

exampleHeaders :: [(Text, ByteString)]
exampleHeaders =
  [ ("(request-target)", exampleMethod <> " " <> examplePath),
    ("(created)", ""),
    ("(expires)", ""),
    ("host", "example.com"),
    ("date", "Sun, 05 Jan 2014 21:31:40 GMT"),
    ("content-type", "application/json"),
    ("digest", "SHA-256=X48E9qOokqqrvdts8nOJRJN3OWDUoyWxBf7kbu9DBPE="),
    ("content-length", "18")
  ]

-- Might be useful if we decide to do a Digest as well
--exampleBody :: ByteString
--exampleBody = "{\"hello\": \"world\"}"

exampleKeyId :: ByteString
exampleKeyId = "Test"

exampleCreated :: Integer
exampleCreated = 1402170695

exampleExpires :: Integer
exampleExpires = 1402170699

exampleSignature :: ByteString
exampleSignature = "MnU8mHCiBQa82Gln4NmxUOauEfsto1V6q3QbPcxZ6L9xBFN+zIU8lklhps8Sx6VaVGfySYehB3FLYJ2TQc0LDA=="

-- | algorithm is modified to hs2019
exampleSignatureHeader :: ByteString
exampleSignatureHeader =
  "Signature keyId=\"" <> exampleKeyId <> "\",algorithm=\"hs2019\",\n"
    <> "created="
    <> show exampleCreated
    <> ",expires="
    <> show exampleExpires
    <> ",\n"
    <> "headers=\""
    <> (encodeUtf8 . T.intercalate " " $ fst <$> exampleHeaders)
    <> "\",\n"
    <> "signature=\""
    <> exampleSignature
    <> "\""

toTime :: Integer -> Maybe NominalDiffTime
toTime = Just . fromInteger

simpleDecode :: TestTree
simpleDecode =
  testCase "Simple header decode" $
    case HttpSig.decode exampleSignatureHeader of
      Left err -> assertFailure $ "signature decode failed: " <> err
      Right (sig, params) -> do
        Right sig @?= Base64.decode exampleSignature
        HttpSig.headers params @?= fst <$> exampleHeaders
        HttpSig.algorithm params @?= HttpSig.Hs2019
        HttpSig.created params @?= toTime exampleCreated
        HttpSig.expires params @?= toTime exampleExpires

exampleParams :: HttpSig.SignatureParams
exampleParams =
  HttpSig.SignatureParams
    (decodeUtf8 exampleKeyId)
    HttpSig.Hs2019
    (fst <$> exampleHeaders)
    (toTime exampleCreated)
    (toTime exampleExpires)

dropNewline :: ByteString -> ByteString
dropNewline = BS.filter (/= 10)

simpleEncode :: TestTree
simpleEncode =
  testCase "Simple header encode" $ do
    let Right sig = Base64.decode exampleSignature
    -- filtering '\n'
    HttpSig.encode sig exampleParams @?= dropNewline exampleSignatureHeader

signRequest :: TestTree
signRequest =
  testCase "Sign a request" $ do
    let mSig =
          HttpSig.sign secretKey exampleParams exampleMethod examplePath (first (CI.mk . encodeUtf8) <$> exampleHeaders)
    case mSig of
      Nothing -> assertFailure "Could not sign request"
      Just sig ->
        -- filtering '\n'
        HttpSig.encode sig exampleParams @?= dropNewline exampleSignatureHeader

verifyRequest :: TestTree
verifyRequest =
  testCase "Verify a signed request" $ do
    let eSig = Base64.decode exampleSignature
    case eSig of
      Left err -> assertFailure $ "Could not decode request: " <> err
      Right sig -> assertBool "Signature is valid" $ do
        HttpSig.verify publicKey exampleParams exampleMethod examplePath (first (CI.mk . encodeUtf8) <$> exampleHeaders) sig

signatureAuthTests :: TestTree
signatureAuthTests =
  testGroup
    "Signature auth tests"
    [ simpleDecode,
      simpleEncode,
      signRequest,
      verifyRequest
    ]
