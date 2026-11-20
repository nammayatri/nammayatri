module Main (main) where

import qualified AppAttestation.AppAttest.Verify as AppAttest
import AppAttestation.Canonical
import qualified AppAttestation.Domain.Types.AppAttestationConfig as Config
import AppAttestation.Domain.Types.Common
import qualified AppAttestation.KeyRegistry as KeyRegistry
import qualified AppAttestation.PlayIntegrity.Types as PI
import qualified AppAttestation.PlayIntegrity.Verify as PI
import AppAttestation.Policy
import AppAttestation.Types
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.Maybe (fromJust, isJust)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime (..), addUTCTime, fromGregorian)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import GHC.Generics (Generic)
import Test.Tasty
import Test.Tasty.HUnit
import Prelude

main :: IO ()
main = do
  fixture <- either fail pure =<< A.eitherDecodeFileStrict "test/AppAttestation/fixtures/app_attest.json"
  defaultMain $
    testGroup
      "AppAttestation"
      [ policyTests,
        canonicalTests,
        evidenceTests,
        playIntegrityTests,
        appAttestTests fixture,
        wireFormatTests,
        configTests
      ]

configTests :: TestTree
configTests =
  testGroup
    "Stored config"
    [ testCase "an empty object is the default config" $
        A.eitherDecode "{}" @?= Right Config.defaultAppAttestationConfig,
      testCase "Play recognition is required unless switched off" $
        Config.requireAndroidPlayRecognizedApp Config.defaultAppAttestationConfig @?= True,
      testCase "missing fields keep their defaults" $ do
        let decoded = A.eitherDecode "{\"mode\":\"SHADOW\",\"fallback\":{\"methods\":[\"CAPTCHA\"]}}"
        fmap Config.mode decoded @?= Right SHADOW
        fmap (Config.methods . Config.fallback) decoded @?= Right [CAPTCHA]
        fmap (Config.maxOtpPerPhonePerHour . Config.fallback) decoded @?= Right (Config.maxOtpPerPhonePerHour Config.defaultFallbackConfig)
        fmap Config.onInvalid decoded @?= Right (Config.onInvalid Config.defaultAppAttestationConfig),
      testCase "a full config round-trips" $ do
        let config =
              Config.defaultAppAttestationConfig
                { Config.mode = ENFORCE_ALL,
                  Config.requireAndroidPlayRecognizedApp = False,
                  Config.androidApps = [AndroidAppConfig "in.juspay.nammayatri.debug" ["FA:C6"]],
                  Config.iosApps = [IosAppConfig "XHH54AU526" "in.mobility.nammayatri.debug" DEVELOPMENT],
                  Config.maxOtpPerIosKeyPerDay = Just 5,
                  Config.fallback = Config.defaultFallbackConfig {Config.methods = [WHATSAPP_OTP, CAPTCHA], Config.captchaMinScore = Just 0.5}
                }
        A.eitherDecode (A.encode config) @?= Right config
    ]

wireFormatTests :: TestTree
wireFormatTests =
  testGroup
    "Wire format"
    [ testCase "key registration answers {status: REGISTERED}" $
        A.encode (KeyRegistry.RegisterKeyRes KeyRegistry.REGISTERED) @?= "{\"status\":\"REGISTERED\"}",
      testCase "key registration status round-trips" $
        A.eitherDecode "{\"status\":\"REGISTERED\"}" @?= Right (KeyRegistry.RegisterKeyRes KeyRegistry.REGISTERED)
    ]

policyTests :: TestTree
policyTests =
  testGroup
    "Policy"
    [ testCase "OFF and SHADOW always allow" $
        sequence_
          [ decide mode actions userKind outcome @?= DecisionAllow
            | mode <- [OFF, SHADOW],
              userKind <- [minBound .. maxBound],
              outcome <- [minBound .. maxBound]
          ],
      testCase "VALID is allowed in every mode" $
        sequence_ [decide mode actions userKind VALID @?= DecisionAllow | mode <- [minBound .. maxBound], userKind <- [minBound .. maxBound]],
      testCase "only the enforcing modes enforce, and ENFORCE_NEW_USERS only for new users" $ do
        map enforces [OFF, SHADOW, ENFORCE_NEW_USERS, ENFORCE_ALL] @?= [False, False, True, True]
        [enforcedFor mode userKind | mode <- [ENFORCE_NEW_USERS, ENFORCE_ALL], userKind <- [NewUser, ExistingUser]] @?= [True, False, True, True],
      testCase "ENFORCE_NEW_USERS lets existing users through" $
        sequence_ [decide ENFORCE_NEW_USERS actions ExistingUser outcome @?= DecisionAllow | outcome <- [minBound .. maxBound]],
      testCase "ENFORCE_NEW_USERS applies the action for new users" $ do
        decide ENFORCE_NEW_USERS actions NewUser INVALID @?= DecisionSilentDrop
        decide ENFORCE_NEW_USERS actions NewUser MISSING @?= DecisionReject
        decide ENFORCE_NEW_USERS actions NewUser UNSUPPORTED @?= DecisionFallback
        decide ENFORCE_NEW_USERS actions NewUser ERROR @?= DecisionFallback,
      testCase "ENFORCE_ALL applies the action for everyone" $
        sequence_
          [ do
              decide ENFORCE_ALL actions userKind INVALID @?= DecisionSilentDrop
              decide ENFORCE_ALL actions userKind MISSING @?= DecisionReject
              decide ENFORCE_ALL actions userKind ERROR @?= DecisionFallback
            | userKind <- [minBound .. maxBound]
          ],
      testCase "an ALLOW action allows" $
        decide ENFORCE_ALL actions {onMissing = ALLOW} NewUser MISSING @?= DecisionAllow,
      testCase "a device's own claim that it cannot attest is never simply allowed" $ do
        decide ENFORCE_ALL actions {onUnsupportedOrError = ALLOW} NewUser UNSUPPORTED @?= DecisionFallback
        decide ENFORCE_ALL actions {onUnsupportedOrError = ALLOW} NewUser ERROR @?= DecisionAllow
    ]
  where
    actions = PolicyActions {onInvalid = SILENT_DROP, onMissing = REJECT, onUnsupportedOrError = FALLBACK}

canonicalTests :: TestTree
canonicalTests =
  testGroup
    "Canonical request"
    [ testCase "mobile" $ do
        canonicalText mobile "abc" @?= "v1|NAMMA_YATRI|+91|9876543210|abc"
        hex (requestDigest mobile "abc") @?= "d987750fef416289f30b758697b0001797721ead76ce398ab5370def5e9696b0"
        androidRequestHash mobile "abc" @?= "2Yd1D-9BYonzC3WGl7AAF5dyHq12zjmKtTcN716WlrA",
      testCase "email" $
        androidRequestHash (EmailAuthRequest "NAMMA_YATRI" "driver@example.com") "abc" @?= "c8h45LmzNaCHiRhnDIK49n12uxraX6iiiF-z2WLgTM8",
      testCase "resend" $
        androidRequestHash (ResendRequest "auth-1") "abc" @?= "N89ibHmIMm2a-brGi8y0Ygz5bbkSitbQvjn-XdyZn2c",
      testCase "base64 decoding accepts both alphabets, padded or not" $ do
        decodeBase64Lenient "q6urq6urq6urq6urq6urq6urq6urq6urq6urq6urq6s" @?= Just (BS.replicate 32 0xAB)
        decodeBase64Lenient "q6urq6urq6urq6urq6urq6urq6urq6urq6urq6urq6s=" @?= Just (BS.replicate 32 0xAB)
        decodeBase64Lenient "-_8" @?= Just (BS.pack [0xFB, 0xFF])
        decodeBase64Lenient "+/8=" @?= Just (BS.pack [0xFB, 0xFF])
        decodeBase64Lenient "" @?= Nothing
        decodeBase64Lenient "not base64!" @?= Nothing
    ]
  where
    mobile = MobileAuthRequest "NAMMA_YATRI" "+91" "9876543210"
    hex = TE.decodeUtf8 . B16.encode

evidenceTests :: TestTree
evidenceTests =
  testGroup
    "Attestation headers"
    [ testCase "no headers is missing evidence" $ parseEvidence emptyHeaders @?= NoEvidence,
      testCase "blank headers count as absent" $ parseEvidence (headers (Just "  ") Nothing (Just "") Nothing Nothing Nothing) @?= NoEvidence,
      testCase "Android" $
        parseEvidence (headers (Just "ANDROID_PLAY_INTEGRITY") (Just "c") (Just "t") Nothing Nothing Nothing) @?= AndroidEvidence "c" "t",
      testCase "Android without a token" $
        parseEvidence (headers (Just "ANDROID_PLAY_INTEGRITY") (Just "c") Nothing Nothing Nothing Nothing) @?= MalformedEvidence (Just ANDROID_PLAY_INTEGRITY) HEADERS_INCOMPLETE,
      testCase "iOS" $
        parseEvidence (headers (Just "IOS_APP_ATTEST") (Just "c") Nothing (Just "k") (Just "a") Nothing) @?= IosEvidence "c" "k" "a",
      testCase "iOS without an assertion" $
        parseEvidence (headers (Just "IOS_APP_ATTEST") (Just "c") Nothing (Just "k") Nothing Nothing) @?= MalformedEvidence (Just IOS_APP_ATTEST) HEADERS_INCOMPLETE,
      testCase "unknown platform" $
        parseEvidence (headers (Just "WEB") (Just "c") (Just "t") Nothing Nothing Nothing) @?= MalformedEvidence Nothing UNKNOWN_PLATFORM,
      testCase "a token without a platform" $
        parseEvidence (headers Nothing Nothing (Just "t") Nothing Nothing Nothing) @?= MalformedEvidence Nothing HEADERS_INCOMPLETE,
      testCase "the client reports it cannot attest" $
        parseEvidence (headers (Just "IOS_APP_ATTEST") Nothing Nothing Nothing Nothing (Just "UNSUPPORTED_DEVICE")) @?= ClientUnsupported (Just IOS_APP_ATTEST) "UNSUPPORTED_DEVICE"
    ]
  where
    headers platform challengeId token keyId assertion clientError = AttestationHeaders platform challengeId token keyId assertion clientError Nothing

playIntegrityTests :: TestTree
playIntegrityTests =
  testGroup
    "Play Integrity verdict"
    [ testCase "a passing verdict has no reasons" $ check validPayload @?= [],
      testCase "Google's int64-as-string fields decode" $ do
        let raw = "{\"requestDetails\":{\"requestPackageName\":\"in.juspay.nammayatri\",\"requestHash\":\"h\",\"timestampMillis\":\"1794931200000\"},\"appIntegrity\":{\"appRecognitionVerdict\":\"PLAY_RECOGNIZED\",\"versionCode\":\"42\"}}"
        fmap (fmap PI.timestampMillis . PI.requestDetails) (A.decode raw) @?= Just (Just (Just (PI.FlexibleInt 1794931200000))),
      testCase "another package" $ check (withRequest (\r -> r {PI.requestPackageName = Just "com.attacker"})) @?= [PACKAGE_MISMATCH, CERT_MISMATCH],
      testCase "another request" $ check (withRequest (\r -> r {PI.requestHash = Just "other"})) @?= [REQUEST_HASH_MISMATCH],
      testCase "a stale token" $ check (withRequest (\r -> r {PI.timestampMillis = Just (millis (addUTCTime (-3600) now))})) @?= [TOKEN_EXPIRED],
      testCase "a repackaged app" $ check (withApp (\a -> a {PI.appRecognitionVerdict = Just "UNRECOGNIZED_VERSION"})) @?= [APP_NOT_RECOGNIZED],
      testCase "a sideloaded build passes when Play recognition is not required" $
        PI.verifyVerdict checks {PI.requirePlayRecognizedApp = False} (withApp (\a -> a {PI.appRecognitionVerdict = Just "UNRECOGNIZED_VERSION"})) @?= [],
      testCase "an unevaluated app fails even when Play recognition is not required" $
        PI.verifyVerdict checks {PI.requirePlayRecognizedApp = False} (withApp (\a -> a {PI.appRecognitionVerdict = Just "UNEVALUATED"})) @?= [APP_NOT_RECOGNIZED],
      testCase "another signing certificate" $ check (withApp (\a -> a {PI.certificateSha256Digest = Just ["AAAA"]})) @?= [CERT_MISMATCH],
      testCase "the configured digest may be colon-separated hex" $
        PI.verifyVerdict checks {PI.allowedApps = [AndroidAppConfig packageName [T.intercalate ":" (replicate 32 "AB")]]} validPayload @?= [],
      testCase "a package listed twice matches on either entry's certificate" $ do
        let twice other = [AndroidAppConfig packageName [other], AndroidAppConfig packageName ["q6urq6urq6urq6urq6urq6urq6urq6urq6urq6urq6s"]]
        PI.verifyVerdict checks {PI.allowedApps = twice "AAAA"} validPayload @?= []
        PI.verifyVerdict checks {PI.allowedApps = [AndroidAppConfig packageName ["AAAA"], AndroidAppConfig packageName ["BBBB"]]} validPayload @?= [CERT_MISMATCH],
      testCase "basic integrity does not meet device integrity" $ check (withDevice ["MEETS_BASIC_INTEGRITY"]) @?= [DEVICE_INTEGRITY_LOW],
      testCase "basic integrity is enough when configured" $
        PI.verifyVerdict checks {PI.minDeviceVerdict = MEETS_BASIC_INTEGRITY} (withDevice ["MEETS_BASIC_INTEGRITY"]) @?= [],
      testCase "strong integrity meets device integrity" $ check (withDevice ["MEETS_STRONG_INTEGRITY"]) @?= [],
      testCase "an emulator" $ check (withDevice []) @?= [DEVICE_INTEGRITY_LOW],
      testCase "licensing is checked only when required" $ do
        let unlicensed = validPayload {PI.accountDetails = Just (PI.AccountDetails (Just "UNLICENSED"))}
        check unlicensed @?= []
        PI.verifyVerdict checks {PI.requireLicensedApp = True} unlicensed @?= [APP_UNLICENSED],
      testCase "risky apps are rejected when configured" $
        PI.verifyVerdict checks {PI.rejectedAppAccessRisks = ["UNKNOWN_CAPTURING"]} (withRisks ["KNOWN_INSTALLED", "UNKNOWN_CAPTURING"]) @?= [APP_ACCESS_RISK],
      testCase "recent device activity above the limit" $ do
        let busy = withActivity "LEVEL_4"
        check busy @?= []
        PI.verifyVerdict checks {PI.maxRecentDeviceActivityLevel = Just 2} busy @?= [DEVICE_ACTIVITY_HIGH]
        PI.verifyVerdict checks {PI.maxRecentDeviceActivityLevel = Just 2} (withActivity "UNEVALUATED") @?= [],
      testCase "an empty verdict fails every check" $
        length (check (PI.TokenPayload Nothing Nothing Nothing Nothing Nothing)) > 5 @? "expected many reasons"
    ]
  where
    packageName = "in.juspay.nammayatri"
    now = UTCTime (fromGregorian 2026 11 17) 36000
    millis = PI.FlexibleInt . round . (* 1000) . utcTimeToPOSIXSeconds
    checks =
      PI.VerdictChecks
        { allowedApps = [AndroidAppConfig packageName ["q6urq6urq6urq6urq6urq6urq6urq6urq6urq6urq6s"]],
          expectedRequestHash = "2Yd1D-9BYonzC3WGl7AAF5dyHq12zjmKtTcN716WlrA",
          now,
          tokenMaxAgeSeconds = 600,
          minDeviceVerdict = MEETS_DEVICE_INTEGRITY,
          requirePlayRecognizedApp = True,
          requireLicensedApp = False,
          maxRecentDeviceActivityLevel = Nothing,
          rejectedAppAccessRisks = []
        }
    check = PI.verifyVerdict checks
    validPayload =
      PI.TokenPayload
        { requestDetails = Just (PI.RequestDetails (Just packageName) (Just "2Yd1D-9BYonzC3WGl7AAF5dyHq12zjmKtTcN716WlrA") (Just (millis (addUTCTime (-30) now)))),
          appIntegrity = Just (PI.AppIntegrity (Just "PLAY_RECOGNIZED") (Just packageName) (Just ["q6urq6urq6urq6urq6urq6urq6urq6urq6urq6urq6s"]) (Just (PI.FlexibleInt 100))),
          deviceIntegrity = Just (PI.DeviceIntegrity (Just ["MEETS_BASIC_INTEGRITY", "MEETS_DEVICE_INTEGRITY"]) Nothing),
          accountDetails = Just (PI.AccountDetails (Just "LICENSED")),
          environmentDetails = Nothing
        }
    withRequest f = validPayload {PI.requestDetails = f <$> PI.requestDetails validPayload}
    withApp f = validPayload {PI.appIntegrity = f <$> PI.appIntegrity validPayload}
    withDevice verdicts = validPayload {PI.deviceIntegrity = Just (PI.DeviceIntegrity (Just verdicts) Nothing)}
    withActivity level = validPayload {PI.deviceIntegrity = Just (PI.DeviceIntegrity (Just ["MEETS_DEVICE_INTEGRITY"]) (Just (PI.RecentDeviceActivity (Just level))))}
    withRisks apps = validPayload {PI.environmentDetails = Just (PI.EnvironmentDetails (Just (PI.AppAccessRiskVerdict (Just apps))) Nothing)}

data Fixture = Fixture
  { rootPem :: T.Text,
    teamId :: T.Text,
    bundleId :: T.Text,
    keyChallenge :: T.Text,
    canonical :: T.Text,
    keyId :: T.Text,
    attestation :: T.Text,
    attestationNonZeroCounter :: T.Text,
    assertionCounter1 :: T.Text,
    assertionOtherRequest :: T.Text
  }
  deriving (Generic, A.FromJSON)

appAttestTests :: Fixture -> TestTree
appAttestTests fixture =
  testGroup
    "App Attest"
    [ testCase "Apple's root certificate parses" $
        isJust (AppAttest.trustAnchorsFromPem AppAttest.appleAppAttestationRootCaPem) @? "Apple root should parse",
      testCase "a valid attestation registers the key" $ do
        verified <- either (assertFailure . show) pure $ attest attestationCheck (raw fixture.attestation)
        verified.iosApp @?= devApp
        BS.length verified.publicKey @?= 65,
      testCase "another challenge" $
        reasonOf (attest attestationCheck {AppAttest.clientDataHash = keyAttestationDigest "other"} (raw fixture.attestation)) @?= Just NONCE_MISMATCH,
      testCase "another key id" $
        reasonOf (attest attestationCheck {AppAttest.keyId = BS.replicate 32 1} (raw fixture.attestation)) @?= Just KEY_ID_MISMATCH,
      testCase "an app that is not configured" $
        reasonOf (attest attestationCheck {AppAttest.allowedApps = [IosAppConfig fixture.teamId "com.other" DEVELOPMENT]} (raw fixture.attestation)) @?= Just APP_NOT_ALLOWED,
      testCase "a development key for a production app" $
        reasonOf (attest attestationCheck {AppAttest.allowedApps = [IosAppConfig fixture.teamId fixture.bundleId PRODUCTION]} (raw fixture.attestation)) @?= Just ENVIRONMENT_MISMATCH,
      testCase "a chain that does not lead to the trusted root" $ do
        let appleOnly = fromJust (AppAttest.trustAnchorsFromPem AppAttest.appleAppAttestationRootCaPem)
        reasonOf (attest attestationCheck {AppAttest.trustAnchors = appleOnly} (raw fixture.attestation)) @?= Just CERT_CHAIN_INVALID,
      testCase "an expired credential certificate" $
        reasonOf (attest attestationCheck {AppAttest.now = UTCTime (fromGregorian 2030 1 1) 0} (raw fixture.attestation)) @?= Just CERT_CHAIN_INVALID,
      testCase "a key that was already used" $
        reasonOf (attest attestationCheck (raw fixture.attestationNonZeroCounter)) @?= Just COUNTER_NOT_ZERO,
      testCase "garbage" $
        reasonOf (attest attestationCheck "not cbor") @?= Just ATTESTATION_MALFORMED,
      testCase "an assertion for this request" $ do
        publicKey <- registeredKey
        AppAttest.verifyAssertion (assertionCheck publicKey 0) (raw fixture.assertionCounter1) @?= Right 1,
      testCase "a replayed assertion" $ do
        publicKey <- registeredKey
        AppAttest.verifyAssertion (assertionCheck publicKey 1) (raw fixture.assertionCounter1) @?= Left COUNTER_NOT_INCREASING,
      testCase "an assertion made for another request" $ do
        publicKey <- registeredKey
        AppAttest.verifyAssertion (assertionCheck publicKey 0) (raw fixture.assertionOtherRequest) @?= Left SIGNATURE_INVALID,
      testCase "an assertion checked against another app" $ do
        publicKey <- registeredKey
        AppAttest.verifyAssertion (assertionCheck publicKey 0) {AppAttest.bundleId = "com.other"} (raw fixture.assertionCounter1) @?= Left RP_ID_MISMATCH
    ]
  where
    raw = fromJust . decodeBase64Lenient
    trustAnchors = fromJust (AppAttest.trustAnchorsFromPem fixture.rootPem)
    devApp = IosAppConfig fixture.teamId fixture.bundleId DEVELOPMENT
    attestationCheck =
      AppAttest.AttestationCheck
        { trustAnchors,
          now = UTCTime (fromGregorian 2026 11 18) 0,
          keyId = raw fixture.keyId,
          clientDataHash = keyAttestationDigest fixture.keyChallenge,
          allowedApps = [devApp]
        }
    attest = AppAttest.verifyAttestation
    reasonOf = either Just (const Nothing)
    registeredKey = either (assertFailure . show) (pure . (.publicKey)) $ attest attestationCheck (raw fixture.attestation)
    assertionCheck publicKey storedSignCount =
      AppAttest.AssertionCheck
        { publicKey,
          clientDataHash = sha256 (TE.encodeUtf8 fixture.canonical),
          teamId = fixture.teamId,
          bundleId = fixture.bundleId,
          storedSignCount
        }
