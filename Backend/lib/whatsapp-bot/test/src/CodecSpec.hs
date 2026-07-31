-- | Round-trip properties for every type that is persisted to Redis.
--
-- These exist because @Kernel.Storage.Hedis.get = safeGet@ and
-- @safeGet key = get' key (del key)@ — a decode failure DELETES the key. For
-- @StoredPerson@ (@wab:user:*@, written with no TTL) that is permanent data
-- loss. A persisted type whose decoder can fail is therefore a data-loss bug,
-- not an inconvenience.
module CodecSpec (codecSpec) where

import Data.Aeson (eitherDecodeStrict', encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Kernel.Prelude
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)
import WhatsappBot.Handles (RegisteredRide (..), StoredPerson (..))
import WhatsappBot.I18n.Types (SupportedLanguage (..))
import WhatsappBot.Types

roundTrip :: (ToJSON a, FromJSON a) => a -> Either String a
roundTrip = eitherDecodeStrict' . BSL.toStrict . encode

assertRoundTrips :: (ToJSON a, FromJSON a, Eq a, Show a) => String -> a -> IO ()
assertRoundTrips lbl x = case roundTrip x of
  Left err -> assertFailure (lbl <> ": decode failed: " <> err)
  Right y -> assertEqual (lbl <> ": round-trip changed the value") x y

codecSpec :: TestTree
codecSpec =
  testGroup
    "persisted codec round-trips"
    [ testCase "FlowContext (initial)" $ assertRoundTrips "initialContext" initialContext,
      testCase "FlowContext (fully populated)" $ assertRoundTrips "populated" populatedContext,
      -- The whole Session envelope is what Adapter/SessionStore.hs reads back
      -- through Redis.get, not just its FlowContext metadata.
      testCase "Session envelope" $
        assertRoundTrips
          "Session"
          Session
            { sessionId = "M:911111111111",
              source = "whatsapp",
              userId = "M:911111111111",
              createdAt = posixSecondsToUTCTime 1751000000,
              lastActiveAt = posixSecondsToUTCTime 1751000123,
              messageCount = 7,
              metadata = populatedContext
            },
      testCase "StoredPerson with a language" $
        assertRoundTrips "StoredPerson" (StoredPerson {personId = "p1", language = Just Kn}),
      testCase "StoredPerson without a language" $
        assertRoundTrips "StoredPerson" (StoredPerson {personId = "p1", language = Nothing}),
      -- RegisteredRide has a 10800s TTL; a decode failure deletes it and orphans
      -- a LIVE ride, letting the rider double-book. Same delete-on-failure path.
      testCase "RegisteredRide" $
        assertRoundTrips
          "RegisteredRide"
          RegisteredRide
            { bookingId = "b1",
              userKey = "whatsapp:M:911111111111",
              sessionUserId = "whatsapp:M:911111111111",
              toPhone = "911111111111",
              merchantLabel = "M",
              personId = "p1",
              rideType = Flexi,
              language = Just Kn,
              lastStage = Just "assigned",
              createdAt = posixSecondsToUTCTime 0
            },
      testCase "every SupportedLanguage round-trips" $
        forM_ [minBound .. maxBound :: SupportedLanguage] $ \l ->
          assertRoundTrips ("SupportedLanguage " <> show l) l,
      testCase "every FlowState round-trips" $
        forM_ allFlowStates $ \s ->
          assertRoundTrips ("FlowState " <> flowStateLabel s) s,
      -- Decoding is not enough: 14 of FlowContext's 15 fields are Maybe (all but
      -- @state@), and aeson's generic decoder turns an ABSENT key into Nothing.
      -- So a RENAMED or RETYPED field still "decodes" — silently, as data loss.
      -- Assert the decoded VALUE, which catches both.
      --
      -- Note the limit of this pin: aeson also IGNORES unknown keys, so DELETING
      -- a field cannot be caught here — the blob still decodes equal to the
      -- correspondingly-shortened expected value. Deletion needs a behavioural
      -- oracle (a fixture that reads the field back), not a codec one.
      testCase "a v1 session blob still decodes (pinned old shape)" $
        assertPinDecodesTo "pinned v1 FlowContext" pinnedV1PopulatedContext populatedContext,
      testCase "a v1 fresh-session blob still decodes (pinned old shape)" $
        assertPinDecodesTo "pinned v1 initial FlowContext" pinnedV1InitialContext initialContext
    ]

-- | A pinned on-the-wire blob must still decode, AND still mean the same thing.
assertPinDecodesTo :: String -> BS.ByteString -> FlowContext -> IO ()
assertPinDecodesTo lbl blob expected =
  case eitherDecodeStrict' blob :: Either String FlowContext of
    Left err -> assertFailure (lbl <> " no longer decodes: " <> err)
    Right got ->
      assertEqual
        (lbl <> " decodes to a DIFFERENT value — a persisted field was renamed, retyped or dropped, which in production means safeGet deletes the key")
        expected
        got

-- | 'FlowState' has no Enum/Bounded instance, so the list is spelled out. The
-- total 'flowStateLabel' below makes a NEW constructor a build error
-- (-Wincomplete-patterns + -Werror), prompting whoever adds one to list it here.
allFlowStates :: [FlowState]
allFlowStates =
  [ Idle,
    AwaitingPickup,
    ConfirmingPickup,
    FlexiSearching,
    AwaitingRegularDrop,
    ConfirmingRegularDrop,
    ConfirmingRegularFare,
    RegularSearching,
    Tracking,
    ConfirmingSos,
    ConfirmingMarkSafe,
    ChoosingLanguage
  ]

-- | Total match over 'FlowState', so adding a constructor without adding it to
-- 'allFlowStates' fails the build here instead of leaving a state untested.
flowStateLabel :: FlowState -> String
flowStateLabel = \case
  Idle -> "Idle"
  AwaitingPickup -> "AwaitingPickup"
  ConfirmingPickup -> "ConfirmingPickup"
  FlexiSearching -> "FlexiSearching"
  AwaitingRegularDrop -> "AwaitingRegularDrop"
  ConfirmingRegularDrop -> "ConfirmingRegularDrop"
  ConfirmingRegularFare -> "ConfirmingRegularFare"
  RegularSearching -> "RegularSearching"
  Tracking -> "Tracking"
  ConfirmingSos -> "ConfirmingSos"
  ConfirmingMarkSafe -> "ConfirmingMarkSafe"
  ChoosingLanguage -> "ChoosingLanguage"

-- | EVERY field populated (no record update on 'initialContext', and no
-- 'Nothing' anywhere), so a field that silently fails to encode or decode
-- cannot hide behind a 'Nothing'. Spelled as a full construction on purpose:
-- adding a field to 'FlowContext' without populating it here is a -Werror
-- build failure (-Wmissing-fields), which is exactly the alarm we want.
populatedContext :: FlowContext
populatedContext =
  FlowContext
    { state = Tracking,
      personId = Just "person-1",
      savedLocations = Just [populatedSavedLocation],
      origin = Just (populatedPlace "place-origin"),
      destination = Just (populatedPlace "place-destination"),
      activeBookingId = Just "booking-1",
      selectStartedAt = Just (posixSecondsToUTCTime 1751000000),
      cancelRequested = Just True,
      sosId = Just "sos-1",
      language = Just Kn,
      pendingAction = Just PendingBook,
      rideType = Just Flexi,
      flexiBookingId = Just "booking-1",
      regularEstimateId = Just "est-1",
      regularFare = Just 123.5
    }

populatedPlace :: Text -> BotPlace
populatedPlace pid =
  BotPlace
    { lat = 13.3379,
      lon = 77.1173,
      placeId = pid,
      address =
        BotAddress
          { area = Just "Tumkur Bus Stand",
            building = Just "Block A",
            city = Just "Tumakuru",
            country = Just "India",
            state = Just "Karnataka",
            street = Just "BH Road"
          }
    }

populatedSavedLocation :: BotSavedLocation
populatedSavedLocation =
  BotSavedLocation
    { tag = "Home",
      lat = 13.3410,
      lon = 77.1010,
      area = Just "Sira Gate",
      building = Just "Block B",
      city = Just "Tumakuru",
      country = Just "India",
      state = Just "Karnataka",
      placeId = Just "mock-home"
    }

-- | A REAL FlowContext blob as persisted by the current code. Pin it so that a
-- future schema change which breaks old in-flight sessions fails HERE, loudly,
-- instead of in production where safeGet deletes the key.
--
-- Regenerate deliberately (never casually) — and never by hand-editing the
-- literal below; it must come out of actually running the code:
--
-- > nix develop .#backend --command bash -c \
-- >   'cd Backend && cabal repl whatsapp-bot:test:whatsapp-bot-tests -v0' <<'EOF'
-- > :load CodecSpec
-- > :module *CodecSpec
-- > import Data.Aeson (encode)
-- > import qualified Data.ByteString.Lazy.Char8 as C
-- > C.putStrLn (encode populatedContext)
-- > C.putStrLn (encode initialContext)
-- > EOF
--
-- (@:module *CodecSpec@ is what exposes these unexported values. Note the output
-- is a JSON string that must be re-escaped as a Haskell literal.)
pinnedV1PopulatedContext :: BS.ByteString
pinnedV1PopulatedContext =
  "{\"activeBookingId\":\"booking-1\",\"cancelRequested\":true,\"destination\":{\"address\":{\"area\":\"Tumkur Bus Stand\",\"building\":\"Block A\",\"city\":\"Tumakuru\",\"country\":\"India\",\"state\":\"Karnataka\",\"street\":\"BH Road\"},\"lat\":13.3379,\"lon\":77.1173,\"placeId\":\"place-destination\"},\"destinationOptions\":[{\"description\":\"Tumkur Bus Stand\",\"placeId\":\"place-destination\"}],\"flexiBookingId\":\"booking-1\",\"flexiQuoteId\":\"flexi-quote-1\",\"flexiSearchId\":\"flexi-search-1\",\"language\":\"kn\",\"origin\":{\"address\":{\"area\":\"Tumkur Bus Stand\",\"building\":\"Block A\",\"city\":\"Tumakuru\",\"country\":\"India\",\"state\":\"Karnataka\",\"street\":\"BH Road\"},\"lat\":13.3379,\"lon\":77.1173,\"placeId\":\"place-origin\"},\"pendingAction\":\"PendingBook\",\"personId\":\"person-1\",\"phone\":\"911111111111\",\"regularEstimateId\":\"est-1\",\"regularFare\":123.5,\"regularSearchId\":\"regular-search-1\",\"rideType\":\"Flexi\",\"savedLocations\":[{\"area\":\"Sira Gate\",\"building\":\"Block B\",\"city\":\"Tumakuru\",\"country\":\"India\",\"lat\":13.341,\"lon\":77.101,\"placeId\":\"mock-home\",\"state\":\"Karnataka\",\"tag\":\"Home\"}],\"selectStartedAt\":\"2025-06-27T04:53:20Z\",\"sosId\":\"sos-1\",\"state\":\"Tracking\"}"

-- | The same pin for a freshly created session (every optional field null).
pinnedV1InitialContext :: BS.ByteString
pinnedV1InitialContext =
  "{\"activeBookingId\":null,\"cancelRequested\":null,\"destination\":null,\"destinationOptions\":null,\"flexiBookingId\":null,\"flexiQuoteId\":null,\"flexiSearchId\":null,\"language\":null,\"origin\":null,\"pendingAction\":null,\"personId\":null,\"phone\":null,\"regularEstimateId\":null,\"regularFare\":null,\"regularSearchId\":null,\"rideType\":null,\"savedLocations\":null,\"selectStartedAt\":null,\"sosId\":null,\"state\":\"Idle\"}"
