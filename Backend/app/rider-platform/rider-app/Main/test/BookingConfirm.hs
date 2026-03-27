module BookingConfirm (bookingConfirmTests) where

import Data.List (sort)
import qualified Data.Text as T
import Data.Time.Clock (addUTCTime)
import Domain.Action.UI.Estimate (isCancelled)
import Domain.Types.BookingStatus
import qualified Domain.Types.EstimateStatus as ES
import Domain.Types.Extra.Booking (activeBookingStatus, activeScheduledBookingStatus, terminalBookingStatus)
import qualified Domain.Types.Extra.MerchantPaymentMethod as DMPM
import Kernel.Prelude
import Test.Tasty
import Test.Tasty.HUnit

bookingConfirmTests :: TestTree
bookingConfirmTests =
  testGroup
    "Booking Confirmation Flow"
    [ estimateStatusTests,
      bookingStatusTests,
      quoteExpiryTests,
      paymentValidationTests,
      scheduledRideTests
    ]

-- ==========================================
-- Estimate Status Tests (isCancelled)
-- Edge case: expired/cancelled estimate
-- ==========================================

allEstimateStatuses :: [ES.EstimateStatus]
allEstimateStatuses =
  [ ES.NEW,
    ES.DRIVER_QUOTE_REQUESTED,
    ES.CANCELLED,
    ES.GOT_DRIVER_QUOTE,
    ES.DRIVER_QUOTE_CANCELLED,
    ES.RIDE_SEARCH_EXPIRED,
    ES.COMPLETED
  ]

estimateStatusTests :: TestTree
estimateStatusTests =
  testGroup
    "EstimateStatus - isCancelled"
    [ testCase "CANCELLED is cancelled" $
        assertBool "should be True" (isCancelled ES.CANCELLED),
      testCase "DRIVER_QUOTE_CANCELLED is cancelled" $
        assertBool "should be True" (isCancelled ES.DRIVER_QUOTE_CANCELLED),
      testCase "NEW is not cancelled" $
        assertBool "should be False" (not $ isCancelled ES.NEW),
      testCase "DRIVER_QUOTE_REQUESTED is not cancelled" $
        assertBool "should be False" (not $ isCancelled ES.DRIVER_QUOTE_REQUESTED),
      testCase "GOT_DRIVER_QUOTE is not cancelled" $
        assertBool "should be False" (not $ isCancelled ES.GOT_DRIVER_QUOTE),
      testCase "RIDE_SEARCH_EXPIRED is not cancelled - expiry differs from cancellation" $
        assertBool "should be False" (not $ isCancelled ES.RIDE_SEARCH_EXPIRED),
      testCase "COMPLETED is not cancelled" $
        assertBool "should be False" (not $ isCancelled ES.COMPLETED),
      testCase "exactly 2 of 7 statuses are cancelled" $
        assertEqual "" 2 (length $ filter isCancelled allEstimateStatuses)
    ]

-- ==========================================
-- Booking Status Classification Tests
-- Edge case: cancelled ride, state transitions
-- ==========================================

allBookingStatuses :: [BookingStatus]
allBookingStatuses = [NEW, CONFIRMED, AWAITING_REASSIGNMENT, REALLOCATED, COMPLETED, CANCELLED, TRIP_ASSIGNED]

bookingStatusTests :: TestTree
bookingStatusTests =
  testGroup
    "BookingStatus Classification"
    [ testCase "active statuses are correct" $
        assertEqual "" (sort [NEW, CONFIRMED, AWAITING_REASSIGNMENT, TRIP_ASSIGNED]) (sort activeBookingStatus),
      testCase "terminal statuses are correct" $
        assertEqual "" (sort [COMPLETED, CANCELLED, REALLOCATED]) (sort terminalBookingStatus),
      testCase "active scheduled statuses are correct" $
        assertEqual "" (sort [AWAITING_REASSIGNMENT, TRIP_ASSIGNED]) (sort activeScheduledBookingStatus),
      testCase "every status is either active or terminal (exhaustiveness)" $
        forM_ allBookingStatuses $ \s ->
          assertBool
            (show s <> " should be in active or terminal")
            (s `elem` activeBookingStatus || s `elem` terminalBookingStatus),
      testCase "no status is both active and terminal (exclusivity)" $
        forM_ allBookingStatuses $ \s ->
          assertBool
            (show s <> " should not be in both active and terminal")
            (not (s `elem` activeBookingStatus && s `elem` terminalBookingStatus)),
      testCase "active scheduled statuses are subset of active" $
        forM_ activeScheduledBookingStatus $ \s ->
          assertBool (show s <> " should be in active") (s `elem` activeBookingStatus),
      testCase "NEW is the initial booking state after confirm" $
        assertBool "NEW should be active" (NEW `elem` activeBookingStatus),
      testCase "CANCELLED is a terminal state" $
        assertBool "CANCELLED should be terminal" (CANCELLED `elem` terminalBookingStatus),
      testCase "COMPLETED is a terminal state" $
        assertBool "COMPLETED should be terminal" (COMPLETED `elem` terminalBookingStatus),
      testCase "REALLOCATED is terminal, not active" $ do
        assertBool "REALLOCATED should be terminal" (REALLOCATED `elem` terminalBookingStatus)
        assertBool "REALLOCATED should not be active" (REALLOCATED `notElem` activeBookingStatus)
    ]

-- ==========================================
-- Quote Expiry Tests
-- Edge case: expired estimate, boundary conditions
-- ==========================================

-- Mirrors SharedLogic.Confirm line 147:
--   when (quote.validTill < now) $ throwError (InvalidRequest $ "Quote expired " <> show quote.id)
isQuoteExpired :: UTCTime -> UTCTime -> Bool
isQuoteExpired validTill now = validTill < now

mkTime :: String -> UTCTime
mkTime str = fromMaybe (error $ "Invalid time: " <> T.pack str) (readMaybe str)

quoteExpiryTests :: TestTree
quoteExpiryTests =
  testGroup
    "Quote Expiry Validation"
    [ testCase "expired: validTill is before now" $ do
        let now = mkTime "2025-03-15 12:00:00 UTC"
            validTill = mkTime "2025-03-15 11:59:00 UTC"
        assertBool "should be expired" (isQuoteExpired validTill now),
      testCase "not expired: validTill is after now" $ do
        let now = mkTime "2025-03-15 12:00:00 UTC"
            validTill = mkTime "2025-03-15 12:01:00 UTC"
        assertBool "should not be expired" (not $ isQuoteExpired validTill now),
      testCase "not expired: validTill equals now (strict less-than)" $ do
        let now = mkTime "2025-03-15 12:00:00 UTC"
        assertBool "equal times: should NOT be expired" (not $ isQuoteExpired now now),
      testCase "expired: 1 second past validTill" $ do
        let validTill = mkTime "2025-03-15 12:00:00 UTC"
            now = addUTCTime 1 validTill
        assertBool "should be expired" (isQuoteExpired validTill now),
      testCase "not expired: validTill is 24 hours in the future" $ do
        let now = mkTime "2025-03-15 12:00:00 UTC"
            validTill = addUTCTime 86400 now
        assertBool "should not be expired" (not $ isQuoteExpired validTill now),
      testCase "expired: validTill was 24 hours ago" $ do
        let now = mkTime "2025-03-15 12:00:00 UTC"
            validTill = addUTCTime (-86400) now
        assertBool "should be expired" (isQuoteExpired validTill now)
    ]

-- ==========================================
-- Payment Validation Tests
-- Edge case: invalid payment state
-- ==========================================

-- Mirrors SharedLogic.Confirm lines 166-167:
--   when (merchant.onlinePayment && paymentInstrument `notElem` [Just DMPM.Cash, Just DMPM.BoothOnline]) $ do
--     when (isNothing paymentMethodId) $ throwError PaymentMethodRequired
requiresPaymentMethodId :: Bool -> Maybe DMPM.PaymentInstrument -> Bool
requiresPaymentMethodId onlinePayment instrument =
  onlinePayment && instrument `notElem` [Just DMPM.Cash, Just DMPM.BoothOnline]

paymentValidationTests :: TestTree
paymentValidationTests =
  testGroup
    "Payment Validation"
    [ testGroup
        "Online payment enabled"
        [ testCase "Card requires payment method" $
            assertBool "" (requiresPaymentMethodId True (Just (DMPM.Card DMPM.DefaultCardType))),
          testCase "UPI requires payment method" $
            assertBool "" (requiresPaymentMethodId True (Just DMPM.UPI)),
          testCase "NetBanking requires payment method" $
            assertBool "" (requiresPaymentMethodId True (Just DMPM.NetBanking)),
          testCase "Wallet requires payment method" $
            assertBool "" (requiresPaymentMethodId True (Just (DMPM.Wallet DMPM.DefaultWalletType))),
          testCase "No instrument specified requires payment method" $
            assertBool "" (requiresPaymentMethodId True Nothing),
          testCase "Cash does NOT require payment method" $
            assertBool "" (not $ requiresPaymentMethodId True (Just DMPM.Cash)),
          testCase "BoothOnline does NOT require payment method" $
            assertBool "" (not $ requiresPaymentMethodId True (Just DMPM.BoothOnline))
        ],
      testGroup
        "Online payment disabled"
        [ testCase "No instrument - no requirement" $
            assertBool "" (not $ requiresPaymentMethodId False Nothing),
          testCase "Cash - no requirement" $
            assertBool "" (not $ requiresPaymentMethodId False (Just DMPM.Cash)),
          testCase "Card - no requirement when online payment off" $
            assertBool "" (not $ requiresPaymentMethodId False (Just (DMPM.Card DMPM.DefaultCardType))),
          testCase "UPI - no requirement when online payment off" $
            assertBool "" (not $ requiresPaymentMethodId False (Just DMPM.UPI)),
          testCase "BoothOnline - no requirement when online payment off" $
            assertBool "" (not $ requiresPaymentMethodId False (Just DMPM.BoothOnline))
        ]
    ]

-- ==========================================
-- Scheduled Ride Tests
-- Edge case: boundary conditions on buffer time
-- ==========================================

-- Mirrors SharedLogic.Confirm line 176:
--   let isScheduled = (maybe False not searchRequest.isMultimodalSearch)
--                     && merchant.scheduleRideBufferTime `addUTCTime` now < searchRequest.startTime
isScheduledRide :: Maybe Bool -> NominalDiffTime -> UTCTime -> UTCTime -> Bool
isScheduledRide isMultimodalSearch bufferTime now startTime =
  maybe False not isMultimodalSearch && addUTCTime bufferTime now < startTime

scheduledRideTests :: TestTree
scheduledRideTests =
  let now = mkTime "2025-03-15 12:00:00 UTC"
      bufferTime = 1800 -- 30 minutes
   in testGroup
        "Scheduled Ride Determination"
        [ testCase "not scheduled: Nothing isMultimodalSearch defaults to False via maybe False not" $
            assertBool "" (not $ isScheduledRide Nothing bufferTime now (addUTCTime 7200 now)),
          testCase "scheduled: isMultimodalSearch is Just False" $
            assertBool "" (isScheduledRide (Just False) bufferTime now (addUTCTime 7200 now)),
          testCase "not scheduled: isMultimodalSearch is Just True" $
            assertBool "" (not $ isScheduledRide (Just True) bufferTime now (addUTCTime 7200 now)),
          testCase "not scheduled: start time within buffer window" $
            assertBool "" (not $ isScheduledRide Nothing bufferTime now (addUTCTime 900 now)),
          testCase "not scheduled: start time exactly at buffer boundary" $
            assertBool "" (not $ isScheduledRide Nothing bufferTime now (addUTCTime bufferTime now)),
          testCase "not scheduled: Nothing isMultimodalSearch even 1 second past buffer" $
            assertBool "" (not $ isScheduledRide Nothing bufferTime now (addUTCTime (bufferTime + 1) now)),
          testCase "not scheduled: start time in the past" $
            assertBool "" (not $ isScheduledRide Nothing bufferTime now (addUTCTime (-3600) now)),
          testCase "not scheduled: zero buffer but start equals now" $
            assertBool "" (not $ isScheduledRide Nothing 0 now now),
          testCase "not scheduled: Nothing isMultimodalSearch even with zero buffer" $
            assertBool "" (not $ isScheduledRide Nothing 0 now (addUTCTime 1 now))
        ]
