module PublicTransport.Search where

import API.UI.Quote (OfferRes (PublicTransport))
import Beckn.Prelude hiding (Proxy)
import Beckn.Streaming.Kafka.Topic.PublicTransportQuoteList
import Beckn.Types.Flow (FlowR)
import Beckn.Types.Id
import Beckn.Utils.Time (threadDelaySec)
import Common
import qualified Data.Text as T
import Domain.Action.UI.QuoteConfirm
import qualified "public-transport-bap" Domain.Types.Booking as TB
import Domain.Types.PaymentTransaction (PaymentStatus (FAILED, PENDING, SUCCESS))
import qualified "app-backend" Domain.Types.SearchRequest as AppBE
import qualified "public-transport-bap" Environment as Bap
import HSpec
import Mobility.AppBackend.Fixtures
import Mobility.Fixtures.Routes
import PublicTransport.API
import PublicTransport.Common
import qualified "app-backend" Storage.Queries.SearchRequest as AppBE
import Utils

{-
 - WARNING! This test is highly dependent on how the public transport bpp mock is working.
 - See app/mocks/public-transport-bpp/src/MockData/* for details
 -}

waitABit :: MonadIO m => m ()
waitABit = threadDelay $ 8 * 1e5

spec :: Spec
spec = do
  describe "Testing public transport APIs" $
    it "Testing flow" $ do
      (quoteEkmAbc, quoteEkmEmb) <- testSearch
      testConfirm True quoteEkmAbc
      testConfirm False quoteEkmEmb

testConfirm :: Bool -> PublicTransportQuote -> IO ()
testConfirm shouldSucceed quote = do
  let confirmReq =
        QConfirmReq
          { quantity = 1,
            requestorName = "The glorious unknown integration tester"
          }
  bookingId <- Id . (.booking_id) <$> callPublicTransportBap (quoteConfirmClientM userToken (Id quote.id) confirmReq)
  booking <- pollDesc "expected a booking confirmed by BPP" $ do
    booking' <- callPublicTransportBap (bookingClientM userToken bookingId)
    booking'.ticketId `shouldSatisfy` isJust
    pure $ Just booking'
  booking.id.getId `shouldBe` bookingId.getId
  _ <- callPublicTransportBap $ triggerStatusClientM userToken bookingId
  booking2 <- callPublicTransportBap (bookingClientM userToken bookingId)
  booking2.id.getId `shouldBe` bookingId.getId
  assertBookingAwaitingPayment booking2
  threadDelaySec $ mockWaitTimeSeconds + 1
  booking3 <- callPublicTransportBap $ bookingClientM userToken bookingId
  if shouldSucceed
    then assertSuccessfulBooking booking3
    else assertFailedBooking booking3

runPublicTransportFlow :: Text -> FlowR Bap.AppEnv a -> IO a
runPublicTransportFlow tag = runFlow tag publicTransportBapEnv

assertBooking :: MonadIO m => TB.BookingStatus -> PaymentStatus -> TB.BookingAPIEntity -> m ()
assertBooking bookingStatus paymentStatus booking = do
  booking.status `shouldBe` bookingStatus
  (.status) <$> booking.paymentTxn `shouldBe` Just paymentStatus

assertBookingAwaitingPayment :: MonadIO m => TB.BookingAPIEntity -> m ()
assertBookingAwaitingPayment = assertBooking TB.AWAITING_PAYMENT PENDING

assertSuccessfulBooking :: MonadIO m => TB.BookingAPIEntity -> m ()
assertSuccessfulBooking = assertBooking TB.CONFIRMED SUCCESS

assertFailedBooking :: MonadIO m => TB.BookingAPIEntity -> m ()
assertFailedBooking = assertBooking TB.CANCELLED FAILED

testSearch :: IO (PublicTransportQuote, PublicTransportQuote)
testSearch = do
  searchId <- (.searchId) <$> callAppBackend (searchServices userToken defaultSearchReq (Just defaultVersion) (Just defaultVersion))
  searchId `shouldSatisfy` (\s -> T.length s.getId == 36)
  searchRequest <- pollDesc "Expected search request in the app backend database" $ findSearchBAP searchId
  searchRequest.id `shouldBe` searchId
  threadDelay $ 3 * kafkaConsumerTimeoutMilliseconds * 1000
  pollDesc "Expected EKM-ABC and EKM-EMB trips in the search results" $ do
    quotes <- (.quotes) <$> callAppBackend (getQuotes searchId userToken)
    ekmAbcQuote <- maybe (expectationFailure "No EKM-ABC trip found") pure $ findEkmAbcQuote quotes
    ekmEmbQuote <- maybe (expectationFailure "No EKM-EMB trip found") pure $ findEkmEmbQuote quotes
    pure $ Just (ekmAbcQuote, ekmEmbQuote)

findSearchBAP :: Id AppBE.SearchRequest -> IO (Maybe AppBE.SearchRequest)
findSearchBAP searchId = runAppFlow "" $ AppBE.findById searchId

findQuoteByStations :: Text -> Text -> [OfferRes] -> Maybe PublicTransportQuote
findQuoteByStations dep arriv = find pred_ . mapMaybe f
  where
    f (PublicTransport q) = Just q
    f _ = Nothing

    pred_ q =
      q.departureStation.stationCode == dep
        && q.arrivalStation.stationCode == arriv

findEkmAbcQuote :: [OfferRes] -> Maybe PublicTransportQuote
findEkmAbcQuote = findQuoteByStations "EKM" "ABC"

findEkmEmbQuote :: [OfferRes] -> Maybe PublicTransportQuote
findEkmEmbQuote = findQuoteByStations "EKM" "EMB"
