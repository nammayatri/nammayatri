module Flow.RideAPI.StartRide where

import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.Id
import EulerHS.Prelude
import qualified Fixtures
import qualified Product.RideAPI.Handlers.StartRide as StartRide
import Servant.Server as Serv (ServerError)
import Test.Hspec
import Test.Tasty
import Test.Tasty.HUnit
import Types.Error
import qualified Types.Storage.Person as Person
import qualified Types.Storage.Quote as Quote
import qualified Types.Storage.Ride as Ride
import qualified Types.Storage.SearchRequest as SearchRequest
import Utils.SilentLogger ()

handle :: StartRide.ServiceHandle IO
handle =
  StartRide.ServiceHandle
    { findPersonById = \_personid -> pure $ Just Fixtures.defaultDriver,
      findQuoteById = \quoteId ->
        pure $
          if quoteId == Id "2"
            then Just searchQuote
            else Nothing,
      findRideById = \rideId ->
        pure $
          if rideId == Id "1"
            then Just ride
            else Nothing,
      startRide = \_quoteIds -> pure (),
      notifyBAPRideStarted = \_quote _rideBooking _ride -> pure (),
      rateLimitStartRide = \_driverId _rideId -> pure ()
    }

ride :: Ride.Ride
ride =
  Fixtures.defaultRide
    { Ride.status = Ride.NEW,
      Ride.bookingId = "2",
      Ride.otp = "otp"
    }

searchQuote :: Quote.Quote
searchQuote =
  Fixtures.defaultQuote
    { Quote.id = "2",
      Quote.status = Quote.CONFIRMED
    }

searchRequest :: SearchRequest.SearchRequest
searchRequest =
  Fixtures.defaultSearchRequest
    { SearchRequest.id = "1"
    }

startRide :: TestTree
startRide =
  testGroup
    "Starting ride"
    [ successfulStartByDriver,
      failedStartRequestedByDriverNotAnOrderExecutor,
      failedStartRequestedNotByDriver,
      failedStartWhenQuoteStatusIsWrong,
      failedStartWithWrongOTP
    ]

runHandler :: StartRide.ServiceHandle IO -> Id Person.Person -> Id Ride.Ride -> Text -> IO APISuccess.APISuccess
runHandler = StartRide.startRideHandler

successfulStartByDriver :: TestTree
successfulStartByDriver =
  testCase "Start successfully if requested by driver executor" $ do
    runHandler handle "1" "1" "otp"
      `shouldReturn` APISuccess.Success

failedStartRequestedByDriverNotAnOrderExecutor :: TestTree
failedStartRequestedByDriverNotAnOrderExecutor = do
  testCase "Fail ride starting if requested by driver not an order executor" $ do
    runHandler handleCase "2" "1" "otp"
      `shouldThrow` (== NotAnExecutor)
  where
    handleCase =
      handle
        { StartRide.findPersonById = \personId ->
            pure $
              Just
                Fixtures.defaultDriver{id = "2"
                                      }
        }

failedStartRequestedNotByDriver :: TestTree
failedStartRequestedNotByDriver = do
  testCase "Fail ride starting if requested not by driver" $ do
    runHandler handleCase "1" "1" "otp"
      `shouldThrow` (== AccessDenied)
  where
    handleCase =
      handle
        { StartRide.findPersonById = \personId ->
            pure $
              Just
                Fixtures.defaultDriver{role = Person.ADMIN
                                      }
        }

failedStartWhenQuoteStatusIsWrong :: TestTree
failedStartWhenQuoteStatusIsWrong = do
  testCase "Fail ride starting if ride has wrong status" $ do
    runHandler handleCase "1" "1" "otp"
      `shouldThrow` (\(RideInvalidStatus _) -> True)
  where
    handleCase =
      handle
        { StartRide.findRideById = \rideId ->
            pure $
              Just
                ride{status = Ride.COMPLETED
                    }
        }

failedStartWithWrongOTP :: TestTree
failedStartWithWrongOTP = do
  testCase "Fail ride starting if OTP is wrong" $ do
    runHandler handle "1" "1" "otp2"
      `shouldThrow` (== IncorrectOTP)
