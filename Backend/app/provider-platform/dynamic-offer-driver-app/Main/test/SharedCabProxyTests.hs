{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

-- | The driver's cancel reason survives every layer of the driver-app proxy on its way to rider-app.
module SharedCabProxyTests (tests) where

import "dynamic-offer-driver-app" API.Types.UI.SharedCab (SharedCabBookingOperation (..), SharedCabBookingOperationReq (..), SharedCabMessageReq (..), SharedCabSessionOperation (..), SharedCabSessionOperationReq (..))
import Data.Aeson (Value (..), decode, encode, object, (.=))
import Data.Time (UTCTime (..), fromGregorian)
import "dynamic-offer-driver-app" Domain.Action.Internal.SharedCabCallNumber (callNumberAllowed)
import "dynamic-offer-driver-app" Domain.Action.UI.SharedCab (BookingAction (..), actionReason, bookingActionOf)
import qualified "dynamic-offer-driver-app" Domain.Types.Common as DCommon
import qualified "dynamic-offer-driver-app" Domain.Types.Person as SP
import "beckn-spec" Domain.Types.SharedCabSession (RiderStatus (..))
import qualified "dynamic-offer-driver-app" Domain.Types.VehicleTrip as DVT
import "mobility-core" Kernel.Types.Id (Id (..))
import "dynamic-offer-driver-app" SharedLogic.SharedCab.RiderClient (BAPBookingOperationReq (..), BAPMessageReq (..), BAPSessionOperationReq (..), RiderCallNumberResp (..))
import "dynamic-offer-driver-app" SharedLogic.SharedCab.Trip (RiderRun (..), TripAction (..), tripAction)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude

activeTrip :: DVT.VehicleTrip
activeTrip =
  DVT.VehicleTrip
    { DVT.id = Id "T1",
      DVT.driverId = Id "d1",
      DVT.vehicleNumber = "ML05A9999",
      DVT.capacity = Just 6,
      DVT.merchantId = Id "m",
      DVT.merchantOperatingCityId = Id "moc",
      DVT.integratedBppConfigId = Id "ibc",
      DVT.routeCode = "R1",
      DVT.status = DVT.ACTIVE,
      DVT.startedAt = t0,
      DVT.endedAt = Nothing,
      DVT.endReason = Nothing,
      DVT.walkupCount = 1,
      DVT.createdAt = t0,
      DVT.updatedAt = t0
    }
  where
    t0 = UTCTime (fromGregorian 2026 11 4) 0

forwarded :: BAPBookingOperationReq -> Maybe Value
forwarded = decode . encode

tests :: TestTree
tests =
  testGroup
    "shared-cab driver proxy (cancel reason)"
    [ testCase "the app's body {operation, reason} decodes" $
        decode "{\"operation\":\"CANCEL\",\"reason\":\"rider not at stop\"}" @?= Just (SharedCabBookingOperationReq {operation = CANCEL, reason = Just "rider not at stop"}),
      testCase "a cancel carries its reason into the action" $
        bookingActionOf (SharedCabBookingOperationReq {operation = CANCEL, reason = Just "cab breakdown"}) @?= Just (Cancel "cab breakdown"),
      testCase "a cancel without a reason is refused" $
        bookingActionOf (SharedCabBookingOperationReq {operation = CANCEL, reason = Nothing}) @?= Nothing,
      testCase "the other operations need no reason" $
        map (\op -> bookingActionOf (SharedCabBookingOperationReq {operation = op, reason = Nothing})) [ACCEPT, DECLINE, DROPPED, BOARDED_WITHOUT_CODE]
          @?= map Just [Accept, Decline, Dropped, BoardedWithoutCode],
      testCase "the cancel action carries its reason on" $ actionReason (Cancel "cab breakdown") @?= Just "cab breakdown",
      testCase "other actions carry none" $ map actionReason [BoardedWithoutCode, Dropped] @?= [Nothing, Nothing],
      testCase "the request to rider-app has the reason under \"reason\"" $
        forwarded (BAPBookingOperationReq {operation = CANCEL, driverId = "d1", vehicleNumber = "ML05A9999", reason = Just "cab breakdown"})
          @?= Just (object ["operation" .= ("CANCEL" :: String), "driverId" .= ("d1" :: String), "vehicleNumber" .= ("ML05A9999" :: String), "reason" .= ("cab breakdown" :: String)]),
      testCase "another action sends reason null (rider-app reads it as Nothing)" $
        forwarded (BAPBookingOperationReq {operation = DROPPED, driverId = "d1", vehicleNumber = "ML05A9999", reason = Nothing})
          @?= Just (object ["operation" .= ("DROPPED" :: String), "driverId" .= ("d1" :: String), "vehicleNumber" .= ("ML05A9999" :: String), "reason" .= Null]),
      -- rider-app's SessionView sends all four; a missing one 500s the driver's session view
      testCase "every rider status rider-app sends decodes" $
        map decode ["\"AT_STOP\"", "\"MINUTES_AWAY\"", "\"ARRIVING\"", "\"BOARDED\""] @?= map Just [AT_STOP, MINUTES_AWAY, ARRIVING, BOARDED],
      testCase "a status rider-app adds later decodes as the fallback instead of failing the session view" $
        map decode ["\"IN_TRANSIT\"", "\"\""] @?= map Just [UNKNOWN_STATUS, UNKNOWN_STATUS],
      testCase "the app's STILL_ON_ROUTE {operation, checkInId} decodes" $
        decode "{\"operation\":\"STILL_ON_ROUTE\",\"checkInId\":\"c1\"}"
          @?= Just (SharedCabSessionOperationReq {operation = STILL_ON_ROUTE, checkInId = Just "c1", version = Nothing, walkupCount = Nothing}),
      testCase "the STILL_ON_ROUTE request to rider-app carries the checkInId" $
        (decode . encode) (BAPSessionOperationReq {operation = STILL_ON_ROUTE, driverId = "d1", vehicleNumber = "ML05A9999", version = Nothing, walkupCount = Nothing, checkInId = Just "c1"})
          @?= Just (object ["operation" .= ("STILL_ON_ROUTE" :: String), "driverId" .= ("d1" :: String), "vehicleNumber" .= ("ML05A9999" :: String), "version" .= Null, "walkupCount" .= Null, "checkInId" .= ("c1" :: String)]),
      testCase "rider-app's call-number answer decodes" $ decode "{\"riderNumber\":\"+919876543210\"}" @?= Just (RiderCallNumberResp {riderNumber = "+919876543210"}),
      testCase "the app's chat body {message} decodes" $ decode "{\"message\":\"on my way\"}" @?= Just (SharedCabMessageReq {message = "on my way"}),
      testCase "the message request to rider-app carries the driver, the plate and the text" $
        (decode . encode) (BAPMessageReq {driverId = "d1", vehicleNumber = "ML05A9999", message = "on my way"})
          @?= Just (object ["driverId" .= ("d1" :: String), "vehicleNumber" .= ("ML05A9999" :: String), "message" .= ("on my way" :: String)]),
      testCase "the call number is handed out only for a driver on a shared cab" $
        [ callNumberAllowed SP.DRIVER [DCommon.SHARED_CAB],
          callNumberAllowed SP.DRIVER [DCommon.TAXI, DCommon.SHARED_CAB],
          callNumberAllowed SP.DRIVER [DCommon.SEDAN],
          callNumberAllowed SP.DRIVER [],
          callNumberAllowed SP.FLEET_OWNER [DCommon.SHARED_CAB],
          callNumberAllowed SP.BUS_DRIVER [DCommon.SHARED_CAB]
        ]
          @?= [True, True, False, False, False, False],
      testCase "the plate's trip row follows rider-app's session: same run, another run, or none" $
        let run tripId walkups = Just RiderRun {tripId, routeCode = "R2", walkupCount = walkups, routeEnds = Nothing}
            sync driver = tripAction (Id driver) DVT.END_ROUTE
         in [ sync "d1" (Just activeTrip) (run "T1" 1),
              sync "d1" (Just activeTrip) (run "T1" 3),
              sync "d1" (Just activeTrip) (run "T2" 0),
              sync "d2" (Just activeTrip) (run "T2" 0),
              sync "d1" Nothing (run "T2" 0),
              sync "d1" (Just activeTrip) Nothing,
              sync "d1" Nothing Nothing
            ]
              @?= [ TripUnchanged,
                    SetWalkups (Id "T1") 3,
                    OpenTrip (Just (Id "T1", DVT.ROUTE_CHANGED)) (RiderRun "T2" "R2" 0 Nothing),
                    OpenTrip (Just (Id "T1", DVT.SESSION_TIMEOUT)) (RiderRun "T2" "R2" 0 Nothing),
                    OpenTrip Nothing (RiderRun "T2" "R2" 0 Nothing),
                    CloseTrip (Id "T1") DVT.END_ROUTE,
                    TripUnchanged
                  ]
    ]
