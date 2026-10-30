{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

-- | The driver's cancel reason survives every layer of the driver-app proxy on its way to rider-app.
module SharedCabProxyTests (tests) where

import "dynamic-offer-driver-app" API.Types.UI.SharedCab (CancelBookingReq (..))
import Data.Aeson (Value (..), decode, encode, object, (.=))
import "dynamic-offer-driver-app" Domain.Action.Internal.SharedCabCallNumber (callNumberAllowed)
import "dynamic-offer-driver-app" Domain.Action.UI.SharedCabBooking (BookingAction (..), actionReason)
import "dynamic-offer-driver-app" Domain.Action.UI.SharedCabCall (SharedCabMessageReq (..))
import qualified "dynamic-offer-driver-app" Domain.Types.Person as SP
import "beckn-spec" Domain.Types.SharedCabSession (RiderStatus (..))
import qualified "beckn-spec" Domain.Types.VehicleVariant as DV
import "dynamic-offer-driver-app" SharedLogic.SharedCab.RiderClient (BAPDriverReq (..), BAPMessageReq (..), RiderCallNumberResp (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude

forwarded :: BAPDriverReq -> Maybe Value
forwarded = decode . encode

tests :: TestTree
tests =
  testGroup
    "shared-cab driver proxy (cancel reason)"
    [ testCase "the app's body {reason} decodes" $ decode "{\"reason\":\"rider not at stop\"}" @?= Just (CancelBookingReq {reason = "rider not at stop"}),
      testCase "a body without a reason is rejected at the door" $ (decode "{}" :: Maybe CancelBookingReq) @?= Nothing,
      testCase "the cancel action carries its reason on" $ actionReason (Cancel "cab breakdown") @?= Just "cab breakdown",
      testCase "other actions carry none" $ map actionReason [BoardedWithoutCode, Dropped] @?= [Nothing, Nothing],
      testCase "the request to rider-app has the reason under \"reason\"" $
        forwarded (BAPDriverReq {driverId = "d1", vehicleNumber = "ML05A9999", reason = Just "cab breakdown"})
          @?= Just (object ["driverId" .= ("d1" :: String), "vehicleNumber" .= ("ML05A9999" :: String), "reason" .= ("cab breakdown" :: String)]),
      testCase "another action sends reason null (rider-app reads it as Nothing)" $
        forwarded (BAPDriverReq {driverId = "d1", vehicleNumber = "ML05A9999", reason = Nothing})
          @?= Just (object ["driverId" .= ("d1" :: String), "vehicleNumber" .= ("ML05A9999" :: String), "reason" .= Null]),
      -- rider-app's SessionView sends all four; a missing one 500s the driver's session view
      testCase "every rider status rider-app sends decodes" $
        map decode ["\"AT_STOP\"", "\"MINUTES_AWAY\"", "\"ARRIVING\"", "\"BOARDED\""] @?= map Just [AT_STOP, MINUTES_AWAY, ARRIVING, BOARDED],
      testCase "a status rider-app adds later decodes as the fallback instead of failing the session view" $
        map decode ["\"IN_TRANSIT\"", "\"\""] @?= map Just [UNKNOWN_STATUS, UNKNOWN_STATUS],
      testCase "rider-app's call-number answer decodes" $ decode "{\"riderNumber\":\"+919876543210\"}" @?= Just (RiderCallNumberResp {riderNumber = "+919876543210"}),
      testCase "the app's chat body {message} decodes" $ decode "{\"message\":\"on my way\"}" @?= Just (SharedCabMessageReq {message = "on my way"}),
      testCase "the message request to rider-app carries the driver, the plate and the text" $
        (decode . encode) (BAPMessageReq {driverId = "d1", vehicleNumber = "ML05A9999", message = "on my way"})
          @?= Just (object ["driverId" .= ("d1" :: String), "vehicleNumber" .= ("ML05A9999" :: String), "message" .= ("on my way" :: String)]),
      testCase "the call number is handed out only for a driver on a shared cab" $
        [ callNumberAllowed SP.DRIVER (Just DV.SHARED_CAB),
          callNumberAllowed SP.DRIVER (Just DV.SEDAN),
          callNumberAllowed SP.DRIVER Nothing,
          callNumberAllowed SP.FLEET_OWNER (Just DV.SHARED_CAB),
          callNumberAllowed SP.BUS_DRIVER (Just DV.SHARED_CAB)
        ]
          @?= [True, False, False, False, False]
    ]
