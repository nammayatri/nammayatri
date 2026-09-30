{-# LANGUAGE PackageImports #-}

module LocationFallbackTests (tests) where

import qualified Data.Aeson as A
import Data.Time (UTCTime (..), fromGregorian)
import Domain.Action.UI.Location (makeLocationAPIEntity)
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Location as DL
import qualified Domain.Types.LocationAddress as DLA
import qualified Domain.Types.LocationMapping as DLM
import Kernel.Prelude
import Kernel.Types.Distance (Distance (..), DistanceUnit (..), HighPrecDistance (..))
import Kernel.Types.Error.BaseError.HTTPError (IsHTTPError (..))
import Kernel.Types.Id
import SharedLogic.LocationFallback
import SharedLogic.LocationFallbackEnrich
import Test.Tasty
import Test.Tasty.HUnit
import "rider-app" Tools.Error (LocationMappingError (..))

t0 :: UTCTime
t0 = UTCTime (fromGregorian 2026 9 29) 0

emptyAddr :: DLA.LocationAddress
emptyAddr = DLA.LocationAddress Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

realLoc :: Text -> DL.Location
realLoc i = DL.Location {address = emptyAddr {DLA.area = Just "HSR"}, createdAt = t0, id = Id i, lat = 12.9, lon = 77.6, updatedAt = t0, merchantId = Nothing, merchantOperatingCityId = Nothing}

ph :: Text -> DL.Location
ph i = mkPlaceholderLocation (Id i) Nothing Nothing t0

bppLoc :: Double -> BppLocation
bppLoc x = BppLocation {lat = x, lon = x, street = Just "st", door = Nothing, city = Just "Bengaluru", state = Nothing, country = Nothing, building = Nothing, areaCode = Nothing, area = Just "Koramangala", instructions = Nothing, extras = Nothing}

parentInfo :: DRB.ParentSearchRequestLocationInfo
parentInfo = DRB.ParentSearchRequestLocationInfo {sourceLat = 1, sourceLon = 1, sourceAddress = emptyAddr, destLat = 2, destLon = 2, destAddress = emptyAddr}

pickupMapping :: Text -> Text -> DLM.LocationMapping
pickupMapping mid locId = DLM.LocationMapping {createdAt = t0, entityId = "b1", id = Id mid, locationId = Id locId, merchantId = Nothing, merchantOperatingCityId = Nothing, order = 0, tag = DLM.BOOKING, updatedAt = t0, version = "LATEST"}

dummyDistance :: Distance
dummyDistance = Distance (HighPrecDistance 0) Meter

tests :: TestTree
tests =
  testGroup
    "LocationFallback"
    [ testCase "placeholder is detected" $ isPlaceholderLocation (ph "a") @?= True,
      testCase "real location is not a placeholder" $ isPlaceholderLocation (realLoc "a") @?= False,
      testCase "address-less real location is not a placeholder" $
        isPlaceholderLocation ((realLoc "a") {DL.address = emptyAddr}) @?= False,
      testCase "resolveOne leaves real locations alone" $
        isNothing (resolveOne Nothing Nothing Pickup 0 (realLoc "a")) @?= True,
      testCase "resolveOne uses driver app for pickup and keeps id" $
        let res = BppBookingLocationsRes {from = bppLoc 12.97, to = Nothing, stops = []}
         in fmap (\(l, s) -> (l.id, l.lat, s)) (resolveOne Nothing (Just res) Pickup 0 (ph "p")) @?= Just (Id "p", 12.97, DriverApp),
      testCase "driver app wins over booking row" $
        (snd <$> resolveOne (Just parentInfo) (Just (BppBookingLocationsRes (bppLoc 5) Nothing [])) Pickup 0 (ph "p")) @?= Just DriverApp,
      testCase "booking row used when driver app has nothing" $
        fmap (\(l, s) -> (l.lat, s)) (resolveOne (Just parentInfo) Nothing Drop 0 (ph "d")) @?= Just (2, BookingRow),
      testCase "resolveOne falls back to placeholder" $
        (snd <$> resolveOne Nothing Nothing Drop 0 (ph "d")) @?= Just Placeholder,
      testCase "stop index beyond bpp stops stays placeholder" $
        let res = BppBookingLocationsRes {from = bppLoc 1, to = Nothing, stops = [bppLoc 2]}
         in (snd <$> resolveOne Nothing (Just res) Stop 3 (ph "s")) @?= Just Placeholder,
      testCase "patch only placeholders" $
        let details = DRB.OneWayDetails (DRB.OneWayBookingDetails {toLocation = realLoc "d", stops = [realLoc "s0", ph "s1"], distance = dummyDistance, isUpgradedToCab = Nothing})
            fixStop i l = if isPlaceholderLocation l then realLoc ("fixed" <> show i) else l
            patched = mapDetails identity (zipWith fixStop [0 :: Int ..]) details
         in map (.id) (snd (detailsLocations patched)) @?= [Id "s0", Id "fixed1"],
      testCase "placeholder api entity is unavailable" $
        (makeLocationAPIEntity (ph "a")).isUnavailable @?= Just True,
      testCase "real location without address is available" $
        (makeLocationAPIEntity ((realLoc "a") {DL.address = emptyAddr})).isUnavailable @?= Nothing,
      testCase "missing drop has a typed error code" $
        toErrorCode (ToLocationNotFound "x") @?= "TO_LOCATION_NOT_FOUND",
      testCase "decodes driver-app booking locations payload" $
        let payload = "{\"from\":{\"id\":\"L1\",\"lat\":12.9,\"lon\":77.6,\"area\":\"HSR\",\"fullAddress\":\"x\",\"street\":null},\"to\":null,\"stops\":[]}"
         in fmap (\r -> (r.from.lat, r.from.area, isNothing r.to)) (A.decode payload :: Maybe BppBookingLocationsRes) @?= Just (12.9, Just "HSR", True),
      testCase "initial pickup reuses resolved pickup when it is the same mapping" $
        initialPickupLookupId [pickupMapping "m1" "p"] (ph "p") @?= Nothing,
      testCase "initial pickup needs its own lookup when it differs" $
        initialPickupLookupId [(pickupMapping "m0" "old") {DLM.version = "v-1"}, pickupMapping "m1" "p"] (ph "p") @?= Just (Id "old"),
      testCase "initial pickup falls back to resolved pickup without mappings" $
        initialPickupLookupId [] (ph "p") @?= Nothing,
      testCase "rental keeps no drop" $
        map (.id) (fst (detailsLocations (DRB.RentalDetails (DRB.RentalBookingDetails {otpCode = Nothing, stopLocation = Nothing})))) @?= []
    ]
