{-# LANGUAGE TypeApplications #-}

module API.Quote.Handler where

import qualified API.Error as Error
import qualified API.Fixtures as Fixtures
import App.Types
import Beckn.Prelude
import Beckn.Utils.Common hiding (withFlowHandlerAPI)
import qualified "fmd-wrapper" ExternalAPI.Dunzo.Types as API
import GHC.Float (double2Float)
import Tools.FlowHandling
import qualified "fmd-wrapper" Types.Common as Common

handler ::
  Maybe Common.Token ->
  Maybe Common.ClientId ->
  Maybe Double ->
  Maybe Double ->
  Maybe Double ->
  Maybe Double ->
  Maybe Text ->
  FlowHandler API.QuoteRes
handler mToken mClientId mPickupLat mPickupLng mDropLat mDropLng mCategoryId = withFlowHandlerAPI $ do
  Fixtures.verifyToken mToken mClientId
  pickupLat <- fromMaybeM (Error.validationFailed "Url Param 'pickup_lat' is invalid.") mPickupLat
  pickupLng <- fromMaybeM (Error.validationFailed "Url Param 'pickup_lng' is invalid.") mPickupLng
  dropLat <- fromMaybeM (Error.validationFailed "Url Param 'drop_lat' is invalid.") mDropLat
  dropLng <- fromMaybeM (Error.validationFailed "Url Param 'drop_lng' is invalid.") mDropLng
  categoryId <- fromMaybeM (Error.validationFailed "Url Param 'category_id' has invalid value.") mCategoryId
  unless (categoryId == "pickup_drop") $ throwError (Error.validationFailed "Url Param 'category_id' has invalid value.")
  let coords = (pickupLat, pickupLng, dropLat, dropLng)
  when (nearbyLocation coords) $ throwError (Error.validationFailed "Pickup and drop location cannot be same")
  unless (successfulSearch coords) $ throwError Error.differentCityError
  pure $ mkQuoteRes coords
  where
    nearbyLocation (pickupLat, pickupLng, dropLat, dropLng) =
      (pickupLat == dropLat) && (pickupLng == dropLng)
    successfulSearch (pickupLat, pickupLng, dropLat, dropLng) =
      pickupLat >= Fixtures.minLat
        && pickupLat <= Fixtures.maxLat
        && pickupLng >= Fixtures.minLng
        && pickupLng <= Fixtures.maxLng
        && dropLat >= Fixtures.minLat
        && dropLat <= Fixtures.maxLat
        && dropLng >= Fixtures.minLng
        && dropLng <= Fixtures.maxLng

mkQuoteRes :: (Double, Double, Double, Double) -> API.QuoteRes
mkQuoteRes (pickupLat, pickupLng, dropLat, dropLng) = do
  let arcDistance = sqrt $ (pickupLat - dropLat) ^ (2 :: Int) + (pickupLng - dropLng) ^ (2 :: Int)
  API.QuoteRes
    { category_id = "pickup_drop",
      distance = double2Float . ceil' 1 $ Fixtures.distanceCoefficient * arcDistance,
      eta = Fixtures.eta,
      estimated_price = double2Float . ceil' 0 $ Fixtures.priceCoefficient * arcDistance
    }

ceil' :: Integer -> Double -> Double
ceil' sg num = (fromIntegral @Integer . ceiling $ num * f) / f
  where
    f = 10 ^ sg
