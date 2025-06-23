module Domain.Action.UI.RentalsIntercityCache
  ( rentalsIntercityCache,
    FareCacheReq (..),
    FareCacheResp (..),
    module Reexport,
  )
where

import Data.List (minimumBy)
import qualified Domain.Action.UI.Serviceability as SVC
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as Person
import Domain.Types.RentalsIntercityCache as Reexport
import qualified Domain.Types.ServiceTierType as ServiceTierType
import qualified Domain.Types.Trip as DTC
import EulerHS.Prelude hiding (elem, minimumBy, null)
import Kernel.External.Maps.Types
import Kernel.Prelude hiding (map, minimumBy)
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.Beckn.City as City
import Kernel.Types.Error
import qualified Kernel.Types.Id as Id
import Kernel.Utils.Common
import qualified Lib.JourneyModule.Types as JL
import qualified Lib.Types.SpecialLocation as LSS
import qualified SharedLogic.CallBPPInternal as CallBPPInternal
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRC
import qualified Storage.Queries.Person as QPerson
import Tools.Error (RiderError (..))

rentalsIntercityCache ::
  JL.GetFareFlow m r =>
  Id.Id Person.Person ->
  Id.Id Merchant.Merchant ->
  FareCacheReq ->
  m FareCacheResp
rentalsIntercityCache personId merchantId req = do
  merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  merchantOperatingCity <- CQMOC.findById person.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantOperatingCityId- " <> show person.merchantOperatingCityId)
  checkForServiceable <- SVC.checkServiceability fetchOriginSuccessor (personId, merchantId) req.currentLatLong False False
  riderConfig <- QRC.findByMerchantOperatingCityId person.merchantOperatingCityId Nothing >>= fromMaybeM (RiderConfigDoesNotExist person.merchantOperatingCityId.getId)
  let checkForSpecialLocation = isJust checkForServiceable.specialLocation
      specialLocationId = fmap (.id) checkForServiceable.specialLocation
      cityCenterLatLong = Just $ LatLong {lat = merchantOperatingCity.lat, lon = merchantOperatingCity.long}
      excludedVehicleVariants = riderConfig.excludedVehicleVariants
      rentalsConfig = riderConfig.fareCacheRentalsConfig
      interCitySearchLocations = riderConfig.fareCacheInterCitySearchLocations
      driverOfferMerchantOperatingCityId = merchantOperatingCity.driverOfferMerchantOperatingCityId
  interCityResp <-
    case checkForSpecialLocation of
      True -> do
        Redis.safeGet (mkSpecialLocationRedisKey specialLocationId ":interCity") >>= \case
          Just resp -> pure resp
          Nothing -> do
            interCityFareResp <- buildMininumIntercityFareArray req (Just req.currentLatLong) merchant merchantOperatingCity.city excludedVehicleVariants interCitySearchLocations driverOfferMerchantOperatingCityId
            Redis.setExp (mkSpecialLocationRedisKey specialLocationId ":interCity") interCityFareResp 3600 -- 1 hour
            pure interCityFareResp
      False -> do
        Redis.safeGet (mkLocationRedisKey req.currentCity ":interCity") >>= \case
          Just resp -> pure resp
          Nothing -> do
            interCityFareResp <- buildMininumIntercityFareArray req cityCenterLatLong merchant merchantOperatingCity.city excludedVehicleVariants interCitySearchLocations driverOfferMerchantOperatingCityId
            Redis.setExp (mkLocationRedisKey req.currentCity ":interCity") interCityFareResp 3600 -- 1 hour
            pure interCityFareResp
  rentalsResp <- case checkForSpecialLocation of
    True -> do
      Redis.safeGet (mkSpecialLocationRedisKey specialLocationId ":rentals") >>= \case
        Just resp -> pure resp
        Nothing -> do
          rentalsAPIResp <- buildMininumRentalsFareArray req (Just req.currentLatLong) merchant merchantOperatingCity.city excludedVehicleVariants rentalsConfig
          Redis.setExp (mkSpecialLocationRedisKey specialLocationId ":rentals") rentalsAPIResp 3600 -- 1 hour
          pure rentalsAPIResp
    False -> do
      Redis.safeGet (mkLocationRedisKey req.currentCity ":rentals") >>= \case
        Just resp -> pure resp
        Nothing -> do
          rentalsAPIResp <- buildMininumRentalsFareArray req cityCenterLatLong merchant merchantOperatingCity.city excludedVehicleVariants rentalsConfig
          Redis.setExp (mkLocationRedisKey req.currentCity ":rentals") rentalsAPIResp 3600 -- 1 hour
          pure rentalsAPIResp
  let res =
        FareCacheResp
          { interCityMinimumFareResp = interCityResp,
            rentalsMininumFareResp = rentalsResp
          }
  pure res
  where
    fetchOriginSuccessor locItem = locItem.origin

mkLocationRedisKey :: Maybe City.City -> Text -> Text
mkLocationRedisKey currentCityLatLong suffix =
  case currentCityLatLong of
    Just currenCity -> show currenCity <> suffix
    Nothing -> "unknown_city:" <> suffix

mkSpecialLocationRedisKey :: Maybe (Id.Id LSS.SpecialLocation) -> Text -> Text
mkSpecialLocationRedisKey specialLocationId suffix =
  case specialLocationId of
    Just locId -> show locId <> suffix
    Nothing -> "special_location:" <> suffix

buildMininumIntercityFareArray ::
  JL.GetFareFlow m r =>
  FareCacheReq ->
  Maybe LatLong ->
  Merchant.Merchant ->
  City.City ->
  Maybe [ServiceTierType.ServiceTierType] ->
  Maybe [IntercitySearchLocation] ->
  Maybe Text ->
  m (Maybe [IntercitySearchResp])
buildMininumIntercityFareArray req mbSourceLatLong merchant merchanOperatingCity mbExcludedVariants mbInterCitySearchLocations driverOfferMerchantOperatingCityId = do
  let interCitySearchLocations_ = fromMaybe [] mbInterCitySearchLocations
      sourceLatLong = fromMaybe req.currentLatLong mbSourceLatLong
      excludedVehicleVariants = fromMaybe [] mbExcludedVariants
  buildMininumFare <-
    mapM
      ( \destinationItem -> do
          let destinationLatLon = fromMaybe sourceLatLong destinationItem.destination
              calculateFareReq =
                CallBPPInternal.CalculateFareReq
                  { pickupLatLong = LatLong {lat = sourceLatLong.lat, lon = sourceLatLong.lon},
                    dropLatLong = Just $ LatLong {lat = destinationLatLon.lat, lon = destinationLatLon.lon},
                    mbDistance = destinationItem.destinationDistance,
                    mbDuration = destinationItem.destinationDuration,
                    mbTripCategory = Just $ DTC.InterCity DTC.OneWayOnDemandStaticOffer driverOfferMerchantOperatingCityId
                  }
          fareData <- CallBPPInternal.getFare merchant merchanOperatingCity calculateFareReq
          let estimatedFares = fareData.estimatedFares
              mbFilteredResponse = filter (\item -> not (item.vehicleServiceTier `elem` excludedVehicleVariants)) estimatedFares
              mbMinFareResp = if null mbFilteredResponse then Nothing else Just $ minimumBy (comparing (.minFare)) mbFilteredResponse
              interCitySearchResp = case mbMinFareResp of
                Just minFareResp ->
                  Just $
                    IntercitySearchResp
                      { minimumFare =
                          Just $ minFareResp.maxFare,
                        destinationItem = Just $ destinationItem
                      }
                Nothing -> Nothing
          pure interCitySearchResp
      )
      interCitySearchLocations_
  pure $ Just $ catMaybes buildMininumFare

buildMininumRentalsFareArray ::
  JL.GetFareFlow m r =>
  FareCacheReq ->
  Maybe LatLong ->
  Merchant.Merchant ->
  City.City ->
  Maybe [ServiceTierType.ServiceTierType] ->
  Maybe [RentalsConfig] ->
  m (Maybe [RentalsSearchResp])
buildMininumRentalsFareArray req mbSourceLatLong merchant merchantOperatingCity mbExcludedVariants mbRentalsConfig = do
  let rentalsConfigList_ = fromMaybe [] mbRentalsConfig
      sourceLatLong = fromMaybe req.currentLatLong mbSourceLatLong
      excludedVehicleVariants = fromMaybe [] mbExcludedVariants
  buildMininumFare <-
    mapM
      ( \rentalElem -> do
          let calculateFareReq =
                CallBPPInternal.CalculateFareReq
                  { pickupLatLong = LatLong {lat = sourceLatLong.lat, lon = sourceLatLong.lon},
                    dropLatLong = Nothing,
                    mbDistance = Just rentalElem.rentalDistance,
                    mbDuration = Just rentalElem.rentalDuration,
                    mbTripCategory = Just $ DTC.Rental DTC.OnDemandStaticOffer
                  }
          fareData <- CallBPPInternal.getFare merchant merchantOperatingCity calculateFareReq
          let mbFilteredResponse = filter (\f -> not (f.vehicleServiceTier `elem` excludedVehicleVariants)) fareData.estimatedFares
              mbMinFareResp = if null mbFilteredResponse then Nothing else Just $ minimumBy (comparing (.minFare)) mbFilteredResponse
              rentalsSearchres =
                case mbMinFareResp of
                  Just minFareResp ->
                    Just $
                      RentalsSearchResp
                        { minimumFare = Just $ minFareResp.minFare,
                          rentalElement = rentalElem
                        }
                  Nothing -> Nothing
          pure rentalsSearchres
      )
      rentalsConfigList_
  pure $ Just $ catMaybes buildMininumFare
