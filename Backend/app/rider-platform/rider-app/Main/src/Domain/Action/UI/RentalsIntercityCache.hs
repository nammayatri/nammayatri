module Domain.Action.UI.RentalsIntercityCache
    (rentalsIntercityCache 
    , RentalsIntercityCacheReq(..)
    , RentalsIntercityCacheResp(..)
    )
where
import EulerHS.Prelude hiding (minimumBy)
import Kernel.Prelude hiding (map , minimumBy)
import Kernel.Utils.Common
import qualified Kernel.Storage.Hedis as Redis
import qualified SharedLogic.CallBPPInternal as CallBPPInternal
import Kernel.External.Maps.Types
import qualified Kernel.Types.Beckn.City as City
import Data.List (minimumBy)
import qualified Lib.JourneyModule.Types as JL
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as Person
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.Person as QPerson
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Kernel.Types.Id as Id
import Kernel.Types.Error
import qualified Domain.Types.Trip as DTC
import qualified Domain.Action.UI.Serviceability as SVC
import qualified Lib.Types.SpecialLocation as LSS



-- import Data.List (minimumBy)

data RentalsIntercityCacheReq = RentalsIntercityCacheReq
  { cityCenterLatLong :: Maybe LatLong,
    currentCity :: Maybe City.City,
    currentLatLong :: LatLong,
    interCitySearchLocations :: Maybe [LatLong],
    rentalsConfig :: Maybe [RentalsConfig]
  }
  deriving (Show, Generic , ToJSON , FromJSON, ToSchema)

data RentalsIntercityCacheResp = RentalsIntercityCacheResp
  { interCityMinimumFareResp :: Maybe [IntercitySearchResp],
    rentalsMininumFareResp :: Maybe [RentalsSearchResp]
  }
  deriving (Show, Generic , ToJSON , FromJSON, ToSchema)

data RentalsConfig = RentalsConfig {
    rentalDuration :: Seconds,
    rentalDistance :: Meters
} deriving (Show, Generic , FromJSON, ToJSON, ToSchema)

data IntercitySearchResp = IntercitySearchResp {
    mininumFare :: Maybe HighPrecMoney,
    destination :: Maybe LatLong
} deriving (Show, Generic , FromJSON, ToJSON , ToSchema)

data RentalsSearchResp = RentalsSearchResp {
    mininumFare :: Maybe HighPrecMoney,
    rentalElement :: RentalsConfig
} deriving (Show, Generic , FromJSON, ToJSON , ToSchema)

rentalsIntercityCache :: 
    JL.GetFareFlow m r =>
    Id.Id Person.Person ->
    Id.Id Merchant.Merchant ->
    RentalsIntercityCacheReq -> 
    m RentalsIntercityCacheResp
rentalsIntercityCache personId merchantId req = do
    merchant <- CQM.findById  (merchantId) >>= fromMaybeM (MerchantNotFound merchantId.getId)
    person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
    merchantOperatingCity <- CQMOC.findById person.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantOperatingCityId- " <> show person.merchantOperatingCityId)
    checkForServiceable <- SVC.checkServiceability (fetchOriginSuccessor) (personId,merchantId) req.currentLatLong False False
    let checkForSpecialLoation = checkForServiceable.serviceable
        specialLocationId = fmap (.id) checkForServiceable.specialLocation

    interCityResp <-
      case checkForSpecialLoation of
        True -> do
          Redis.safeGet (mkSpecialLocationRedisKey (specialLocationId) ":interCity") >>= \case
            Just resp -> pure resp
            Nothing -> do
              interCityFareResp <- buildMininumIntercityFareArray req (Just req.currentLatLong) merchant merchantOperatingCity.city
              Redis.setExp (mkSpecialLocationRedisKey specialLocationId ":interCity") interCityFareResp 3600 -- 1 hour
              pure interCityFareResp
        False -> do 
          Redis.safeGet (mkLocationRedisKey req.currentCity ":interCity") >>= \case 
              Just resp -> pure resp
              Nothing -> do
                interCityFareResp <- buildMininumIntercityFareArray req req.cityCenterLatLong merchant merchantOperatingCity.city
                Redis.setExp (mkLocationRedisKey req.currentCity ":interCity") interCityFareResp 3600 -- 1 hour
                pure interCityFareResp
    rentalsResp <- case checkForSpecialLoation of
      True -> do
        Redis.safeGet (mkSpecialLocationRedisKey specialLocationId ":rentals") >>= \case
          Just resp -> pure resp
          Nothing -> do
            rentalsAPIResp <- buildMininumRentalsFareArray req (Just req.currentLatLong) merchant merchantOperatingCity.city
            Redis.setExp (mkSpecialLocationRedisKey specialLocationId ":rentals") rentalsAPIResp 3600 -- 1 hour
            pure rentalsAPIResp
      False -> do 
        Redis.safeGet (mkLocationRedisKey req.currentCity ":interCity") >>= \case
          Just resp -> pure resp
          Nothing -> do
            rentalsAPIResp <- buildMininumRentalsFareArray req req.cityCenterLatLong merchant merchantOperatingCity.city
            Redis.setExp (mkLocationRedisKey req.currentCity ":rentals") rentalsAPIResp 3600 -- 1 hour
            pure rentalsAPIResp
    let res = RentalsIntercityCacheResp {   
                interCityMinimumFareResp = interCityResp,
                rentalsMininumFareResp = rentalsResp
              }
    pure $ res
    where 
      fetchOriginSuccessor niggesh = niggesh.origin

mkLocationRedisKey :: Maybe City.City -> Text -> Text
mkLocationRedisKey currentCityLatLong suffix = 
    case currentCityLatLong of
        Just currenCity -> (show currenCity) <> suffix
        Nothing ->  "unknown_city:" <> suffix
mkSpecialLocationRedisKey :: Maybe (Id.Id LSS.SpecialLocation)-> Text -> Text
mkSpecialLocationRedisKey specialLocationId suffix =
    case specialLocationId of
        Just locId -> (show locId) <> suffix
        Nothing ->  "special_location:" <> suffix


buildMininumIntercityFareArray :: 
    JL.GetFareFlow m r=>
    RentalsIntercityCacheReq -> 
    Maybe LatLong ->
    Merchant.Merchant ->
    City.City ->
    m (Maybe [IntercitySearchResp])
buildMininumIntercityFareArray req mbSourceLatLong merchant merchanOperatingCity = do
    let 
        interCitySearchLocations_ = fromMaybe [] req.interCitySearchLocations
        sourceLatLong = fromMaybe req.currentLatLong mbSourceLatLong
    buildMininumFare <- mapM
            (\destinationLatLon -> do
              let calculateFareReq =
                    CallBPPInternal.CalculateFareReq
                      { pickupLatLong = LatLong {lat = sourceLatLong.lat, lon = sourceLatLong.lon},
                        dropLatLong = Just $ LatLong {lat = destinationLatLon.lat, lon = destinationLatLon.lon},
                        mbDistance = Nothing,
                        mbDuration = Nothing,
                        mbTripCategory = Just $ DTC.InterCity DTC.OneWayOnDemandDynamicOffer Nothing
                      }
              fareData <- CallBPPInternal.getFare merchant merchanOperatingCity calculateFareReq
              let
                estimatedFares = fareData.estimatedFares 
                minFareResp = minimumBy (comparing (.minFare)) estimatedFares
              -- logDebug $ "MIN_FARE_ARRAY" <> show minFareResp
              let interCitySearchResp = IntercitySearchResp {
                  mininumFare = 
                    Just $ minFareResp.maxFare,
                  destination = Just $ destinationLatLon
              }
              pure interCitySearchResp 
          ) interCitySearchLocations_
    pure $ Just buildMininumFare


buildMininumRentalsFareArray :: 
    JL.GetFareFlow m r =>
    RentalsIntercityCacheReq -> 
    Maybe LatLong ->
    Merchant.Merchant ->
    City.City ->
    m  (Maybe [RentalsSearchResp])
buildMininumRentalsFareArray req mbSourceLatLong merchant merchantOperatingCity = do
    let 
        rentalsConfigList_ = fromMaybe [] req.rentalsConfig
        sourceLatLong = fromMaybe req.currentLatLong mbSourceLatLong
    -- excludedVehicleVariants <- getExcludedVehicleVariants req.currentCity
    -- merchantOperatingCity <- CQMOC.findById merchant.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantOperatingCityId- " <> show merchant.merchantOperatingCityId)
    buildMininumFare <- mapM
            (\rentalElem -> do
              let calculateFareReq =
                    CallBPPInternal.CalculateFareReq
                      { pickupLatLong = LatLong {lat = sourceLatLong.lat, lon = sourceLatLong.lon},
                        dropLatLong = Nothing,
                        mbDistance = Just rentalElem.rentalDistance,
                        mbDuration = Just rentalElem.rentalDuration,
                        mbTripCategory = Just $ DTC.Rental DTC.OnDemandStaticOffer
                      }
              fareData <- CallBPPInternal.getFare merchant merchantOperatingCity calculateFareReq
              -- let mbFilteredResponse =  (\f -> not (f.vehicleServiceTier `elem` excludedVehicleVariants)) fareData.estimatedFares
              let minFareResp = minimumBy (comparing (.minFare)) fareData.estimatedFares
              let rentalsSearchres = RentalsSearchResp {
                  mininumFare = Just $ minFareResp.minFare,
                  rentalElement =  rentalElem
              }
              pure rentalsSearchres
            ) rentalsConfigList_
    pure $ Just buildMininumFare

-- excludedVehicleVariants :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
--     Kernel.Types.Beckn.City -> 
--     m [VehicleServiceTier]
-- excludedVehicleVariants = getExcludedVehicleVariants req.currentCity
--  need to put this in config
-- also need to figure out how to get these intercity fares
