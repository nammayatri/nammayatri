module Domain.Action.Dashboard.AppManagement.ShuttleInterest
  ( getShuttleInterestList,
    getShuttleInterestMapPoints,
  )
where

import qualified API.Types.Dashboard.AppManagement.ShuttleInterest as ShuttleInterestAPI
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified "this" Domain.Types.Person as DPerson
import qualified "this" Domain.Types.ShuttleInterest as DShuttleInterest
import qualified Environment
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import qualified Kernel.Types.Id as Id
import Kernel.Utils.Common (fromMaybeM)
import qualified Storage.Queries.MerchantOperatingCity as QMerchantOperatingCity
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.ShuttleInterest as QShuttleInterest
import Tools.Error

-- | The map shows every matching interest at once; this caps one response.
maxMapPoints :: Int
maxMapPoints = 5000

-- | Interest forms of the dashboard's city, newest first, with the employee's name, for the control-centre table.
-- companyDomain (e.g. juspay.in) narrows it to one company.
getShuttleInterestList ::
  ( Id.ShortId DMerchant.Merchant ->
    Context.City ->
    Maybe Text ->
    Maybe Int ->
    Maybe Int ->
    Environment.Flow ShuttleInterestAPI.ShuttleInterestListResp
  )
getShuttleInterestList merchantShortId opCity mbCompanyDomain mbLimit mbOffset = do
  merchantOperatingCity <- findCity merchantShortId opCity
  let limit = min 100 (max 1 (fromMaybe 20 mbLimit))
      offset = max 0 (fromMaybe 0 mbOffset)
  interests <- findInterests merchantOperatingCity.id mbCompanyDomain limit offset
  employeeNames <- getEmployeeNames interests
  pure $
    ShuttleInterestAPI.ShuttleInterestListResp
      { ShuttleInterestAPI.interests = map (mkItem employeeNames) interests,
        ShuttleInterestAPI.limit = limit,
        ShuttleInterestAPI.offset = offset
      }

-- | Pickup and drop coordinates of the city's interests (optionally one company's), for the control-centre map.
getShuttleInterestMapPoints ::
  ( Id.ShortId DMerchant.Merchant ->
    Context.City ->
    Maybe Text ->
    Environment.Flow ShuttleInterestAPI.ShuttleInterestMapResp
  )
getShuttleInterestMapPoints merchantShortId opCity mbCompanyDomain = do
  merchantOperatingCity <- findCity merchantShortId opCity
  interests <- findInterests merchantOperatingCity.id mbCompanyDomain maxMapPoints 0
  pure $ ShuttleInterestAPI.ShuttleInterestMapResp {ShuttleInterestAPI.points = map mkMapPoint interests}

findCity :: Id.ShortId DMerchant.Merchant -> Context.City -> Environment.Flow DMOC.MerchantOperatingCity
findCity merchantShortId opCity =
  QMerchantOperatingCity.findByMerchantShortIdAndCity merchantShortId opCity >>= fromMaybeM (InvalidRequest "Merchant Operating City not found")

findInterests :: Id.Id DMOC.MerchantOperatingCity -> Maybe Text -> Int -> Int -> Environment.Flow [DShuttleInterest.ShuttleInterest]
findInterests merchantOperatingCityId mbCompanyDomain limit offset =
  case T.toLower . T.strip <$> mbCompanyDomain of
    Just companyDomain
      | not (T.null companyDomain) ->
        QShuttleInterest.findAllByMerchantOperatingCityIdAndCompanyDomain (Just limit) (Just offset) merchantOperatingCityId companyDomain
    _ -> QShuttleInterest.findAllByMerchantOperatingCityId (Just limit) (Just offset) merchantOperatingCityId

getEmployeeNames :: [DShuttleInterest.ShuttleInterest] -> Environment.Flow (Map.Map (Id.Id DPerson.Person) Text)
getEmployeeNames [] = pure Map.empty
getEmployeeNames interests = do
  persons <- QPerson.findAllByIds (map (.personId) interests)
  pure $ Map.fromList [(person.id, name) | person <- persons, Just name <- [fullName person]]
  where
    fullName person =
      let name = T.unwords (catMaybes [person.firstName, person.lastName])
       in if T.null name then Nothing else Just name

mkItem :: Map.Map (Id.Id DPerson.Person) Text -> DShuttleInterest.ShuttleInterest -> ShuttleInterestAPI.ShuttleInterestItem
mkItem employeeNames interest =
  ShuttleInterestAPI.ShuttleInterestItem
    { ShuttleInterestAPI.personId = interest.personId,
      ShuttleInterestAPI.employeeName = Map.lookup interest.personId employeeNames,
      ShuttleInterestAPI.companyDomain = interest.companyDomain,
      ShuttleInterestAPI.pickupLat = interest.pickupLat,
      ShuttleInterestAPI.pickupLon = interest.pickupLon,
      ShuttleInterestAPI.pickupAddress = interest.pickupAddress,
      ShuttleInterestAPI.dropLat = interest.dropLat,
      ShuttleInterestAPI.dropLon = interest.dropLon,
      ShuttleInterestAPI.dropAddress = interest.dropAddress,
      ShuttleInterestAPI.tripType = interest.tripType,
      ShuttleInterestAPI.pickupTime = interest.pickupTime,
      ShuttleInterestAPI.dropTime = interest.dropTime,
      ShuttleInterestAPI.shiftStartTime = interest.shiftStartTime,
      ShuttleInterestAPI.shiftEndTime = interest.shiftEndTime,
      ShuttleInterestAPI.maxWalkingDistanceMeters = interest.maxWalkingDistanceMeters,
      ShuttleInterestAPI.daysOfWeek = interest.daysOfWeek,
      ShuttleInterestAPI.timeFlexibilityMinutes = interest.timeFlexibilityMinutes,
      ShuttleInterestAPI.acPreference = interest.acPreference,
      ShuttleInterestAPI.vehicleSizePreference = interest.vehicleSizePreference,
      ShuttleInterestAPI.createdAt = interest.createdAt,
      ShuttleInterestAPI.updatedAt = interest.updatedAt
    }

mkMapPoint :: DShuttleInterest.ShuttleInterest -> ShuttleInterestAPI.ShuttleInterestMapPoint
mkMapPoint interest =
  ShuttleInterestAPI.ShuttleInterestMapPoint
    { ShuttleInterestAPI.personId = interest.personId,
      ShuttleInterestAPI.companyDomain = interest.companyDomain,
      ShuttleInterestAPI.tripType = interest.tripType,
      ShuttleInterestAPI.pickupLat = interest.pickupLat,
      ShuttleInterestAPI.pickupLon = interest.pickupLon,
      ShuttleInterestAPI.dropLat = interest.dropLat,
      ShuttleInterestAPI.dropLon = interest.dropLon
    }
