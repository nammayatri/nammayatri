{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}

module Epass.Product.User.CRUD where

import Beckn.Types.App (PersonId (..))
import qualified Beckn.Types.Storage.Person as Person
import qualified Beckn.Types.Storage.RegistrationToken as SR
import Data.Aeson
import qualified Data.List as List
import Data.Time
import Epass.Product.Common
import qualified Epass.Storage.Queries.Location as Location
import qualified Epass.Storage.Queries.Organization as Org
import Epass.Types.API.Common
import Epass.Types.API.User
import Epass.Types.App
import Epass.Types.Common
import qualified Epass.Types.Storage.Location as Location
import qualified Epass.Types.Storage.Organization as Org
import Epass.Utils.Common
import Epass.Utils.Extra
import Epass.Utils.Routes
import Epass.Utils.Storage
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Servant
import qualified Storage.Queries.Person as Person

create :: Maybe Text -> CreateReq -> FlowHandler CreateRes
create regToken CreateReq {..} = withFlowHandler $ do
  verifyToken regToken
  loc <- Location.findLocationWithErr _locationId
  user <- userInfo id
  Person.create user
  eres <- Person.findById (user ^. #_id)
  case eres of
    Nothing -> L.throwException $ err500 {errBody = "Couldnt find user"}
    Just user -> do
      locInfo <- getLocationInfo _locationId
      return $ mkUInfo user locInfo
  where
    userInfo id = do
      id <- L.generateGUID -- TODO: use GUID typeclass instance
      now <- getCurrTime
      return $
        Person.Person
          { _id = PersonId id,
            _firstName = Nothing,
            _middleName = Nothing,
            _lastName = Nothing,
            _fullName = Just $ _name,
            _role = _role,
            _gender = Person.UNKNOWN,
            _identifierType = Person.MOBILENUMBER,
            _email = Nothing,
            _mobileNumber = Just $ _mobileNumber,
            _mobileCountryCode = _mobileCountryCode,
            _identifier = Just $ _mobileNumber,
            _rating = Nothing,
            _verified = False,
            _udf1 = Nothing,
            _udf2 = Nothing,
            _status = Person.INACTIVE,
            _organizationId = Just $ _getOrganizationId _organizationId,
            _locationId = Just $ _locationId,
            _deviceToken = Nothing,
            _description = Nothing,
            _createdAt = now,
            _updatedAt = now
          }

list ::
  Maybe Text ->
  Maybe Int ->
  Maybe Int ->
  Maybe LocateBy ->
  [Text] ->
  [Person.Role] ->
  FlowHandler ListRes
list regToken offsetM limitM locateM locate roleM = withFlowHandler $ do
  reg <- verifyToken regToken
  -- when (SR._entityType reg == SR.CUSTOMER) $ do
  --   L.throwException $ err400 {errBody = "UNAUTHORIZED_CUSTOMER"}
  user <-
    fromMaybeM500 "Could not find user"
      =<< Person.findById (PersonId $ SR._EntityId reg)
  orgM <-
    join
      <$> mapM
        (Org.findOrganizationById . OrganizationId)
        (user ^. #_organizationId)
  when (isNothing orgM)
    $ L.throwException
    $ err400 {errBody = "NO_ORGANIZATION_FOUND"}
  getUsers limitM offsetM locateM roleM locate user (fromJust orgM)

getUsers ::
  Maybe Int ->
  Maybe Int ->
  Maybe LocateBy ->
  [Person.Role] ->
  [Text] ->
  Person.Person ->
  Org.Organization ->
  L.Flow ListRes
getUsers offsetM limitM locateM role locate user org = do
  case user ^. #_role of
    Person.ADMIN ->
      case locateM of
        Just LCITY -> cityLevelUsers limitM offsetM role locate
        Just LDISTRICT -> districtLevelUsers limitM offsetM role locate
        Just LWARD -> wardLevelUsers limitM offsetM role locate
        _ ->
          Person.findAllWithLimitOffsetByRole limitM offsetM role
            >>= return . ListRes
    Person.CITYLEVEL -> do
      allLocations <-
        Location.findByStOrDistrict offsetM limitM LCITY (org ^. #_city)
      case locateM of
        Just LCITY ->
          if List.null locate || elem (org ^. #_city) locate
            then cityLevelUsers limitM offsetM role [(org ^. #_city)]
            else L.throwException $ err400 {errBody = "UNAUTHORIZED"}
        Just LDISTRICT -> do
          let dists = catMaybes $ map Location._district allLocations
          let locateD =
                if List.null locate
                  then dists
                  else filter (flip elem dists) locate
          districtLevelUsers limitM offsetM role locateD
        Just LWARD -> do
          let wards = catMaybes $ map Location._ward allLocations
          let locateW =
                if List.null locate
                  then wards
                  else filter (flip elem wards) locate
          wardLevelUsers limitM offsetM role locateW
        _ -> L.throwException $ err400 {errBody = "UNAUTHORIZED"}
    Person.DISTRICTLEVEL -> do
      let district = fromJust $ org ^. #_district
      allLocations <-
        Location.findByStOrDistrict offsetM limitM LDISTRICT district
      case locateM of
        Just LDISTRICT ->
          if List.null locate || elem district locate
            then districtLevelUsers limitM offsetM role [district]
            else L.throwException $ err400 {errBody = "UNAUTHORIZED"}
        Just LWARD -> do
          let wards = catMaybes $ map Location._ward allLocations
          let locateW =
                if List.null locate
                  then wards
                  else filter (flip elem wards) locate
          wardLevelUsers limitM offsetM role locateW
        _ -> L.throwException $ err400 {errBody = "UNAUTHORIZED"}
    Person.WARDLEVEL -> do
      let ward = fromJust $ org ^. #_ward
      case locateM of
        Just LWARD ->
          if List.null locate || List.elem ward locate
            then wardLevelUsers limitM offsetM role [ward]
            else L.throwException $ err400 {errBody = "UNAUTHORIZED"}
        _ -> L.throwException $ err400 {errBody = "UNAUTHORIZED"}
    _ -> L.throwException $ err400 {errBody = "UNAUTHORIZED"}

cityLevelUsers :: Maybe Int -> Maybe Int -> [Person.Role] -> [Text] -> L.Flow ListRes
cityLevelUsers limitM offsetM r cities =
  Org.listOrganizations
    Nothing
    Nothing
    mempty
    mempty
    cities
    mempty
    mempty
    empty
    empty
    Nothing
    >>= Person.findAllWithLimitOffsetBy limitM offsetM r . map (^. #_id)
    >>= return . ListRes

districtLevelUsers ::
  Maybe Int -> Maybe Int -> [Person.Role] -> [Text] -> L.Flow ListRes
districtLevelUsers limitM offsetM r districts =
  Org.listOrganizations
    Nothing
    Nothing
    mempty
    mempty
    mempty
    districts
    mempty
    empty
    empty
    Nothing
    >>= Person.findAllWithLimitOffsetBy limitM offsetM r . map (^. #_id)
    >>= return . ListRes

wardLevelUsers :: Maybe Int -> Maybe Int -> [Person.Role] -> [Text] -> L.Flow ListRes
wardLevelUsers limitM offsetM r wards =
  Org.listOrganizations
    Nothing
    Nothing
    mempty
    mempty
    mempty
    mempty
    wards
    empty
    empty
    Nothing
    >>= Person.findAllWithLimitOffsetBy limitM offsetM r . map (^. #_id)
    >>= return . ListRes

get :: Maybe Text -> PersonId -> FlowHandler GetRes
get regToken userId = withFlowHandler $ do
  verifyToken regToken
  user <-
    fromMaybeM400 "User not found"
      =<< Person.findById userId
  locInfo <- getLocationInfo (fromJust $ user ^. #_locationId) -- TODO: fix this
  return $ mkUInfo user locInfo

update :: Maybe Text -> PersonId -> UpdateReq -> FlowHandler UpdateRes
update regToken userId UpdateReq {..} = withFlowHandler $ do
  verifyToken regToken
  Person.update userId _status _name _email _role
  Person.findById userId
    >>= fromMaybeM500 "Couldnot find user"
    >>= return . UpdateRes

delete :: Maybe RegistrationTokenText -> PersonId -> FlowHandler Ack
delete regToken userId = withFlowHandler $ do
  verifyToken regToken
  Person.deleteById userId
  sendAck

listRoles :: Maybe RegistrationTokenText -> FlowHandler [Person.Role]
listRoles regToken = withFlowHandler $ do
  verifyToken regToken
  pure $ enumFrom minBound

-- Transformers
mkUInfo :: Person.Person -> LocationInfo -> UserInfo
mkUInfo user locInfo = UserInfo {_user = user, _locationInfo = locInfo}
