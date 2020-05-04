{-# LANGUAGE TypeFamilies #-}

module Beckn.Product.User.CRUD where

import qualified Beckn.Data.Accessor                   as Accessor
import           Beckn.Product.Common
import qualified Beckn.Storage.Queries.Location        as Loc
import qualified Beckn.Storage.Queries.Organization    as QO
import qualified Beckn.Storage.Queries.User            as DB
import           Beckn.Types.API.Common                as C
import           Beckn.Types.API.User
import           Beckn.Types.App
import           Beckn.Types.Common
import qualified Beckn.Types.Storage.Organization      as SO
import qualified Beckn.Types.Storage.RegistrationToken as SR
import           Beckn.Types.Storage.User              as Storage
import qualified Beckn.Types.Storage.User              as SU
import           Beckn.Utils.Common
import           Beckn.Utils.Routes
import           Beckn.Utils.Storage
import           Data.Aeson
import           Data.Default
import           Data.Time
import qualified Database.Beam.Schema.Tables           as B
import qualified EulerHS.Language                      as L
import           EulerHS.Prelude
import           Servant

create :: Maybe Text -> CreateReq -> FlowHandler CreateRes
create regToken CreateReq {..} =
  withFlowHandler $ do
    verifyToken regToken
    id <- generateGUID
    loc <- Loc.findLocationWithErr _LocationId
    user <- userInfo id
    DB.create user
    eres <- DB.findById id
    locInfo <- getLocationInfo _LocationId
    return $ mkUInfo user locInfo
  where
    userInfo id = do
      now <- getCurrTime
      return
        Storage.User
          { _id = id
          , _verified = False
          , _status = INACTIVE
          , _info = Nothing
          , _createdAt = now
          , _updatedAt = now
          , ..
          }

list ::
     Maybe Text
  -> Maybe Int
  -> Maybe Int
  -> Maybe LocateBy
  -> Maybe Role
  -> [Role]
  -> FlowHandler ListRes
list regToken offsetM limitM locateM locate roleM =
  withFlowHandler $ do
    reg <- verifyToken regToken

    when (SR._entityType reg == SR.CUSTOMER) $ do
      L.throwException $ err400 {errBody = "UNAUTHORIZED_CUSTOMER"}

    user <- DB.findById (UserId $ SR._EntityId reg)
    orgM <- QO.findOrganizationById (SU._OrganizationId user)
    when (isNothing orgM) $
      L.throwException $ err400 {errBody = "NO_ORGANIZATION_FOUND"}
    let org = fromJust orgM

    if null roleM
      then getLocateBased offsetM limitM locateM locate user org
      else DB.findAllWithLimitOffsetByRole limitM offsetM roleM >>=
           return . ListRes

getLocateBased ::
     Maybe Int
  -> Maybe Int
  -> Maybe LocateBy
  -> Maybe Role
  -> SU.User
  -> SO.Organization
  -> L.Flow ListRes
getLocateBased offsetM limitM locateM locate user org =
  case SU._role user of
    ADMIN ->
      case locateM of
        Just LCITY -> cityLevelUsers limitM offsetM locate [(SO._city org)]
        Just LDISTRICT ->
          districtLevelUsers limitM offsetM locate [(fromJust $ SO._district org)]
        Just LWARD -> wardLevelUsers limitM offsetM locate [(fromJust $ SO._ward org)]
        _ -> DB.findAllWithLimitOffset limitM offsetM >>= return . ListRes
    CITYLEVEL ->
      case locateM of
        Just LCITY -> cityLevelUsers limitM offsetM locate [(SO._city org)]
        Just LDISTRICT ->
          districtLevelUsers limitM offsetM locate [(fromJust $ SO._district org)]
        Just LWARD -> wardLevelUsers limitM offsetM locate [(fromJust $ SO._ward org)]
        _ -> L.throwException $ err400 {errBody = "UNAUTHORIZED"}
    DISTRICTLEVEL -> do
      case locateM of
        Just LDISTRICT ->
          districtLevelUsers limitM offsetM locate [(fromJust $ SO._district org)]
        Just LWARD -> wardLevelUsers limitM offsetM locate [(fromJust $ SO._ward org)]
        _ -> L.throwException $ err400 {errBody = "UNAUTHORIZED"}
    WARDLEVEL -> do
      case locateM of
        Just LWARD -> wardLevelUsers limitM offsetM locate [(fromJust $ SO._ward org)]
        _ -> L.throwException $ err400 {errBody = "UNAUTHORIZED"}
    _ -> L.throwException $ err400 {errBody = "UNAUTHORIZED"}

cityLevelUsers :: Maybe Int -> Maybe Int -> Maybe Role -> [Text] -> L.Flow ListRes
cityLevelUsers limitM offsetM r cities =
  QO.listOrganizations
    Nothing
    Nothing
    mempty
    mempty
    cities
    mempty
    mempty
    empty
    empty
    Nothing >>=
  DB.findAllWithLimitOffsetBy limitM offsetM r . map SO._id >>=
  return . ListRes

districtLevelUsers :: Maybe Int -> Maybe Int -> Maybe Role -> [Text] -> L.Flow ListRes
districtLevelUsers limitM offsetM r districts =
  QO.listOrganizations
    Nothing
    Nothing
    mempty
    mempty
    mempty
    districts
    mempty
    empty
    empty
    Nothing >>=
  DB.findAllWithLimitOffsetBy limitM offsetM r . map SO._id >>=
  return . ListRes

wardLevelUsers :: Maybe Int -> Maybe Int -> Maybe Role -> [Text] -> L.Flow ListRes
wardLevelUsers limitM offsetM r wards =
  QO.listOrganizations
    Nothing
    Nothing
    mempty
    mempty
    mempty
    mempty
    wards
    empty
    empty
    Nothing >>=
  DB.findAllWithLimitOffsetBy limitM offsetM r . map SO._id >>=
  return . ListRes

get :: Maybe Text -> UserId -> FlowHandler GetRes
get regToken userId = withFlowHandler $ do
  verifyToken regToken
  user@User {..} <- DB.findById userId
  locInfo <- getLocationInfo _LocationId
  return $ mkUInfo user locInfo

update :: Maybe Text -> UserId -> UpdateReq -> FlowHandler UpdateRes
update regToken userId UpdateReq {..} =
  withFlowHandler $
  do verifyToken regToken
     DB.update userId _status _name _role
     UpdateRes <$> DB.findById userId

delete :: Maybe RegistrationTokenText -> UserId -> FlowHandler Ack
delete regToken userId =
  withFlowHandler $ do
    verifyToken regToken
    DB.deleteById userId
    sendAck

listRoles :: Maybe RegistrationTokenText -> FlowHandler [Role]
listRoles regToken =
  withFlowHandler $ do
    verifyToken regToken
    pure $ enumFrom minBound

-- Transformers
mkUInfo :: User -> C.LocationInfo -> UserInfo
mkUInfo user locInfo =
  UserInfo
    { _user = user
    , _locationInfo = locInfo
    }