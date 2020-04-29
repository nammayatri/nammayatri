module Beckn.Product.PassApplication where

import qualified Beckn.Data.Accessor                   as Accessor
import qualified Beckn.Storage.Queries.PassApplication as DB
import           Beckn.Types.API.PassApplication
import           Beckn.Types.App
import           Beckn.Types.Common
import qualified Beckn.Types.Common                    as Location (Location (..),
                                                                    LocationType)
import           Beckn.Types.Storage.PassApplication
import qualified Beckn.Types.Storage.RegistrationToken as RegistrationToken
import           Beckn.Utils.Common
import           Beckn.Utils.Routes
import           Beckn.Utils.Storage
import           Data.Aeson
import qualified EulerHS.Language                      as L
import           EulerHS.Prelude
import           Servant

createPassApplication ::
  Maybe Text -> CreatePassApplicationReq -> FlowHandler PassApplicationRes
createPassApplication regToken CreatePassApplicationReq{..} = withFlowHandler $ do
  token <- verifyToken regToken
  checkForCustomerId _type _CustomerId
  checkForOrgId _type _OrganizationId
  id <- generateGUID
  passAppInfo <- (getPassAppInfo id token)
  DB.create passAppInfo
  earea <- DB.findById id
  case earea of
    Right (Just passApplication) ->
      return $ PassApplicationRes passApplication
    _                 -> L.throwException $ err500 {errBody = "Could not create PassApplication"}
  where
    checkForCustomerId :: PassApplicationType -> Maybe CustomerId -> L.Flow ()
    checkForCustomerId pAtype mCustId =
      if (pAtype == SELF || pAtype == SPONSOROR) && mCustId == Nothing
        then L.throwException $ err400 {errBody = "CustomerId cannot be empty"}
        else return ()

    checkForOrgId :: PassApplicationType -> Maybe OrganizationId -> L.Flow ()
    checkForOrgId BULKSPONSOROR Nothing = L.throwException $ err400 {errBody = "CustomerId cannot be empty"}
    checkForOrgId _ _ = return ()

    getPassType SELF          = INDIVIDUAL
    getPassType SPONSOROR     = INDIVIDUAL
    getPassType BULKSPONSOROR = ORGANIZATION

    getCount SELF _ = return 1
    getCount SPONSOROR _ = return 1
    getCount BULKSPONSOROR (Just c) = return c
    getCount BULKSPONSOROR Nothing = L.throwException $ err400 {errBody = "Count cannot be null"}

    getPassAppInfo id token = do
      currTime <- getCurrTime
      count <- getCount _type _count
      return $ PassApplication
              { _id = id
              , _type = getPassType _type
              , _fromLocationType = Location._type <$> _fromLocation
              , _fromLat = join (Location._lat <$> _fromLocation)
              , _fromLong = join (Location._long <$> _fromLocation)
              , _fromWard = join (Location._ward <$> _fromLocation)
              , _fromDistrict = join (Location._district <$> _fromLocation)
              , _fromCity = join (Location._city <$> _fromLocation)
              , _fromState = join (Location._state <$> _fromLocation)
              , _fromCountry = join (Location._country <$> _fromLocation)
              , _fromPincode = join (Location._pincode <$> _fromLocation)
              , _fromAddress = join (Location._address <$> _fromLocation)
              , _fromBound = join (Location._bound <$> _fromLocation)
              , _toLocationType = Just $ Location._type _toLocation
              , _toLat = Location._lat _toLocation
              , _toLong = Location._long _toLocation
              , _toWard = Location._ward _toLocation
              , _toDistrict = Location._district _toLocation
              , _toCity = Location._city _toLocation
              , _toState = Location._state _toLocation
              , _toCountry = Location._country _toLocation
              , _toPincode = Location._pincode _toLocation
              , _toAddress = Location._address _toLocation
              , _toBound = Location._bound _toLocation
              , _createdAt = currTime
              , _updatedAt = currTime
              , _status = PENDING
              , _CreatedBy = CustomerId (RegistrationToken._EntityId token)
              , _AssignedTo = UserId "admin" -- TODO: fix this
              , _count = count
              , _approvedCount = 0
              , _remarks = ""
              , _info = ""
              , ..
              }

listPassApplication ::
  Maybe Text
  -> Maybe Int
  -> Maybe Int
  -> [Status]
  -> [PassType]
  -> FlowHandler ListPassApplicationRes
listPassApplication regToken offsetM limitM status passType = withFlowHandler $ do
  verifyToken regToken
  DB.findAllWithLimitOffsetWhere status passType limitM offsetM
  >>= \case
      Left err -> L.throwException $ err500 {errBody = ("DBError: " <> show err)}
      Right v -> return $ ListPassApplicationRes v

getPassApplicationById :: Maybe Text -> PassApplicationId -> FlowHandler PassApplicationRes
getPassApplicationById regToken applicationId = withFlowHandler $ do
  verifyToken regToken
  DB.findById applicationId
  >>= \case
    Right (Just v) -> return $ PassApplicationRes v
    Right Nothing -> L.throwException $ err400 {errBody = "Pass Application not found"}
    Left err -> L.throwException $ err500 {errBody = ("DBError: " <> show err)}

updatePassApplication ::
  Maybe Text ->
  PassApplicationId ->
  UpdatePassApplicationReq ->
  FlowHandler PassApplicationRes
updatePassApplication regToken passApplicationId UpdatePassApplicationReq{..} = withFlowHandler $ do
  verifyToken regToken
  eres <- DB.update passApplicationId _status _approvedCount _remarks
  case eres of
    Left err -> L.throwException $ err500 {errBody = ("DBError: " <> show err)}
    Right _ ->
      DB.findById passApplicationId
      >>= \case
        Right (Just v) -> return $ PassApplicationRes v
        Right Nothing -> L.throwException $ err400 {errBody = "Pass Application not found"}
        Left err -> L.throwException $ err500 {errBody = ("DBError: " <> show err)}
