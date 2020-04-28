module Beckn.Product.PassApplication where

import qualified Beckn.Data.Accessor                   as Accessor
import qualified Beckn.Storage.Queries.PassApplication as DB
import           Beckn.Types.API.PassApplication
import           Beckn.Types.App
import           Beckn.Types.Common
import qualified Beckn.Types.Common                    as Location (Location (..),
                                                                    LocationType)
import           Beckn.Types.Storage.PassApplication
import           Beckn.Utils.Common
import           Data.Aeson
import           EulerHS.Prelude
import           Beckn.Utils.Routes

createPassApplication ::
  Maybe Text -> CreatePassApplicationReq -> FlowHandler PassApplicationRes
createPassApplication regToken CreatePassApplicationReq{..} = withFlowHandler $ do
  id <- generateGUID
  passAppInfo <- (getPassAppInfo id)
  DB.create passAppInfo
  earea <- DB.findById id
  case earea of
    Right (Just passApplication) ->
      return $ PassApplicationRes passApplication
    _                 -> error "DBError" "Could not create PassApplication"
  where
    getPassType SELF          = INDIVIDUAL
    getPassType SPONSOROR     = INDIVIDUAL
    getPassType BULKSPONSOROR = INDIVIDUAL

    getPassAppInfo id = do
      currTime <- getCurrTime
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
              , ..
              }

listPassApplication ::
  Maybe Text
  -> Maybe Int
  -> Maybe Int
  -> [Status]
  -> [PassType]
  -> FlowHandler ListPassApplicationRes
listPassApplication regToken offsetM limitM status passType = undefined

getPassApplicationById :: Maybe Text -> Text -> FlowHandler PassApplicationRes
getPassApplicationById regToken applicationId = undefined

updatePassApplication ::
  Maybe Text ->
  Text ->
  UpdatePassApplicationReq ->
  FlowHandler PassApplicationRes
updatePassApplication regToken passApplicationId req = undefined

