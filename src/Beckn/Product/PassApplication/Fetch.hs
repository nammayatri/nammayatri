module Beckn.Product.PassApplication.Fetch where

import qualified Beckn.Data.Accessor                   as Accessor
import qualified Beckn.Storage.Queries.Customer        as Customer
import qualified Beckn.Storage.Queries.Organization    as Organization
import qualified Beckn.Storage.Queries.PassApplication as DB
import qualified Beckn.Types.API.PassApplication       as API
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
import           Data.List
import qualified EulerHS.Language                      as L
import           EulerHS.Prelude
import           Servant

listPassApplication ::
  Maybe Text
  -> Maybe Int
  -> Maybe Int
  -> [Int]
  -> [Text]
  -> [Text]
  -> [Text]
  -> [Text]
  -> [Int]
  -> [Text]
  -> [Text]
  -> [Text]
  -> [Text]
  -> [Status]
  -> [OrganizationId]
  -> [PassType]
  -> FlowHandler API.ListPassApplicationRes
listPassApplication regToken limitM offsetM fPins fCities fDists fWards fStates toPins toCities toDists toWards toStates statuses orgIds passType =
   withFlowHandler $ do
      verifyToken regToken
      passApplications <- (DB.findAllWithLimitOffsetWhere fPins fCities fDists fWards fStates toPins toCities toDists toWards toStates statuses orgIds passType limitM offsetM
        >>= \case
            Left err -> L.throwException $ err500 {errBody = ("DBError: " <> show err)}
            Right v -> pure v)
      let orgIds = nub $ catMaybes $ _OrganizationId <$> passApplications
          custIds = nub $ catMaybes $ _CustomerId <$> passApplications
          passAppIds =  _id <$> passApplications
      passInfo <- (traverse getPassAppInfo passApplications)
      return $ API.ListPassApplicationRes passInfo
     where
       getPassAppInfo PassApplication {..} = do
         morg <- maybe (pure Nothing) (Organization.findOrganizationById) $ _OrganizationId
         mcustomer <- maybe (pure Nothing) (Customer.findCustomerById) $ _CustomerId
         let toLocation = Location
                          { _type     = fromMaybe PINCODE _toLocationType
                          , _lat      = _toLat
                          , _long     = _toLong
                          , _ward     = _toWard
                          , _district = _toDistrict
                          , _city     = _toCity
                          , _state    = _toState
                          , _country  = _toCountry
                          , _pincode  = _toPincode
                          , _address  = _toAddress
                          , _bound   = _toBound
                          }

         pure API.PassAppInfo
            { _Customer = mcustomer
            , _Tags = Nothing
            , _Documents  = Nothing
            , _Organization = morg
            , _isBlacklistedOrganization = False
            , _isBlacklistedLocation = False
            , _fromLocation = Nothing
            , _toLocation = toLocation
            ,..
            }

getPassApplicationById :: Maybe Text -> PassApplicationId -> FlowHandler API.PassApplicationRes
getPassApplicationById regToken applicationId = withFlowHandler $ do
  verifyToken regToken
  DB.findById applicationId
  >>= \case
    Right (Just v) -> return $ API.PassApplicationRes v
    Right Nothing -> L.throwException $ err400 {errBody = "Pass Application not found"}
    Left err -> L.throwException $ err500 {errBody = ("DBError: " <> show err)}
