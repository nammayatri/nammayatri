module Beckn.Product.PassApplication.Fetch where

import qualified Beckn.Data.Accessor                   as Accessor
import qualified Beckn.Storage.Queries.Customer        as Customer
import qualified Beckn.Storage.Queries.Document        as Document
import qualified Beckn.Storage.Queries.EntityDocument  as EntityDocument
import qualified Beckn.Storage.Queries.Organization    as Organization
import qualified Beckn.Storage.Queries.PassApplication as DB
import qualified Beckn.Types.API.PassApplication       as API
import           Beckn.Types.App
import           Beckn.Types.Common
import qualified Beckn.Types.Common                    as Location (Location (..),
                                                                    LocationType)
import qualified Beckn.Types.Storage.Customer          as Customer
import qualified Beckn.Types.Storage.Document          as Document
import qualified Beckn.Types.Storage.EntityDocument    as EntityDocument
import           Beckn.Types.Storage.PassApplication
import qualified Beckn.Types.Storage.RegistrationToken as RegistrationToken
import           Beckn.Utils.Common
import           Beckn.Utils.Routes
import           Beckn.Utils.Storage
import           Data.Aeson
import           Data.List
import           Data.Maybe
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
      passApplications <- DB.findAllWithLimitOffsetWhere fPins fCities fDists fWards fStates toPins toCities toDists toWards toStates statuses orgIds passType limitM offsetM
      let orgIds = nub $ catMaybes $ _OrganizationId <$> passApplications
          custIds = nub $ catMaybes $ _CustomerId <$> passApplications
          passAppIds =  _id <$> passApplications
      passInfo <- (traverse getPassAppInfo passApplications)
      return $ API.ListPassApplicationRes passInfo
     where
       getPassAppInfo PassApplication {..} = do
         morg <- maybe (pure Nothing) (Organization.findOrganizationById) $ _OrganizationId
         mcustomer <- maybe (pure Nothing) (Customer.findCustomerById) $ _CustomerId
         let maybeCustId  = Customer._id <$> mcustomer
         entityDocs <- EntityDocument.findAllByPassApplicationId _id
         let docIds = EntityDocument._DocumentId <$> entityDocs
         docs <- catMaybes <$> (traverse (Document.findById) (DocumentId <$> docIds))
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
            , _Tags = []
            , _Documents  = docs
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
  >>= fromMaybeM400 "Pass Application not found"
  >>= return . API.PassApplicationRes
