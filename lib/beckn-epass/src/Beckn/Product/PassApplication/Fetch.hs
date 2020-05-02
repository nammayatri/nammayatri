module Beckn.Product.PassApplication.Fetch where

import qualified Beckn.Data.Accessor                   as Accessor
import qualified Beckn.Storage.Queries.Blacklist       as Blacklist
import qualified Beckn.Storage.Queries.Comment         as Comment
import qualified Beckn.Storage.Queries.Customer        as Customer
import qualified Beckn.Storage.Queries.Document        as Document
import qualified Beckn.Storage.Queries.EntityDocument  as EntityDocument
import qualified Beckn.Storage.Queries.EntityTag       as EntityTag
import qualified Beckn.Storage.Queries.Organization    as Organization
import qualified Beckn.Storage.Queries.PassApplication as DB
import qualified Beckn.Storage.Queries.Tag             as Tag
import qualified Beckn.Types.API.PassApplication       as API
import           Beckn.Types.App
import           Beckn.Types.Common
import qualified Beckn.Types.Common                    as Location (Location (..),
                                                                    LocationType)
import qualified Beckn.Types.Storage.Customer          as Customer
import qualified Beckn.Types.Storage.Document          as Document
import qualified Beckn.Types.Storage.EntityDocument    as EntityDocument
import qualified Beckn.Types.Storage.EntityTag         as EntityTag
import           Beckn.Types.Storage.PassApplication
import qualified Beckn.Types.Storage.RegistrationToken as RegistrationToken
import qualified Beckn.Types.Storage.Tag               as Document
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

getPassAppInfo :: PassApplication -> L.Flow API.PassAppInfo
getPassAppInfo PassApplication {..} = do
  morg <- maybe (pure Nothing) (Organization.findOrganizationById) $ _OrganizationId
  mcustomer <- maybe (pure Nothing) (Customer.findCustomerById) $ _CustomerId
  let maybeCustId  = Customer._id <$> mcustomer
  entityDocs <- EntityDocument.findAllByPassApplicationId _id
  let docIds = EntityDocument._DocumentId <$> entityDocs
  docs <- catMaybes <$> (traverse (Document.findById) (DocumentId <$> docIds))
  entityTags <- maybe (pure []) (\id-> EntityTag.findAllByEntity "PASS_APPLICATION" $ _getOrganizationId id) _OrganizationId
  let tagIds = EntityTag._TagId <$> entityTags
  tags <- catMaybes <$> (traverse (Tag.findById) (TagId <$> tagIds))
  comments <- maybe (pure []) (\id-> Comment.findAllByCommentedOnEntity "PASS_APPLICATION" $ _getOrganizationId id) _OrganizationId
  isBlacklistedOrg <- maybe (pure False) (\oid-> isJust <$> (Blacklist.findByOrgId oid)) _OrganizationId
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
  let fromLocation = case _fromLocationType of
        Just locType ->
          Just $ Location
          { _type     = locType
          , _lat      = _fromLat
          , _long     = _fromLong
          , _ward     = _fromWard
          , _district = _fromDistrict
          , _city     = _fromCity
          , _state    = _fromState
          , _country  = _fromCountry
          , _pincode  = _fromPincode
          , _address  = _fromAddress
          , _bound   = _fromBound
          }
        Nothing -> Nothing
  pure API.PassAppInfo
    { _Customer = mcustomer
    , _Tags = tags
    , _Comments = comments
    , _Documents  = docs
    , _Organization = morg
    , _isBlacklistedOrganization = isBlacklistedOrg
    , _isBlacklistedLocation = False
    , _fromLocation = fromLocation
    , _toLocation = toLocation
    ,..
    }

getPassApplicationById :: Maybe Text -> PassApplicationId -> FlowHandler API.GetPassApplication
getPassApplicationById regToken applicationId = withFlowHandler $ do
  verifyToken regToken
  DB.findById applicationId
  >>= \case
    (Just v) -> do
      passInfo :: API.PassAppInfo <- getPassAppInfo v
      return $ API.GetPassApplication  passInfo
    Nothing -> L.throwException $ err400 {errBody = "Pass Application not found"}
