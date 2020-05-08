module Epass.Product.PassApplication.Fetch where

import qualified Epass.Data.Accessor                   as Accessor
import qualified Epass.Storage.Queries.Blacklist       as Blacklist
import qualified Epass.Storage.Queries.Comment         as Comment
import qualified Epass.Storage.Queries.Customer        as Customer
import qualified Epass.Storage.Queries.Document        as Document
import qualified Epass.Storage.Queries.EntityDocument  as EntityDocument
import qualified Epass.Storage.Queries.EntityTag       as EntityTag
import qualified Epass.Storage.Queries.Organization    as Organization
import qualified Epass.Storage.Queries.PassApplication as DB
import qualified Epass.Storage.Queries.Tag             as Tag
import qualified Epass.Types.API.PassApplication       as API
import           Epass.Types.App
import           Epass.Types.Common
import qualified Epass.Types.Common                    as Location (Location (..),
                                                                    LocationType)
import qualified Epass.Types.Storage.Customer          as Customer
import qualified Epass.Types.Storage.Document          as Document
import qualified Epass.Types.Storage.EntityDocument    as EntityDocument
import qualified Epass.Types.Storage.EntityTag         as EntityTag
import           Epass.Types.Storage.PassApplication
import qualified Epass.Types.Storage.RegistrationToken as RegistrationToken
import qualified Epass.Types.Storage.Tag               as Document
import           Epass.Utils.Common
import           Epass.Utils.Routes
import           Epass.Utils.Storage
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
      token <- verifyToken regToken
      let entityId = scopeEntityAccess token
      passApplications <- DB.findAllWithLimitOffsetWhere fPins fCities fDists fWards fStates toPins toCities toDists toWards toStates statuses orgIds passType entityId limitM offsetM
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

scopeEntityAccess :: RegistrationToken.RegistrationToken -> Maybe CustomerId
scopeEntityAccess RegistrationToken.RegistrationToken{..} =
  case _entityType of
    RegistrationToken.CUSTOMER -> Just (CustomerId _EntityId)
    RegistrationToken.USER     -> Nothing
