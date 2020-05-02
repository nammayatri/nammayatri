module Beckn.Product.Organization where

import qualified Beckn.Data.Accessor                   as Lens
import qualified Beckn.Storage.Queries.Blacklist       as Blacklist
import qualified Beckn.Storage.Queries.Comment         as Comment
import qualified Beckn.Storage.Queries.Customer        as QC
import qualified Beckn.Storage.Queries.Document        as Document
import qualified Beckn.Storage.Queries.EntityDocument  as EntityDocument
import qualified Beckn.Storage.Queries.EntityTag       as EntityTag
import qualified Beckn.Storage.Queries.Location        as Location
import qualified Beckn.Storage.Queries.Organization    as QO
import qualified Beckn.Storage.Queries.Tag             as Tag
import qualified Beckn.Types.API.Organization          as API
import           Beckn.Types.App
import qualified Beckn.Types.Common                    as Location (Location (..),
                                                                    LocationType (..))
import qualified Beckn.Types.Storage.Document          as Document
import qualified Beckn.Types.Storage.EntityDocument    as EntityDocument
import qualified Beckn.Types.Storage.EntityTag         as EntityTag
import qualified Beckn.Types.Storage.Location          as SL
import           Beckn.Types.Storage.Organization
import qualified Beckn.Types.Storage.RegistrationToken as SR
import qualified Beckn.Types.Storage.Tag               as Tag
import           Beckn.Utils.Extra
import           Beckn.Utils.Routes
import           Beckn.Utils.Storage
import           Data.Aeson
import qualified EulerHS.Language                      as L
import           EulerHS.Prelude
import           Servant

createOrganization ::
     Maybe Text -> API.CreateOrganizationReq -> FlowHandler API.OrganizationRes
createOrganization regToken req =
  withFlowHandler $ do
    reg <- verifyToken regToken
    customer <- QC.findCustomerById (CustomerId $ SR._EntityId reg)
    uuid <- L.generateGUID
    now <- getCurrentTimeUTC
    let org =
          Organization
            (OrganizationId uuid)
            (req ^. Lens.name)
            (req ^. Lens.gstin)
            PENDING_VERIFICATION
            False
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            (req ^. Lens.city)
            (req ^. Lens.state)
            (req ^. Lens.country)
            (req ^. Lens.pincode)
            (req ^. Lens.address)
            Nothing
            now
            now
    QO.create org
    QC.updateCustomerOrgId (OrganizationId uuid) (CustomerId $ SR._EntityId reg)
    return $ API.OrganizationRes org

getOrganization :: Maybe Text -> Text -> FlowHandler API.GetOrganizationRes
getOrganization regToken orgId =
  withFlowHandler $ do
    regToken <- verifyToken regToken
    QO.findOrganizationById (OrganizationId orgId) >>=
      maybe
        (L.throwException $ err400 {errBody = "INVALID_DATA"})
        (\org-> do
          orgInfo <- getOrgInfo  org
          pure $ API.GetOrganizationRes orgInfo)

listOrganization ::
  Maybe Text
  -> Maybe Int
  -> Maybe Int
  -> [Location.LocationType]
  -> [Int]
  -> [Text]
  -> [Text]
  -> [Text]
  -> [Text]
  -> [Status]
  -> Maybe Bool
  -> FlowHandler API.ListOrganizationRes
listOrganization regToken limitM offsetM locationTypes pincodes cities districts wards states statuses verifiedM = withFlowHandler $ do
  verifyToken regToken
  organizations  <- QO.listOrganizations limitM offsetM locationTypes pincodes cities districts wards states statuses verifiedM
  orgInfo  <- (traverse getOrgInfo organizations)
  pure $ API.ListOrganizationRes {_organizations = orgInfo}

getOrgInfo :: Organization -> L.Flow API.OrgInfo
getOrgInfo Organization {..} = do
  entityDocs <- EntityDocument.findAllByOrgId _id
  let docIds = EntityDocument._DocumentId <$> entityDocs
  docs <- catMaybes <$> (traverse (Document.findById) (DocumentId <$> docIds))
  entityTags <- EntityTag.findAllByEntity "ORGANIZATION" $ _getOrganizationId _id
  let tagIds = EntityTag._TagId <$> entityTags
  tags <- catMaybes <$> (traverse (Tag.findById) (TagId <$> tagIds))
  comments <- Comment.findAllByCommentedOnEntity "ORGANIZATION" $ _getOrganizationId _id
  isBlacklistedOrg <- isJust <$> Blacklist.findByOrgId _id
  let locType = fromMaybe Location.PINCODE _locationType
  let toLocation = Location.Location
                  { _type     = locType
                  , _lat      = _lat
                  , _long     = _long
                  , _ward     = _ward
                  , _district = _district
                  , _city     = Just _city
                  , _state    = Just _state
                  , _country  = Just _country
                  , _pincode  = Just _pincode
                  , _address  = Just _address
                  , _bound    = _bound
                  }

  locationM <- Location.findByLocation locType _district (Just _city) (Just _state) (Just _country) (_ward) (Just _pincode)
  isBlacklistedLocation <- maybe (pure False) (\loc-> (isJust <$> Blacklist.findByLocationId (SL._id loc))) $ locationM
  pure API.OrgInfo
    { _Tags = tags
    , _Documents  = docs
    , _Comments = comments
    , _isBlacklistedOrganization = isBlacklistedOrg
    , _isBlacklistedLocation = isBlacklistedLocation
    , _location = toLocation
    ,..
    }

updateOrganization ::
     Maybe Text -> Text -> API.UpdateOrganizationReq -> FlowHandler API.OrganizationRes
updateOrganization regToken orgId API.UpdateOrganizationReq{..} = withFlowHandler $ do
  verifyToken regToken
  QO.update (OrganizationId orgId) _status
  QO.findOrganizationById (OrganizationId orgId)
  >>= \case
    Just v -> return $ API.OrganizationRes v
    Nothing -> L.throwException $ err400 {errBody = "Organization not found"}

