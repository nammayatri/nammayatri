module Epass.Product.Organization where

import qualified Beckn.Types.Storage.Location as BTL
import qualified Beckn.Types.Storage.Person as SP
import qualified Beckn.Types.Storage.RegistrationToken as SR
import Beckn.Utils.Common
import Beckn.Utils.Extra
import Data.Aeson
import qualified Epass.Data.Accessor as Lens
import qualified Epass.Storage.Queries.Blacklist as Blacklist
import qualified Epass.Storage.Queries.Comment as Comment
import qualified Epass.Storage.Queries.Document as Document
import qualified Epass.Storage.Queries.EntityDocument as EntityDocument
import qualified Epass.Storage.Queries.EntityTag as EntityTag
import qualified Epass.Storage.Queries.Location as Location
import qualified Epass.Storage.Queries.Organization as QO
import qualified Epass.Storage.Queries.Tag as Tag
import qualified Epass.Types.API.Organization as API
import Epass.Types.App
import qualified Epass.Types.Common as Location (Location (..))
import qualified Epass.Types.Storage.Document as Document
import qualified Epass.Types.Storage.EntityDocument as EntityDocument
import qualified Epass.Types.Storage.EntityTag as EntityTag
import qualified Epass.Types.Storage.Location as SL
import Epass.Types.Storage.Organization
import qualified Epass.Types.Storage.Tag as Tag
import Epass.Utils.Storage
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Servant
import qualified Storage.Queries.Person as QP

createOrganization ::
  RegToken -> API.CreateOrganizationReq -> FlowHandler API.OrganizationRes
createOrganization regToken req =
  withFlowHandler $ do
    reg <- verifyToken regToken
    customer <- QP.findById (PersonId $ SR._EntityId reg)
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
    QP.updatePersonOrgId uuid (PersonId $ SR._EntityId reg)
    return $ API.OrganizationRes org

getOrganization :: RegToken -> Text -> FlowHandler API.GetOrganizationRes
getOrganization regToken orgId =
  withFlowHandler $ do
    reg <- verifyToken regToken
    when (SR._entityType reg == SR.CUSTOMER) $ do
      customer <-
        QP.findById (PersonId $ SR._EntityId reg)
          >>= fromMaybeM400 "INVALID_DATA"
      unless (Just orgId == SP._organizationId customer)
        $ L.throwException
        $ err400 {errBody = "INVALID_DATA"}
    QO.findOrganizationById (OrganizationId orgId)
      >>= maybe
        (L.throwException $ err400 {errBody = "INVALID_DATA"})
        ( \org -> do
            orgInfo <- getOrgInfo org
            pure $ API.GetOrganizationRes orgInfo
        )

listOrganization ::
  RegToken -> API.ListOrganizationReq -> FlowHandler API.ListOrganizationRes
listOrganization regToken API.ListOrganizationReq {..} = withFlowHandler $ do
  L.logInfo "list organization" "invoked"
  reg <- verifyToken regToken
  when (SR._entityType reg == SR.CUSTOMER)
    $ L.throwException
    $ err400 {errBody = "UNAUTHORIZED"}
  organizations <- QO.listOrganizations limit offset locationType pincode city district ward state status verified
  orgInfo <- traverse getOrgInfo organizations
  pure $ API.ListOrganizationRes {_organizations = orgInfo}

getOrgInfo :: Organization -> L.Flow API.OrgInfo
getOrgInfo Organization {..} = do
  entityDocs <- EntityDocument.findAllByOrgId _id
  let docIds = EntityDocument._DocumentId <$> entityDocs
  docs <- catMaybes <$> traverse Document.findById (DocumentId <$> docIds)
  entityTags <- EntityTag.findAllByEntity "ORGANIZATION" $ _getOrganizationId _id
  let tagIds = EntityTag._TagId <$> entityTags
  tags <- catMaybes <$> traverse Tag.findById (TagId <$> tagIds)
  comments <- Comment.findAllByCommentedOnEntity "ORGANIZATION" $ _getOrganizationId _id
  isBlacklistedOrg <- isJust <$> Blacklist.findByOrgId _id
  let locType = fromMaybe BTL.PINCODE _locationType
  let toLocation =
        Location.Location
          { _type = locType,
            _lat = _lat,
            _long = _long,
            _ward = _ward,
            _district = _district,
            _city = Just _city,
            _state = Just _state,
            _country = Just _country,
            _pincode = Just _pincode,
            _address = Just _address,
            _bound = _bound
          }
  locationM <-
    Location.findByLocation locType _district (Just _city) (Just _state) (Just _country) _ward (Just _pincode)
  isBlacklistedLocation <-
    maybe (pure False) (\loc -> isJust <$> Blacklist.findByLocationId (SL._id loc)) locationM
  pure
    API.OrgInfo
      { _Tags = tags,
        _Documents = docs,
        _Comments = comments,
        _isBlacklistedOrganization = isBlacklistedOrg,
        _isBlacklistedLocation = isBlacklistedLocation,
        _location = toLocation,
        ..
      }

updateOrganization ::
  RegToken -> Text -> API.UpdateOrganizationReq -> FlowHandler API.OrganizationRes
updateOrganization regToken orgId API.UpdateOrganizationReq {..} = withFlowHandler $
  do
    reg <- verifyToken regToken
    when (SR._entityType reg == SR.CUSTOMER)
      $ L.throwException
      $ err400 {errBody = "UNAUTHORIZED"}
    QO.update (OrganizationId orgId) _status
    QO.findOrganizationById (OrganizationId orgId)
    >>= \case
      Just v -> return $ API.OrganizationRes v
      Nothing -> L.throwException $ err400 {errBody = "Organization not found"}
