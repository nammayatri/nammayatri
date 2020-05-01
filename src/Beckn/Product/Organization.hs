module Beckn.Product.Organization where

import qualified Beckn.Data.Accessor                   as Lens
import qualified Beckn.Storage.Queries.Customer        as QC
import qualified Beckn.Storage.Queries.Organization    as QO
import           Beckn.Types.API.Organization
import           Beckn.Types.App
import           Beckn.Types.Common
import qualified Beckn.Types.Storage.Organization      as SO
import qualified Beckn.Types.Storage.RegistrationToken as SR
import           Beckn.Utils.Extra
import           Beckn.Utils.Routes
import           Beckn.Utils.Storage
import           Data.Aeson
import qualified EulerHS.Language                      as L
import           EulerHS.Prelude
import           Servant

createOrganization ::
     Maybe Text -> CreateOrganizationReq -> FlowHandler OrganizationRes
createOrganization regToken req =
  withFlowHandler $ do
    reg <- verifyToken regToken
    customer <- QC.findCustomerById (CustomerId $ SR._EntityId reg)
    uuid <- L.generateGUID
    now <- getCurrentTimeUTC
    let org =
          SO.Organization
            (OrganizationId uuid)
            (req ^. Lens.name)
            (req ^. Lens.gstin)
            SO.PENDING_VERIFICATION
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
    return $ OrganizationRes org

getOrganization :: Maybe Text -> Text -> FlowHandler OrganizationRes
getOrganization regToken orgId =
  withFlowHandler $ do
    regToken <- verifyToken regToken
    QO.findOrganizationById (OrganizationId orgId) >>=
      maybe
        (L.throwException $ err400 {errBody = "INVALID_DATA"})
        (return . OrganizationRes)

listOrganization ::
  Maybe Text
  -> Maybe Int
  -> Maybe Int
  -> [LocationType]
  -> [Int]
  -> [Text]
  -> [Text]
  -> [Text]
  -> [Text]
  -> [SO.Status]
  -> Maybe Bool
  -> FlowHandler ListOrganizationRes
listOrganization regToken limitM offsetM locationTypes pincodes cities districts wards states statuses verifiedM = withFlowHandler $ do
  verifyToken regToken
  organizations <- QO.listOrganizations limitM offsetM locationTypes pincodes cities districts wards states statuses verifiedM
  pure $ ListOrganizationRes {organizations = organizations}

updateOrganization ::
     Maybe Text -> Text -> UpdateOrganizationReq -> FlowHandler OrganizationRes
updateOrganization regToken orgId req = undefined
