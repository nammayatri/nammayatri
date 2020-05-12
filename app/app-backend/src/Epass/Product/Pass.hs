module Epass.Product.Pass where

import qualified Beckn.Types.Storage.Case as SC
import qualified Beckn.Types.Storage.CaseProduct as SCP
import qualified Beckn.Types.Storage.Person as Person
import qualified Beckn.Types.Storage.Products as SP
import qualified Beckn.Types.Storage.RegistrationToken as RegistrationToken
import Data.Aeson
import qualified Epass.Data.Accessor as Accessor
import qualified Epass.Storage.Queries.Comment as Comment
import qualified Epass.Storage.Queries.Customer as Customer
import qualified Epass.Storage.Queries.CustomerDetail as QCD
import qualified Epass.Storage.Queries.Document as Document
import qualified Epass.Storage.Queries.EntityDocument as EntityDocument
import qualified Epass.Storage.Queries.EntityTag as EntityTag
import qualified Epass.Storage.Queries.Organization as Organization
import qualified Epass.Storage.Queries.Pass as QP
import qualified Epass.Storage.Queries.PassApplication as PassApplication
import qualified Epass.Storage.Queries.Tag as Tag
import qualified Epass.Storage.Queries.User as User
import Epass.Types.API.Pass
import Epass.Types.App
import Epass.Types.Common
import qualified Epass.Types.Common as Location (Location (..))
import qualified Epass.Types.Storage.Customer as Customer
import qualified Epass.Types.Storage.CustomerDetail as SCD
import qualified Epass.Types.Storage.Document as Document
import qualified Epass.Types.Storage.EntityDocument as EntityDocument
import qualified Epass.Types.Storage.EntityTag as EntityTag
import Epass.Types.Storage.Pass
import qualified Epass.Types.Storage.PassApplication as PassApplication
import Epass.Utils.Common
import Epass.Utils.Routes
import Epass.Utils.Storage
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (pass)
import Servant
import qualified Storage.Queries.Case as QC
import qualified Storage.Queries.CaseProduct as QCP
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.Products as QProd

getPassById :: Maybe Text -> Text -> FlowHandler PassRes
getPassById regToken passId =
  withFlowHandler $ do
    reg <- verifyToken regToken
    caseProduct <- QCP.findByProductId (ProductsId passId)
    case' <- QC.findById (SCP._caseId caseProduct)
    product <- QProd.findById (ProductsId passId)
    PassRes <$> getPassInfo case' product caseProduct

updatePass :: Maybe Text -> Text -> UpdatePassReq -> FlowHandler PassRes
updatePass regToken passId UpdatePassReq {..} = withFlowHandler $ do
  RegistrationToken.RegistrationToken {..} <- verifyToken regToken
  pass <- QProd.findById (ProductsId passId)
  pass' <- case _entityType of
    RegistrationToken.USER -> do
      when
        (isNothing _action)
        (L.throwException $ err400 {errBody = "Status update cannot be empty"})
      when
        (isJust _CustomerId || isJust _fromLocation || isJust _toLocation)
        (L.throwException $ err400 {errBody = "Access denied"})
      QCP.updateStatus (ProductsId passId) (fromJust _action)
      return $ pass
    RegistrationToken.CUSTOMER -> do
      customer <- fromMaybeM500 "Could not find Customer" =<< Person.findById (PersonId _EntityId)
      case Person._role customer of
        Person.ADMIN -> do
          when
            (isNothing _CustomerId && isNothing _toLocation)
            (L.throwException $ err400 {errBody = "Can update customerId or toLocation"})
          when
            (isJust _action || isJust _fromLocation)
            (L.throwException $ err400 {errBody = "Access denied"})
          return $
            pass {SP._toLocation = _toLocation }
        Person.USER -> do
          when
            (isNothing _CustomerId && isNothing _fromLocation)
            (L.throwException $ err400 {errBody = "Can update customerId or fromLocation"})
          when
            (isJust _action || isJust _toLocation)
            (L.throwException $ err400 {errBody = "Access denied"})
          return $ pass { SP._fromLocation = _fromLocation }

  caseProduct <- QCP.findByProductId (ProductsId passId)
  case' <- QC.findById (SCP._caseId caseProduct)
  QProd.updateMultiple passId pass'
  PassRes <$> getPassInfo case' pass caseProduct

listPass ::
  Maybe Text ->
  PassIDType ->
  Text ->
  Maybe Int ->
  Maybe Int ->
  PassType ->
  FlowHandler ListPassRes
listPass regToken passIdType passV limitM offsetM passType =
  withFlowHandler $ do
    reg <- verifyToken regToken
    listBy <- getListBy
    passes <- maybe (return []) getPasses listBy
    undefined
  where
    --ListPassRes <$> traverse getPassInfo passes

    getListBy =
      case passIdType of
        ORGANIZATIONID ->
          return $ Just $ QP.ByOrganizationId (OrganizationId passV)
        PASSAPPLICATIONID ->
          return $ Just $ QP.ByApplicationId (PassApplicationId passV)
        CUSTOMERID -> return $ Just $ QP.ByCustomerId (CustomerId passV)
        MOBILENUMBER -> do
          detail <- QCD.findByIdentifier SCD.MOBILENUMBER passV
          return $ (QP.ByCustomerId . SCD._CustomerId) <$> detail
    getPasses listBy =
      case (toEnum <$> limitM, toEnum <$> offsetM) of
        (Just l, Just o) -> QP.listAllPassesWithOffset l o listBy []
        _ -> QP.listAllPasses listBy []

getPassInfo :: SC.Case -> SP.Products -> SCP.CaseProduct -> L.Flow PassInfo
getPassInfo case' prod caseProduct = do
  person <- sequence $ Person.findById <$> (SCP._personId caseProduct)
  org <- Organization.findOrganizationById (OrganizationId $ SP._organizationId prod)
  entityDocs <- EntityDocument.findAllByPassApplicationId (PassApplicationId $ _getCaseId $ SC._id case')
  let docIds = EntityDocument._DocumentId <$> entityDocs
  docs <- catMaybes <$> (traverse (Document.findById) (DocumentId <$> docIds))
  pure $
    PassInfo
      { _fromLocation = fromJust $ SP._fromLocation prod,
        _toLocation = fromJust $ SP._toLocation prod,
        _Customer = join person,
        _Documents = docs,
        _id = _getProductsId $ SP._id prod,
        _ShortId = "", -- TODO: short_id in product
        _TenantOrganizationId = Nothing,
        _status = SCP._status caseProduct,
        _fromDate = SP._startTime prod,
        _toDate = SP._validTill prod,
        _passType = INDIVIDUAL, -- TODO:remove this hardcoded
        _PassApplicationId = _getCaseId $ SC._id case',
        _CreatedBy = fromJust $ SC._requestor case',
        _Organization = org
      }

