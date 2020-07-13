module Epass.Product.PassApplication.Update where

import Beckn.Types.Common
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Location as BTL
import qualified Beckn.Types.Storage.ProductInstance as ProductInstance
import qualified Beckn.Types.Storage.Products as Products
import qualified Beckn.Types.Storage.RegistrationToken as RegistrationToken
import Beckn.Utils.Common
import Beckn.Utils.Extra
import Data.Aeson
import qualified Data.Text as DT
import qualified Epass.Data.Accessor as Accessor
import qualified Epass.Storage.Queries.Pass as Pass
import qualified Epass.Storage.Queries.PassApplication as DB
import Epass.Types.API.PassApplication
import Epass.Types.App
import Epass.Types.Common
import qualified Epass.Types.Common as Location (Location (..))
import qualified Epass.Types.Storage.Pass as Pass
import Epass.Types.Storage.PassApplication
import qualified Epass.Types.Storage.PassApplication as PassApplication
import Epass.Utils.Storage
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified EulerHS.Types as T
import qualified Models.Case as MC
import Servant
import qualified Storage.Queries.Case as QC
import qualified Storage.Queries.ProductInstance as QCP
import qualified Storage.Queries.Products as QProd
import qualified Test.RandomStrings as RS

updatePassApplication ::
  RegToken ->
  CaseId ->
  UpdatePassApplicationReq ->
  FlowHandler PassApplicationRes'
updatePassApplication regToken caseId UpdatePassApplicationReq {..} = withFlowHandler $ do
  token <- verifyToken regToken
  allowOnlyUser token
  pA <- QC.findById caseId
  -- verifyIfStatusUpdatable (PassApplication._status pA) _status
  -- TODO It seems we do not need to check statuses transitions here
  case _status of
    REVOKED -> do
      QCP.updateAllProductInstByCaseId caseId ProductInstance.OUTOFSTOCK
      QC.updateStatusAndUdfs caseId Case.CLOSED Nothing Nothing Nothing Nothing _remarks
    APPROVED -> do
      when
        (isNothing _approvedCount)
        (L.throwException $ err400 {errBody = "Approved count cannot be empty"})
      let approvedCount = fromJust _approvedCount
      -- Create passes
      replicateM_ approvedCount (createPass pA)
      --TODO: should we need to update case product to CONFIRMED?
      QC.updateStatusAndUdfs
        caseId
        Case.COMPLETED
        Nothing
        Nothing
        Nothing
        (show <$> _approvedCount)
        _remarks
    PENDING -> QC.updateStatus caseId Case.INPROGRESS
  PassApplicationRes' <$> QC.findById caseId
  where
    validApprovedCount count approvedCount =
      if approvedCount > count then count else approvedCount

verifyIfStatusUpdatable :: Status -> Status -> L.Flow ()
verifyIfStatusUpdatable currStatus newStatus =
  case (currStatus, newStatus) of
    (PENDING, APPROVED) -> return ()
    (PENDING, REJECTED) -> return ()
    (PENDING, EXPIRED) -> return ()
    -- Blocked APPROVED going to REJECTED
    (APPROVED, EXPIRED) -> return ()
    (APPROVED, REVOKED) -> return ()
    _ -> L.throwException $ err400 {errBody = "Invalid status update"}

createPass :: Case.Case -> L.Flow ProductInstance.ProductInstance
createPass c@Case.Case {..} = do
  id <- generateGUID
  cpId <- generateGUID
  currTime <- getCurrentTimeUTC
  let orgId = "" --TODO: this should be optional
      productInstance =
        ProductInstance.ProductInstance
          { _id = ProductInstanceId id,
            _shortId = "",
            _caseId = _id,
            _productId = ProductsId cpId, --TODO need to be fixed
            _personId = Nothing,
            _quantity = 1,
            _entityType = ProductInstance.PASS,
            _status = ProductInstance.CONFIRMED,
            _parentId = Nothing,
            _entityId = Nothing,
            _price = 0,
            _udf1 = Nothing,
            _udf2 = Nothing,
            _udf3 = Nothing,
            _udf4 = Nothing,
            _udf5 = Nothing,
            _fromLocation = Just _fromLocationId,
            _toLocation = Just _toLocationId,
            _info = Nothing,
            _organizationId = orgId,
            _createdAt = currTime,
            _updatedAt = currTime,
            ..
          }
  QCP.create productInstance
  return productInstance

allowOnlyUser :: RegistrationToken.RegistrationToken -> L.Flow ()
allowOnlyUser RegistrationToken.RegistrationToken {..} =
  case _entityType of
    RegistrationToken.USER -> return ()
    RegistrationToken.CUSTOMER ->
      L.throwException $ err400 {errBody = "OPERATION_NOT_ALLOWED"}
