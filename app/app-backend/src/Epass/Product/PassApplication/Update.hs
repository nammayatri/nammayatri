module Epass.Product.PassApplication.Update where

import qualified Beckn.Types.Storage.Case              as Case
import qualified Beckn.Types.Storage.CaseProduct       as CaseProduct
import qualified Beckn.Types.Storage.Products          as Products

import qualified Beckn.Types.Storage.RegistrationToken as RegistrationToken
import           Data.Aeson
import qualified Data.Text                             as DT
import qualified Epass.Data.Accessor                   as Accessor
import qualified Epass.Storage.Queries.Pass            as Pass
import qualified Epass.Storage.Queries.PassApplication as DB
import           Epass.Types.API.PassApplication
import           Epass.Types.App
import           Epass.Types.Common
import qualified Epass.Types.Common                    as Location (Location (..),
                                                                    LocationType)
import qualified Epass.Types.Storage.Pass              as Pass
import           Epass.Types.Storage.PassApplication
import qualified Epass.Types.Storage.PassApplication   as PassApplication
import           Epass.Utils.Common
import           Epass.Utils.Routes
import           Epass.Utils.Storage
import qualified EulerHS.Language                      as L
import           EulerHS.Prelude
import qualified EulerHS.Types                         as T
import           Servant
import qualified Storage.Queries.Case                  as QC
import qualified Storage.Queries.CaseProduct           as QCP
import qualified Storage.Queries.Products              as QProd
import qualified Test.RandomStrings                    as RS

updatePassApplication ::
  Maybe Text ->
  CaseId ->
  UpdatePassApplicationReq ->
  FlowHandler PassApplicationRes'
updatePassApplication regToken caseId UpdatePassApplicationReq {..} = withFlowHandler $ do
  token <- verifyToken regToken
  allowOnlyUser token
  pA <- QC.findById caseId
  -- verifyIfStatusUpdatable (PassApplication._status pA) _status
  case _status of
    REVOKED -> do
  --     Pass.revokeByPassApplicationId passApplicationId
  --     -- _approvedCount remains unchanged as part of history
      -- TODO: mark passes(product) as revoked
      QC.updateStatus caseId Case.CLOSED
    APPROVED -> do
      when
        (isNothing _approvedCount)
        (L.throwException $ err400 {errBody = "Approved count cannot be empty"})
      let approvedCount = fromJust _approvedCount
      -- Create passes
      replicateM approvedCount (createPass pA)
      QC.updateStatus caseId Case.CONFIRMED
    PENDING -> QC.updateStatus caseId Case.INPROGRESS
    _ -> return ()
  QC.findById caseId
    >>= return . PassApplicationRes'
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

createPass :: Case.Case -> L.Flow Products.Products
createPass c@(Case.Case {..}) = do
  id <- generateGUID
  cpId <- generateGUID
  currTime <- getCurrTime
  let orgId = "" --TODO: this should be optional
      product =
       Products.Products
        { _id = ProductsId id
        , _createdAt = currTime
        , _updatedAt = currTime
        , _type = Products.PASS
        , _status = Products.CONFIRMED
        , _fromLocation = Just _fromLocationId
        , _toLocation = Just _toLocationId
        , _organizationId = orgId
        , _price = 0 -- TODO: this should be optional?
        , _rating = Nothing
        , _review = Nothing
        , ..
        }
      caseProduct =
         CaseProduct.CaseProduct
          { _id = CaseProductId cpId
            ,_caseId = _id
            ,_productId = ProductsId id
            ,_quantity = 0
            ,_price = 0.0
            ,_status = CaseProduct.CONFIRMED
            ,_info = Nothing
            ,_createdAt = currTime
            ,_updatedAt = currTime
          }
  QProd.create product
  QCP.create caseProduct
  return product




allowOnlyUser :: RegistrationToken.RegistrationToken -> L.Flow ()
allowOnlyUser RegistrationToken.RegistrationToken {..} =
  case _entityType of
    RegistrationToken.USER -> return ()
    RegistrationToken.CUSTOMER ->
      L.throwException $ err400 {errBody = "OPERATION_NOT_ALLOWED"}
