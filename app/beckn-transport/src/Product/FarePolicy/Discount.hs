module Product.FarePolicy.Discount where

import App.Types
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.APISuccess
import Beckn.Types.Id (Id (..))
import Beckn.Utils.Validation (runRequestValidation)
import EulerHS.Prelude
import qualified Storage.Queries.FarePolicy.Discount as QDisc
import Types.API.FarePolicy.Discount
import qualified Types.Domain.FarePolicy.Discount as DFPDiscount
import Types.Error
import qualified Types.Storage.Organization as Org
import qualified Types.Storage.Person as SP
import Utils.Common (GuidLike (generateGUID), MonadFlow, MonadTime (getCurrentTime), fromMaybeM, throwError, withFlowHandlerAPI)

createFarePolicyDiscount :: SP.Person -> CreateFarePolicyDiscountReq -> FlowHandler CreateFarePolicyDiscountRes
createFarePolicyDiscount admin req = withFlowHandlerAPI $ do
  let Just orgId = admin.organizationId
  runRequestValidation validateCreateFarePolicyDiscountReq req
  disc <- buildDiscount orgId
  DB.runSqlDBTransaction $ QDisc.create disc
  pure Success
  where
    buildDiscount :: MonadFlow m => Id Org.Organization -> m DFPDiscount.Discount
    buildDiscount orgId = do
      currTime <- getCurrentTime
      uuid <- generateGUID
      return $
        DFPDiscount.Discount
          { id = uuid,
            vehicleVariant = req.vehicleVariant,
            organizationId = orgId,
            fromDate = req.fromDate,
            toDate = req.toDate,
            discount = toRational req.discount,
            enabled = req.enabled,
            createdAt = currTime,
            updatedAt = currTime
          }

updateFarePolicyDiscount :: SP.Person -> Id DFPDiscount.Discount -> UpdateFarePolicyDiscountReq -> FlowHandler UpdateFarePolicyDiscountRes
updateFarePolicyDiscount admin discId req = withFlowHandlerAPI $ do
  let Just orgId = admin.organizationId
  runRequestValidation validateUpdateFarePolicyDiscountReq req
  discount <- QDisc.findById discId >>= fromMaybeM FPDiscountDoesNotExist
  unless (discount.organizationId == orgId) $ throwError AccessDenied
  let updatedFarePolicy =
        discount{fromDate = req.fromDate,
                 toDate = req.toDate,
                 discount = toRational req.discount,
                 enabled = req.enabled
                }
  DB.runSqlDBTransaction $ QDisc.update discId updatedFarePolicy
  pure Success

deleteFarePolicyDiscount :: SP.Person -> Id DFPDiscount.Discount -> FlowHandler UpdateFarePolicyDiscountRes
deleteFarePolicyDiscount admin discId = withFlowHandlerAPI $ do
  let Just orgId = admin.organizationId
  discount <- QDisc.findById discId >>= fromMaybeM FPDiscountDoesNotExist
  unless (discount.organizationId == orgId) $ throwError AccessDenied
  DB.runSqlDBTransaction $ QDisc.deleteById discId
  pure Success
