module Product.FarePolicy.Discount where

import App.Types
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.APISuccess
import Beckn.Types.Id (Id (..))
import Beckn.Utils.Validation (runRequestValidation)
import qualified Domain.Types.FarePolicy.Discount as DFPDiscount
import qualified Domain.Types.FareProduct as DFProduct
import qualified Domain.Types.Organization as Org
import qualified Domain.Types.Person as SP
import EulerHS.Prelude
import qualified Storage.Queries.FarePolicy.Discount as QDisc
import qualified Storage.Queries.Person as QP
import Types.API.FarePolicy.Discount
import Types.Error
import Utils.Common (GuidLike (generateGUID), MonadFlow, MonadTime (getCurrentTime), fromMaybeM, throwError, withFlowHandlerAPI)
import qualified Utils.Notifications as Notify

createFarePolicyDiscount :: SP.Person -> CreateFarePolicyDiscountReq -> FlowHandler CreateFarePolicyDiscountRes
createFarePolicyDiscount admin req = withFlowHandlerAPI $ do
  let Just orgId = admin.organizationId
  runRequestValidation validateCreateFarePolicyDiscountReq req
  discounts <- QDisc.findAll orgId req.vehicleVariant
  when (req.enabled && any (.enabled) discounts) $ throwError FPDiscountAlreadyEnabled
  disc <- buildDiscount orgId
  cooridinators <- QP.findAdminsByOrgId orgId
  Esq.runTransaction $ QDisc.create disc
  let otherCoordinators = filter (\coordinator -> coordinator.id /= admin.id) cooridinators
  for_ otherCoordinators $ \cooridinator -> do
    Notify.notifyDiscountChange cooridinator.id cooridinator.deviceToken
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
            fareProductType = DFProduct.ONE_WAY,
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
  discounts <- QDisc.findAll orgId discount.vehicleVariant
  when (req.enabled && any (.enabled) (filter (\disc -> disc.id /= discId) discounts)) $ throwError FPDiscountAlreadyEnabled
  let updatedFarePolicy =
        discount{fromDate = req.fromDate,
                 toDate = req.toDate,
                 discount = toRational req.discount,
                 enabled = req.enabled
                }
  cooridinators <- QP.findAdminsByOrgId orgId
  Esq.runTransaction $ QDisc.update discId updatedFarePolicy
  let otherCoordinators = filter (\coordinator -> coordinator.id /= admin.id) cooridinators
  for_ otherCoordinators $ \cooridinator -> do
    Notify.notifyDiscountChange cooridinator.id cooridinator.deviceToken
  pure Success

deleteFarePolicyDiscount :: SP.Person -> Id DFPDiscount.Discount -> FlowHandler UpdateFarePolicyDiscountRes
deleteFarePolicyDiscount admin discId = withFlowHandlerAPI $ do
  let Just orgId = admin.organizationId
  discount <- QDisc.findById discId >>= fromMaybeM FPDiscountDoesNotExist
  unless (discount.organizationId == orgId) $ throwError AccessDenied
  cooridinators <- QP.findAdminsByOrgId orgId
  Esq.runTransaction $ QDisc.deleteById discId
  let otherCoordinators = filter (\coordinator -> coordinator.id /= admin.id) cooridinators
  for_ otherCoordinators $ \cooridinator -> do
    Notify.notifyDiscountChange cooridinator.id cooridinator.deviceToken
  pure Success
