module Domain.Action.UI.FarePolicy.Discount
  ( UpdateFarePolicyDiscountReq,
    UpdateFarePolicyDiscountRes,
    CreateFarePolicyDiscountReq,
    CreateFarePolicyDiscountRes,
    DeleteFarePolicyDiscountRes,
    createFarePolicyDiscount,
    updateFarePolicyDiscount,
    deleteFarePolicyDiscount,
  )
where

import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Storage.Hedis
import Beckn.Types.APISuccess
import Beckn.Types.Id (Id (..))
import Beckn.Types.Predicate
import Beckn.Utils.Common
import Beckn.Utils.Validation
import Data.OpenApi (ToSchema)
import qualified Domain.Types.FarePolicy.Discount as DFPDiscount
import qualified Domain.Types.FarePolicy.FareProduct as DFProduct
import qualified Domain.Types.Organization as Org
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Vehicle as Veh
import EulerHS.Prelude hiding (id)
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.FarePolicy.Discount as QDisc
import qualified Storage.Queries.Person as QP
import Tools.Error
import Tools.Metrics
import qualified Tools.Notifications as Notify

data CreateFarePolicyDiscountReq = CreateFarePolicyDiscountReq
  { vehicleVariant :: Veh.Variant,
    fromDate :: UTCTime,
    toDate :: UTCTime,
    discount :: Money,
    enabled :: Bool
  }
  deriving (Generic, Show, FromJSON, ToSchema)

type CreateFarePolicyDiscountRes = APISuccess

validateCreateFarePolicyDiscountReq :: Validate CreateFarePolicyDiscountReq
validateCreateFarePolicyDiscountReq CreateFarePolicyDiscountReq {..} =
  sequenceA_
    [ validateField "discount" discount $ Min @Money 1,
      validateField "fromDate" fromDate $ Max @UTCTime toDate
    ]

data UpdateFarePolicyDiscountReq = UpdateFarePolicyDiscountReq
  { fromDate :: UTCTime,
    toDate :: UTCTime,
    discount :: Money,
    enabled :: Bool
  }
  deriving (Generic, Show, FromJSON, ToSchema)

type UpdateFarePolicyDiscountRes = APISuccess

validateUpdateFarePolicyDiscountReq :: Validate UpdateFarePolicyDiscountReq
validateUpdateFarePolicyDiscountReq UpdateFarePolicyDiscountReq {..} =
  sequenceA_
    [ validateField "discount" discount $ Min @Money 1,
      validateField "fromDate" fromDate $ Max @UTCTime toDate
    ]

type DeleteFarePolicyDiscountRes = APISuccess

createFarePolicyDiscount :: (HasCacheConfig r, HedisFlow m r, EsqDBFlow m r, FCMFlow m r, CoreMetrics m) => SP.Person -> CreateFarePolicyDiscountReq -> m CreateFarePolicyDiscountRes
createFarePolicyDiscount admin req = do
  orgId <- admin.organizationId & fromMaybeM (PersonFieldNotPresent "organizationId")
  runRequestValidation validateCreateFarePolicyDiscountReq req
  discounts <- QDisc.findAllByOrgIdAndVariant orgId req.vehicleVariant
  when (req.enabled && any (.enabled) discounts) $ throwError FPDiscountAlreadyEnabled
  disc <- buildDiscount orgId
  cooridinators <- QP.findAdminsByOrgId orgId
  Esq.runTransaction $ QDisc.create disc
  QDisc.clearCache disc
  let otherCoordinators = filter ((/= admin.id) . (.id)) cooridinators
  for_ otherCoordinators $ \cooridinator ->
    Notify.notifyDiscountChange cooridinator.id cooridinator.deviceToken
  logTagInfo ("orgAdmin-" <> getId admin.id <> " -> createFarePolicyDiscount : ") (show disc)
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
            discount = req.discount,
            enabled = req.enabled,
            createdAt = currTime,
            updatedAt = currTime
          }

updateFarePolicyDiscount :: (HasCacheConfig r, HedisFlow m r, EsqDBFlow m r, FCMFlow m r, CoreMetrics m) => SP.Person -> Id DFPDiscount.Discount -> UpdateFarePolicyDiscountReq -> m UpdateFarePolicyDiscountRes
updateFarePolicyDiscount admin discId req = do
  orgId <- admin.organizationId & fromMaybeM (PersonFieldNotPresent "organizationId")
  runRequestValidation validateUpdateFarePolicyDiscountReq req
  discount <- QDisc.findById discId >>= fromMaybeM FPDiscountDoesNotExist
  unless (discount.organizationId == orgId) $ throwError AccessDenied
  discounts <- QDisc.findAllByOrgIdAndVariant orgId discount.vehicleVariant
  when (req.enabled && any (.enabled) (filter (\disc -> disc.id /= discId) discounts)) $ throwError FPDiscountAlreadyEnabled
  let updatedFarePolicy =
        discount{fromDate = req.fromDate,
                 toDate = req.toDate,
                 discount = req.discount,
                 enabled = req.enabled
                }
  cooridinators <- QP.findAdminsByOrgId orgId
  Esq.runTransaction $ QDisc.update updatedFarePolicy
  QDisc.clearCache updatedFarePolicy
  let otherCoordinators = filter ((/= admin.id) . (.id)) cooridinators
  for_ otherCoordinators $ \cooridinator ->
    Notify.notifyDiscountChange cooridinator.id cooridinator.deviceToken
  logTagInfo ("orgAdmin-" <> getId admin.id <> " -> updateFarePolicyDiscount : ") (show updatedFarePolicy)
  pure Success

deleteFarePolicyDiscount :: (HasCacheConfig r, HedisFlow m r, EsqDBFlow m r, FCMFlow m r, CoreMetrics m) => SP.Person -> Id DFPDiscount.Discount -> m UpdateFarePolicyDiscountRes
deleteFarePolicyDiscount admin discId = do
  orgId <- admin.organizationId & fromMaybeM (PersonFieldNotPresent "organizationId")
  discount <- QDisc.findById discId >>= fromMaybeM FPDiscountDoesNotExist
  unless (discount.organizationId == orgId) $ throwError AccessDenied
  cooridinators <- QP.findAdminsByOrgId orgId
  Esq.runTransaction $ QDisc.deleteById discId
  QDisc.clearCache discount
  let otherCoordinators = filter ((/= admin.id) . (.id)) cooridinators
  for_ otherCoordinators $ \cooridinator ->
    Notify.notifyDiscountChange cooridinator.id cooridinator.deviceToken
  logTagInfo ("orgAdmin-" <> getId admin.id <> " -> deleteFarePolicyDiscount : ") (show discount)
  pure Success
