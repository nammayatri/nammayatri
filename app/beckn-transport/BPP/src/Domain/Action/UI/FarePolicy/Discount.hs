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
import Beckn.Storage.Hedis (HedisFlow)
import Beckn.Types.APISuccess
import Beckn.Types.Common
import Beckn.Types.Id (Id (..))
import Beckn.Types.Predicate
import Beckn.Utils.Common
import Beckn.Utils.Validation
import Data.OpenApi (ToSchema)
import qualified Domain.Types.FarePolicy.Discount as DFPDiscount
import qualified Domain.Types.FarePolicy.FareProduct as DFProduct
import qualified Domain.Types.Merchant as DM
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

createFarePolicyDiscount ::
  ( HasCacheConfig r,
    EsqDBFlow m r,
    HedisFlow m r,
    FCMFlow m r,
    CoreMetrics m
  ) =>
  SP.Person ->
  CreateFarePolicyDiscountReq ->
  m CreateFarePolicyDiscountRes
createFarePolicyDiscount admin req = do
  merchantId <- admin.merchantId & fromMaybeM (PersonFieldNotPresent "merchantId")
  runRequestValidation validateCreateFarePolicyDiscountReq req
  discounts <- QDisc.findAllByMerchantIdAndVariant merchantId req.vehicleVariant
  when (req.enabled && any (.enabled) discounts) $ throwError FPDiscountAlreadyEnabled
  disc <- buildDiscount merchantId
  cooridinators <- QP.findAdminsByMerchantId merchantId
  Esq.runTransaction $ QDisc.create disc
  QDisc.clearCache disc
  let otherCoordinators = filter ((/= admin.id) . (.id)) cooridinators
  for_ otherCoordinators $ \cooridinator ->
    Notify.notifyDiscountChange cooridinator.id cooridinator.deviceToken
  logTagInfo ("orgAdmin-" <> getId admin.id <> " -> createFarePolicyDiscount : ") (show disc)
  pure Success
  where
    buildDiscount :: MonadFlow m => Id DM.Merchant -> m DFPDiscount.Discount
    buildDiscount merchantId = do
      currTime <- getCurrentTime
      uuid <- generateGUID
      return $
        DFPDiscount.Discount
          { id = uuid,
            vehicleVariant = req.vehicleVariant,
            merchantId = merchantId,
            fareProductType = DFProduct.ONE_WAY,
            fromDate = req.fromDate,
            toDate = req.toDate,
            discount = req.discount,
            enabled = req.enabled,
            createdAt = currTime,
            updatedAt = currTime
          }

updateFarePolicyDiscount ::
  ( HasCacheConfig r,
    EsqDBFlow m r,
    HedisFlow m r,
    FCMFlow m r,
    CoreMetrics m
  ) =>
  SP.Person ->
  Id DFPDiscount.Discount ->
  UpdateFarePolicyDiscountReq ->
  m UpdateFarePolicyDiscountRes
updateFarePolicyDiscount admin discId req = do
  merchantId <- admin.merchantId & fromMaybeM (PersonFieldNotPresent "merchantId")
  runRequestValidation validateUpdateFarePolicyDiscountReq req
  discount <- QDisc.findById discId >>= fromMaybeM FPDiscountDoesNotExist
  unless (discount.merchantId == merchantId) $ throwError AccessDenied
  discounts <- QDisc.findAllByMerchantIdAndVariant merchantId discount.vehicleVariant
  when (req.enabled && any (.enabled) (filter (\disc -> disc.id /= discId) discounts)) $ throwError FPDiscountAlreadyEnabled
  let updatedFarePolicy =
        discount{fromDate = req.fromDate,
                 toDate = req.toDate,
                 discount = req.discount,
                 enabled = req.enabled
                }
  cooridinators <- QP.findAdminsByMerchantId merchantId
  Esq.runTransaction $ QDisc.update updatedFarePolicy
  QDisc.clearCache updatedFarePolicy
  let otherCoordinators = filter ((/= admin.id) . (.id)) cooridinators
  for_ otherCoordinators $ \cooridinator ->
    Notify.notifyDiscountChange cooridinator.id cooridinator.deviceToken
  logTagInfo ("orgAdmin-" <> getId admin.id <> " -> updateFarePolicyDiscount : ") (show updatedFarePolicy)
  pure Success

deleteFarePolicyDiscount ::
  ( HasCacheConfig r,
    EsqDBFlow m r,
    HedisFlow m r,
    FCMFlow m r,
    CoreMetrics m
  ) =>
  SP.Person ->
  Id DFPDiscount.Discount ->
  m UpdateFarePolicyDiscountRes
deleteFarePolicyDiscount admin discId = do
  merchantId <- admin.merchantId & fromMaybeM (PersonFieldNotPresent "merchantId")
  discount <- QDisc.findById discId >>= fromMaybeM FPDiscountDoesNotExist
  unless (discount.merchantId == merchantId) $ throwError AccessDenied
  cooridinators <- QP.findAdminsByMerchantId merchantId
  Esq.runTransaction $ QDisc.deleteById discId
  QDisc.clearCache discount
  let otherCoordinators = filter ((/= admin.id) . (.id)) cooridinators
  for_ otherCoordinators $ \cooridinator ->
    Notify.notifyDiscountChange cooridinator.id cooridinator.deviceToken
  logTagInfo ("orgAdmin-" <> getId admin.id <> " -> deleteFarePolicyDiscount : ") (show discount)
  pure Success
