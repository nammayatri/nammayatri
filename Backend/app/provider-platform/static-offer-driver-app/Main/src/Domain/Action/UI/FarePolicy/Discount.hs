{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

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

import Data.OpenApi (ToSchema)
import qualified Domain.Types.FarePolicy.Discount as DFPDiscount
import qualified Domain.Types.FarePolicy.FareProduct as DFProduct
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Vehicle as Veh
import EulerHS.Prelude hiding (id)
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Storage.Hedis (HedisFlow)
import Kernel.Types.APISuccess
import Kernel.Types.Common
import Kernel.Types.Id (Id (..))
import Kernel.Types.Predicate
import Kernel.Utils.Common
import Kernel.Utils.Validation
import SharedLogic.TransporterConfig
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
  forall m r.
  ( HasCacheConfig r,
    EsqDBFlow m r,
    HedisFlow m r,
    CoreMetrics m
  ) =>
  SP.Person ->
  CreateFarePolicyDiscountReq ->
  m CreateFarePolicyDiscountRes
createFarePolicyDiscount admin req = do
  let merchantId = admin.merchantId
  runRequestValidation validateCreateFarePolicyDiscountReq req
  discounts <- QDisc.findAllByMerchantIdAndVariant merchantId req.vehicleVariant
  when (req.enabled && any (.enabled) discounts) $ throwError FPDiscountAlreadyEnabled
  disc <- buildDiscount merchantId
  cooridinators <- QP.findAdminsByMerchantId merchantId (Proxy @m)
  Esq.runTransaction $ QDisc.create @m disc
  QDisc.clearCache disc
  let otherCoordinators = filter ((/= admin.id) . (.id)) cooridinators
  fcmConfig <- findFCMConfigByMerchantId merchantId
  for_ otherCoordinators $ \cooridinator ->
    Notify.notifyDiscountChange fcmConfig cooridinator.id cooridinator.deviceToken
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
  forall m r.
  ( HasCacheConfig r,
    EsqDBFlow m r,
    HedisFlow m r,
    CoreMetrics m
  ) =>
  SP.Person ->
  Id DFPDiscount.Discount ->
  UpdateFarePolicyDiscountReq ->
  m UpdateFarePolicyDiscountRes
updateFarePolicyDiscount admin discId req = do
  let merchantId = admin.merchantId
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
  cooridinators <- QP.findAdminsByMerchantId merchantId (Proxy @m)
  Esq.runTransaction $ QDisc.update @m updatedFarePolicy
  let otherCoordinators = filter ((/= admin.id) . (.id)) cooridinators
  fcmConfig <- findFCMConfigByMerchantId merchantId
  for_ otherCoordinators $ \cooridinator ->
    Notify.notifyDiscountChange fcmConfig cooridinator.id cooridinator.deviceToken
  logTagInfo ("orgAdmin-" <> getId admin.id <> " -> updateFarePolicyDiscount : ") (show updatedFarePolicy)
  pure Success

deleteFarePolicyDiscount ::
  forall m r.
  ( HasCacheConfig r,
    EsqDBFlow m r,
    HedisFlow m r,
    CoreMetrics m
  ) =>
  SP.Person ->
  Id DFPDiscount.Discount ->
  m UpdateFarePolicyDiscountRes
deleteFarePolicyDiscount admin discId = do
  let merchantId = admin.merchantId
  discount <- QDisc.findById discId >>= fromMaybeM FPDiscountDoesNotExist
  unless (discount.merchantId == merchantId) $ throwError AccessDenied
  cooridinators <- QP.findAdminsByMerchantId merchantId (Proxy @m)
  Esq.runTransaction $ QDisc.delete @m discount
  let otherCoordinators = filter ((/= admin.id) . (.id)) cooridinators
  fcmConfig <- findFCMConfigByMerchantId merchantId
  for_ otherCoordinators $ \cooridinator ->
    Notify.notifyDiscountChange fcmConfig cooridinator.id cooridinator.deviceToken
  logTagInfo ("orgAdmin-" <> getId admin.id <> " -> deleteFarePolicyDiscount : ") (show discount)
  pure Success
