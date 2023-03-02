{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.FarePolicy.OneWayFarePolicy
  ( ListOneWayFarePolicyRes (..),
    UpdateOneWayFarePolicyReq (..),
    UpdateOneWayFarePolicyRes,
    listOneWayFarePolicies,
    updateOneWayFarePolicy,
  )
where

import Data.OpenApi (ToSchema)
import Data.Time (TimeOfDay (..))
import Domain.Types.FarePolicy.OneWayFarePolicy (OneWayFarePolicyAPIEntity, makeOneWayFarePolicyAPIEntity)
import qualified Domain.Types.FarePolicy.OneWayFarePolicy as DFarePolicy
import Domain.Types.FarePolicy.OneWayFarePolicy.PerExtraKmRate (PerExtraKmRateAPIEntity, validatePerExtraKmRateAPIEntity)
import qualified Domain.Types.FarePolicy.OneWayFarePolicy.PerExtraKmRate as DPerExtraKmRate
import qualified Domain.Types.Person as SP
import EulerHS.Prelude hiding (id)
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Storage.Hedis (HedisFlow)
import Kernel.Types.APISuccess
import Kernel.Types.Id (Id (..))
import Kernel.Types.Predicate
import Kernel.Utils.Common
import Kernel.Utils.Validation
import SharedLogic.TransporterConfig
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.FarePolicy.OneWayFarePolicy as SFarePolicy
import qualified Storage.Queries.Person as QP
import Tools.Error
import Tools.Metrics
import qualified Tools.Notifications as Notify

newtype ListOneWayFarePolicyRes = ListOneWayFarePolicyRes
  { oneWayFarePolicies :: [OneWayFarePolicyAPIEntity]
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data UpdateOneWayFarePolicyReq = UpdateOneWayFarePolicyReq
  { baseFare :: Maybe Money,
    perExtraKmRateList :: NonEmpty PerExtraKmRateAPIEntity,
    nightShiftStart :: Maybe TimeOfDay,
    nightShiftEnd :: Maybe TimeOfDay,
    nightShiftRate :: Maybe Centesimal
  }
  deriving (Generic, Show, FromJSON, ToSchema)

type UpdateOneWayFarePolicyRes = APISuccess

validateUpdateFarePolicyRequest :: Validate UpdateOneWayFarePolicyReq
validateUpdateFarePolicyRequest UpdateOneWayFarePolicyReq {..} =
  sequenceA_
    [ validateField "baseFare" baseFare . InMaybe $ InRange @Money 0 500,
      validateList "perExtraKmRateList" perExtraKmRateList validatePerExtraKmRateAPIEntity,
      validateField "perExtraKmRateList" perExtraKmRateList $ UniqueField @"distanceRangeStart",
      validateField "nightShiftRate" nightShiftRate . InMaybe $ InRange @Centesimal 1 2,
      validateField "nightShiftStart" nightShiftStart . InMaybe $ InRange (TimeOfDay 18 0 0) (TimeOfDay 23 30 0),
      validateField "nightShiftEnd" nightShiftEnd . InMaybe $ InRange (TimeOfDay 0 30 0) (TimeOfDay 7 0 0)
    ]

listOneWayFarePolicies :: (CacheFlow m r, EsqDBFlow m r) => SP.Person -> m ListOneWayFarePolicyRes
listOneWayFarePolicies person = do
  oneWayFarePolicies <- SFarePolicy.findAllByMerchantId person.merchantId
  pure $
    ListOneWayFarePolicyRes
      { oneWayFarePolicies = map makeOneWayFarePolicyAPIEntity oneWayFarePolicies
      }

updateOneWayFarePolicy ::
  forall m r.
  ( HasCacheConfig r,
    EsqDBFlow m r,
    HedisFlow m r,
    CoreMetrics m
  ) =>
  SP.Person ->
  Id DFarePolicy.OneWayFarePolicy ->
  UpdateOneWayFarePolicyReq ->
  m UpdateOneWayFarePolicyRes
updateOneWayFarePolicy admin fpId req = do
  runRequestValidation validateUpdateFarePolicyRequest req
  farePolicy <- SFarePolicy.findById fpId >>= fromMaybeM NoFarePolicy
  unless (admin.merchantId == farePolicy.merchantId) $ throwError AccessDenied
  let perExtraKmRateList = map DPerExtraKmRate.fromPerExtraKmRateAPIEntity req.perExtraKmRateList
  let updatedFarePolicy =
        farePolicy{baseFare = req.baseFare,
                   perExtraKmRateList = perExtraKmRateList,
                   nightShiftStart = req.nightShiftStart,
                   nightShiftEnd = req.nightShiftEnd,
                   nightShiftRate = req.nightShiftRate
                  }
  coordinators <- QP.findAdminsByMerchantId admin.merchantId (Proxy @m)
  Esq.runTransaction $
    SFarePolicy.update @m updatedFarePolicy
  let otherCoordinators = filter (\coordinator -> coordinator.id /= admin.id) coordinators
  fcmConfig <- findFCMConfigByMerchantId admin.merchantId
  for_ otherCoordinators $ \cooridinator -> do
    Notify.notifyFarePolicyChange fcmConfig cooridinator.id cooridinator.deviceToken
  logTagInfo ("orgAdmin-" <> getId admin.id <> " -> updateOneWayFarePolicy : ") (show updatedFarePolicy)
  pure Success
