module Domain.Action.UI.FarePolicy
  ( ListFarePolicyRes (..),
    UpdateFarePolicyReq (..),
    UpdateFarePolicyRes,
    listFarePolicies,
    updateFarePolicy,
  )
where

import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Storage.Hedis
import Beckn.Types.APISuccess
import Beckn.Types.Id (Id (..))
import Beckn.Types.Predicate
import Beckn.Utils.Common
import Beckn.Utils.Validation
import Data.Time
import Domain.Types.FarePolicy
import qualified Domain.Types.FarePolicy as DFarePolicy
import qualified Domain.Types.Person as SP
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.FarePolicy as SFarePolicy
import qualified Storage.Queries.Person as QP
import Tools.Error
import Tools.Metrics
import qualified Tools.Notifications as Notify

newtype ListFarePolicyRes = ListFarePolicyRes
  { oneWayFarePolicies :: [FarePolicyAPIEntity]
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data UpdateFarePolicyReq = UpdateFarePolicyReq
  { baseDistanceFare :: HighPrecMoney,
    baseDistance :: Meters,
    perExtraKmFare :: HighPrecMoney,
    deadKmFare :: Money, -- constant value
    driverMinExtraFee :: Money,
    driverMaxExtraFee :: Money,
    nightShiftStart :: Maybe TimeOfDay,
    nightShiftEnd :: Maybe TimeOfDay,
    nightShiftRate :: Maybe Double
  }
  deriving (Generic, Show, FromJSON, ToSchema)

type UpdateFarePolicyRes = APISuccess

validateUpdateFarePolicyRequest :: Validate UpdateFarePolicyReq
validateUpdateFarePolicyRequest UpdateFarePolicyReq {..} =
  sequenceA_ --FIXME: ask for lower and upper bounds for all the values
    [ validateField "baseDistanceFare" baseDistanceFare $ InRange @HighPrecMoney 0 500,
      validateField "baseDistance" baseDistance $ InRange @Meters 0 500,
      validateField "perExtraKmFare" perExtraKmFare $ InRange @HighPrecMoney 0 500,
      validateField "deadKmFare" deadKmFare $ InRange @Money 0 500,
      validateField "driverMinExtraFee" driverMinExtraFee $ InRange @Money 0 500,
      validateField "driverMaxExtraFee" driverMaxExtraFee $ InRange @Money driverMinExtraFee 500,
      validateField "nightShiftRate" nightShiftRate . InMaybe $ InRange @Double 1 2,
      validateField "nightShiftStart" nightShiftStart . InMaybe $ InRange (TimeOfDay 18 0 0) (TimeOfDay 23 30 0),
      validateField "nightShiftEnd" nightShiftEnd . InMaybe $ InRange (TimeOfDay 0 30 0) (TimeOfDay 7 0 0)
    ]

listFarePolicies :: (HasCacheConfig r, EsqDBFlow m r, HedisFlow m r) => SP.Person -> m ListFarePolicyRes
listFarePolicies person = do
  orgId <- person.organizationId & fromMaybeM (PersonFieldNotPresent "organizationId")
  oneWayFarePolicies <- SFarePolicy.findAllByOrgId orgId
  pure $
    ListFarePolicyRes
      { oneWayFarePolicies = map makeFarePolicyAPIEntity oneWayFarePolicies
      }

updateFarePolicy :: (HasCacheConfig r, EsqDBFlow m r, FCMFlow m r, CoreMetrics m, HedisFlow m r) => SP.Person -> Id DFarePolicy.FarePolicy -> UpdateFarePolicyReq -> m UpdateFarePolicyRes
updateFarePolicy admin fpId req = do
  runRequestValidation validateUpdateFarePolicyRequest req
  farePolicy <- SFarePolicy.findById fpId >>= fromMaybeM NoFarePolicy
  unless (admin.organizationId == Just farePolicy.organizationId) $ throwError AccessDenied
  let updatedFarePolicy =
        farePolicy
          { baseDistanceFare = req.baseDistanceFare,
            baseDistanceMeters = req.baseDistance,
            perExtraKmFare = req.perExtraKmFare,
            deadKmFare = req.deadKmFare,
            driverExtraFee =
              ExtraFee
                { minFee = req.driverMinExtraFee,
                  maxFee = req.driverMaxExtraFee
                },
            nightShiftStart = req.nightShiftStart,
            nightShiftEnd = req.nightShiftEnd,
            nightShiftRate = req.nightShiftRate
          } ::
          DFarePolicy.FarePolicy
  let Just orgId = admin.organizationId
  coordinators <- QP.findAdminsByOrgId orgId
  Esq.runTransaction $
    SFarePolicy.update updatedFarePolicy
  SFarePolicy.clearCache updatedFarePolicy
  let otherCoordinators = filter (\coordinator -> coordinator.id /= admin.id) coordinators
  for_ otherCoordinators $ \cooridinator -> do
    Notify.notifyFarePolicyChange cooridinator.id cooridinator.deviceToken
  logTagInfo ("orgAdmin-" <> getId admin.id <> " -> updateFarePolicy : ") (show updatedFarePolicy)
  pure Success
