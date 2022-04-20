module Product.FarePolicy.Rentals where

import App.Types
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.APISuccess
import Beckn.Types.Id (Id (..))
import Beckn.Utils.Validation (runRequestValidation)
import Domain.Types.Organization
import qualified Domain.Types.Person as SP
import Domain.Types.RentalFarePolicy as Domain
import EulerHS.Prelude
import qualified Storage.Queries.RentalFarePolicy as SRentalFarePolicy
import Types.API.FarePolicy.Rentals
import Types.Error
import Utils.Common

createRentalFarePolicy :: SP.Person -> CreateRentalFarePolicyReq -> FlowHandler APISuccess
createRentalFarePolicy admin req = withFlowHandlerAPI $ do
  orgId <- admin.organizationId & fromMaybeM (PersonFieldNotPresent "organizationId")
  mapM_ (runRequestValidation validateCreateRentalsFarePolicyRequest) req.createList
  newRentalFarePolicyItems <- forM req.createList $ \createItemReq -> do
    guid <- Id <$> generateGUID
    pure $ toDomainType orgId guid createItemReq
  Esq.runTransaction $ do
    SRentalFarePolicy.markAllAsDeleted orgId
    forM_ newRentalFarePolicyItems SRentalFarePolicy.create
  pure Success
  where
    toDomainType :: Id Organization -> Id RentalFarePolicy -> CreateRentalFarePolicyItem -> RentalFarePolicy
    toDomainType orgId guid CreateRentalFarePolicyItem {..} = do
      RentalFarePolicy
        { id = guid,
          organizationId = orgId,
          baseFare = realToFrac baseFare,
          extraKmFare = realToFrac extraKmFare,
          extraMinuteFare = realToFrac extraMinuteFare,
          driverAllowanceForDay = realToFrac <$> driverAllowanceForDay,
          ..
        }

listRentalFarePolicies :: SP.Person -> FlowHandler ListRentalFarePoliciesRes
listRentalFarePolicies person = withFlowHandlerAPI $ do
  orgId <- person.organizationId & fromMaybeM (PersonFieldNotPresent "organizationId")
  rentalFarePolicies <- SRentalFarePolicy.findRentalFarePoliciesByOrg orgId
  pure $
    ListRentalFarePoliciesRes
      { rentalFarePolicies = map makeRentalFarePolicyAPIEntity rentalFarePolicies
      }
