module Domain.Action.Dashboard.Operator.FleetManagement (getFleetManagementFleets) where

import qualified API.Types.ProviderPlatform.Operator.FleetManagement as Common
import Control.Monad.Extra (mapMaybeM)
import Domain.Types.FleetOperatorAssociation (FleetOperatorAssociation (fleetOwnerId))
import Domain.Types.FleetOwnerInformation (FleetOwnerInformation (..))
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person as DP
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.External.Encryption (decrypt)
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Error (PersonError (PersonDoesNotExist, PersonNotFound))
import qualified Kernel.Types.Id as ID
import Kernel.Utils.Common (fromMaybeM, throwError)
import Storage.Queries.FleetOperatorAssociationExtra (findAllActiveByOperatorIdWithLimitOffset)
import Storage.Queries.FleetOwnerInformationExtra
  ( findByPersonIdAndEnabledAndVerified,
  )
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.VehicleRegistrationCertificateExtra as VRCQuery
import Tools.Error (GenericError (InvalidRequest))

getFleetManagementFleets ::
  ID.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
  Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
  Kernel.Prelude.Maybe Kernel.Prelude.Int ->
  Kernel.Prelude.Maybe Kernel.Prelude.Int ->
  Kernel.Prelude.Text ->
  Environment.Flow [Common.FleetInfo]
getFleetManagementFleets _merchantShortId _opCity mbIsActive mbVerified mbLimit mbOffset requestorId = do
  person <- QP.findById (ID.Id requestorId) >>= fromMaybeM (PersonNotFound requestorId)
  unless (person.role == DP.OPERATOR) $ throwError (InvalidRequest "Requestor role is not OPERATOR")
  activeFleetOwnerLs <- findAllActiveByOperatorIdWithLimitOffset requestorId mbLimit mbOffset
  fleetOwnerInfoLs <-
    mapMaybeM (findByPersonIdAndEnabledAndVerified mbIsActive mbVerified . ID.Id . fleetOwnerId) activeFleetOwnerLs
  mapM createFleetInfo fleetOwnerInfoLs
  where
    createFleetInfo FleetOwnerInformation {..} = do
      totalVehicle <- VRCQuery.countAllActiveRCForFleet fleetOwnerPersonId.getId merchantId
      person <-
        QP.findById fleetOwnerPersonId
          >>= fromMaybeM (PersonDoesNotExist fleetOwnerPersonId.getId)
      decryptedMobileNumber <-
        mapM decrypt person.mobileNumber
          >>= fromMaybeM (InvalidRequest $ "Person do not have a mobile number " <> person.id.getId)
      pure $
        Common.FleetInfo
          { id = ID.cast fleetOwnerPersonId,
            name = person.firstName,
            isActive = enabled,
            mobileCountryCode = fromMaybe "+91" person.mobileCountryCode,
            mobileNumber = decryptedMobileNumber,
            vehicleCount = totalVehicle,
            verified = verified
          }
