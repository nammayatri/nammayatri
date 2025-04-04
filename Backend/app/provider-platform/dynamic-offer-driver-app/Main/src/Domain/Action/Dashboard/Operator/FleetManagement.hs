{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Dashboard.Operator.FleetManagement (getFleetManagementFleets) where

import qualified API.Types.ProviderPlatform.Operator.FleetManagement as Common
import Data.OpenApi (ToSchema)
import Domain.Types.FleetOwnerInformation (FleetOwnerInformation (..))
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.External.Encryption (decrypt)
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Error (PersonError (PersonDoesNotExist))
import qualified Kernel.Types.Id as ID
import Kernel.Utils.Common (fromMaybeM)
import Servant
import Storage.Queries.FleetOwnerInformationExtra
  ( findAllByReferredByOperatorIdAndEnabledAndVerifiedWithLimitOffset,
  )
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.VehicleRegistrationCertificateExtra as VRCQuery
import Tools.Auth
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
  fleetOwnerInfoLs <-
    findAllByReferredByOperatorIdAndEnabledAndVerifiedWithLimitOffset mbIsActive mbVerified mbLimit mbOffset $
      Just requestorId
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

--   FleetInfo:
--     - id: Id Person +
--     - name: Text
--     - isActive: Bool +
--     - mobileCountryCode: Text
--     - mobileNumber: Text
--     - vehicleCount: Int
--     - verified: Bool    +

-- FleetOwnerInformation:
--   tableName: fleet_owner_information
--   fields:
--     merchantId: Id Merchant
--     fleetOwnerPersonId : Id Person
--     enabled: Bool
--     blocked: Bool
--     verified: Bool
--     gstNumber: Maybe Text
--     fleetType: FleetType
--     gstImageId : Maybe Text
--     referredByOperatorId: Maybe Text
