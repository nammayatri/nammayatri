{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.VehicleRegistrationCertificate
  ( getAllLinkedRCs,
    RCLinkStatus (..),
    ResponseStatus (..),
  )
where

import qualified Data.HashMap.Strict as HM
import qualified Domain.Types.Person as Person
import Environment
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Id
import Storage.Queries.DriverOnboarding.DriverRCAssociation (buildRcHM)
import qualified Storage.Queries.DriverOnboarding.DriverRCAssociation as DAQuery
import qualified Storage.Queries.DriverOnboarding.VehicleRegistrationCertificate as RCQuery

data RCLinkStatus = RCLinkStatus
  { rcNumber :: Text,
    isActive :: Bool,
    status :: ResponseStatus
  }
  deriving (Show, Eq, Read, Generic, ToSchema, ToJSON, FromJSON)

data ResponseStatus = NO_DOC_AVAILABLE | PENDING | VALID | FAILED | INVALID | LIMIT_EXCEED | MANUAL_VERIFICATION_REQUIRED
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema, Enum, Bounded)

getAllLinkedRCs :: Id Person.Person -> Flow [RCLinkStatus]
getAllLinkedRCs driverId = do
  allLinkedRCs <- DAQuery.findAllLinkedByDriverId driverId
  rcs <- RCQuery.findAllById (map (.rcId) allLinkedRCs)
  let activeRcs = buildRcHM allLinkedRCs
  mapM (getCombinedRcData activeRcs) rcs
  where
    getCombinedRcData activeRcs rc = do
      rcNo <- decrypt rc.certificateNumber
      return $
        RCLinkStatus
          { rcNumber = rcNo,
            isActive = Just True == (HM.lookup rc.id.getId activeRcs <&> (.isRcActive)),
            status = VALID
          }
