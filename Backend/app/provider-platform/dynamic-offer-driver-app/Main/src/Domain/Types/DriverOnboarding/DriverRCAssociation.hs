{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.DriverOnboarding.DriverRCAssociation where

import Domain.Types.DriverOnboarding.VehicleRegistrationCertificate
import Domain.Types.Person
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id

data DriverRCAssociation = DriverRCAssociation
  { id :: Id DriverRCAssociation,
    driverId :: Id Person,
    rcId :: Id VehicleRegistrationCertificate,
    associatedOn :: UTCTime,
    associatedTill :: Maybe UTCTime,
    consent :: Bool,
    consentTimestamp :: UTCTime,
    isRcActive :: Bool
  }
  deriving (Generic, ToSchema, ToJSON, FromJSON)

makeRCAssociation :: (MonadFlow m) => Id Person -> Id VehicleRegistrationCertificate -> Maybe UTCTime -> m DriverRCAssociation
makeRCAssociation driverId rcId end = do
  id <- generateGUID
  now <- getCurrentTime
  return $
    DriverRCAssociation
      { id,
        driverId,
        rcId,
        associatedOn = now,
        associatedTill = end,
        consent = True,
        consentTimestamp = now,
        isRcActive = False
      }
