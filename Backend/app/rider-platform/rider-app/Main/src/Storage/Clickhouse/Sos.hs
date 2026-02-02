{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Clickhouse.Sos where

import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DR
import Kernel.Prelude
import Kernel.Storage.ClickhouseV2 as CH
import qualified Kernel.Storage.ClickhouseV2.UtilsTH as TH
import Kernel.Types.Id
import qualified Safety.Domain.Types.Sos as SafetyDSos

data SosT f = SosT
  { flow :: C f SafetyDSos.SosType,
    id :: C f (Id SafetyDSos.Sos),
    personId :: C f (Id DP.Person),
    rideId :: C f (Id DR.Ride),
    status :: C f SafetyDSos.SosStatus,
    ticketId :: C f (Maybe Text),
    merchantId :: C f (Maybe (Id DM.Merchant)),
    merchantOperatingCityId :: C f (Maybe (Id DMOC.MerchantOperatingCity))
  }
  deriving (Generic)

deriving instance Show Sos

instance ClickhouseValue SafetyDSos.SosType

instance ClickhouseValue SafetyDSos.SosStatus

sosTTable :: SosT (FieldModification SosT)
sosTTable =
  SosT
    { flow = "flow",
      id = "id",
      personId = "person_id",
      rideId = "ride_id",
      status = "status",
      ticketId = "ticket_id",
      merchantId = "merchant_id",
      merchantOperatingCityId = "merchant_operating_city_id"
    }

type Sos = SosT Identity

$(TH.mkClickhouseInstances ''SosT 'SELECT_FINAL_MODIFIER)

findAllByPersonId ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  Id DP.Person ->
  m [Sos]
findAllByPersonId personId = do
  CH.findAll $
    CH.select $
      CH.filter_
        ( \sos ->
            sos.personId CH.==. personId
        )
        (CH.all_ @CH.APP_SERVICE_CLICKHOUSE sosTTable)
