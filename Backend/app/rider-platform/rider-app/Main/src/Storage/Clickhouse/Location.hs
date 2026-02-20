{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Clickhouse.Location where

import Control.Lens ((^?), _head)
import qualified Domain.Types.Location as DLocation
import Kernel.Prelude
import Kernel.Storage.ClickhouseV2 as CH
import qualified Kernel.Storage.ClickhouseV2.UtilsTH as TH
import Kernel.Types.Id
import Kernel.Utils.Common

data LocationT f = LocationT
  { id :: C f (Id DLocation.Location),
    fullAddress :: C f (Maybe Text),
    createdAt :: C f UTCTime
  }
  deriving (Generic)

deriving instance Show Location

-- TODO move to TH (quietSnake)
locationTTable :: LocationT (FieldModification LocationT)
locationTTable =
  LocationT
    { id = "id",
      fullAddress = "full_address",
      createdAt = "created_at"
    }

type Location = LocationT Identity

$(TH.mkClickhouseInstances ''LocationT 'SELECT_FINAL_MODIFIER)

findLocationById ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  Id DLocation.Location ->
  UTCTime ->
  m (Maybe Location)
findLocationById id createdAt = do
  location <-
    CH.findAll $
      CH.select $
        CH.filter_
          ( \location ->
              location.id CH.==. id
                CH.&&. location.createdAt >=. addUTCTime (-120) createdAt -- locations are created before booking, so 2 mins buffer is added here
          )
          (CH.all_ @CH.APP_SERVICE_CLICKHOUSE locationTTable)
  return $ location ^? _head
