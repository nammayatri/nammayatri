module Storage.Clickhouse.Location where

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

findFullAddressById ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  Id DLocation.Location ->
  UTCTime ->
  m (Maybe Text)
findFullAddressById id createdAt = do
  location <-
    CH.findAll $
      CH.select_ (\loc -> CH.notGrouped (loc.fullAddress)) $
        CH.filter_
          ( \location ->
              location.id CH.==. id
                CH.&&. location.createdAt >=. addUTCTime (-120) createdAt -- locations are created before booking, so 2 mins buffer is added here
          )
          (CH.all_ @CH.APP_SERVICE_CLICKHOUSE locationTTable)
  return . join . listToMaybe $ location
