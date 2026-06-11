{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Clickhouse.FareBreakupInfo where

import qualified Domain.Types.FareBreakup as DFareBreakup
import qualified Domain.Types.FareBreakupInfo as DFareBreakupInfo
import Kernel.Prelude
import Kernel.Storage.ClickhouseV2 as CH
import qualified Kernel.Storage.ClickhouseV2.UtilsTH as TH
import Kernel.Utils.Common
import qualified Kernel.Utils.Text

data FareBreakupInfoT f = FareBreakupInfoT
  { entityId :: C f Text,
    entityType :: C f Text,
    fareBreakups :: C f Text,
    createdAt :: C f UTCTime
  }
  deriving (Generic)

deriving instance Show FareBreakupInfo

fareBreakupInfoTTable :: FareBreakupInfoT (FieldModification FareBreakupInfoT)
fareBreakupInfoTTable =
  FareBreakupInfoT
    { entityId = "entity_id",
      entityType = "entity_type",
      fareBreakups = "fare_breakups",
      createdAt = "created_at"
    }

type FareBreakupInfo = FareBreakupInfoT Identity

$(TH.mkClickhouseInstances ''FareBreakupInfoT 'SELECT_FINAL_MODIFIER)

findFareBreakupItemsByEntityIdAndType ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  Text ->
  DFareBreakup.FareBreakupEntityType ->
  UTCTime ->
  m (Maybe [DFareBreakupInfo.FareBreakupInfoItem])
findFareBreakupItemsByEntityIdAndType entityId entityType createdAt = do
  rows <-
    CH.findAll $
      CH.select $
        CH.filter_
          ( \fareBreakupInfo ->
              fareBreakupInfo.entityId CH.==. entityId
                CH.&&. fareBreakupInfo.entityType CH.==. show entityType
                CH.&&. fareBreakupInfo.createdAt >=. addUTCTime (-120) createdAt
          )
          (CH.all_ @CH.APP_SERVICE_CLICKHOUSE fareBreakupInfoTTable)
  pure $ (\fareBreakupInfo -> Kernel.Utils.Text.decodeFromText fareBreakupInfo.fareBreakups) =<< listToMaybe rows
