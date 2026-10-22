{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Clickhouse.ExternalApiCallLogs where

import Kernel.Prelude
import Kernel.Storage.ClickhouseV2 as CH
import qualified Kernel.Storage.ClickhouseV2.UtilsTH as TH

data ExternalApiCallLogsT f = ExternalApiCallLogsT
  { id :: C f Text,
    apiName :: C f (Maybe Text),
    svcProvider :: C f (Maybe Text),
    entityId :: C f (Maybe Text),
    responsePayload :: C f (Maybe Text),
    createdAt :: C f CH.DateTime -- DateTime on clickhouse side
  }
  deriving (Generic)

deriving instance Show ExternalApiCallLogs

externalApiCallLogsTTable :: ExternalApiCallLogsT (FieldModification ExternalApiCallLogsT)
externalApiCallLogsTTable =
  ExternalApiCallLogsT
    { id = "id",
      apiName = "apiName",
      svcProvider = "svcProvider",
      entityId = "entityId",
      responsePayload = "responsePayload",
      createdAt = "createdAt"
    }

type ExternalApiCallLogs = ExternalApiCallLogsT Identity

$(TH.mkClickhouseInstances ''ExternalApiCallLogsT 'NO_SELECT_MODIFIER)

findAllByEntityIdAndApiNames ::
  CH.HasClickhouseEnv CH.ATLAS_KAFKA m =>
  Text ->
  [Text] ->
  UTCTime ->
  UTCTime ->
  Int ->
  m [(Text, Maybe Text, Maybe Text, CH.DateTime, Maybe Text)]
findAllByEntityIdAndApiNames entityId apiNames from to limit =
  CH.findAll $
    CH.select_ (\logs -> CH.notGrouped (logs.id, logs.apiName, logs.svcProvider, logs.createdAt, logs.responsePayload)) $
      CH.orderBy_ (\logs _ -> CH.asc logs.createdAt) $
        CH.limit_ limit $
          CH.filter_
            ( \logs ->
                logs.createdAt >=. CH.DateTime from
                  CH.&&. logs.createdAt <=. CH.DateTime to
                  CH.&&. logs.entityId CH.==. Just entityId
                  CH.&&. logs.apiName `CH.in_` map Just apiNames
            )
            (CH.all_ @CH.ATLAS_KAFKA externalApiCallLogsTTable)
