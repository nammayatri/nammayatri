module Lib.Yudhishthira.Storage.Clickhouse.UserData where

import qualified Data.Aeson as A
import Kernel.Prelude
import Kernel.Storage.ClickhouseV2 as CH
import qualified Kernel.Storage.ClickhouseV2.UtilsTH as TH
import Kernel.Types.Id
import qualified Lib.Yudhishthira.Types
import qualified Lib.Yudhishthira.Types.Common
import qualified Lib.Yudhishthira.Types.UserData

data UserDataT f = UserDataT
  { batchNumber :: C f Int,
    chakra :: C f Lib.Yudhishthira.Types.Chakra,
    eventId :: C f (Id Lib.Yudhishthira.Types.Event),
    id :: C f (Kernel.Types.Id.Id Lib.Yudhishthira.Types.UserData.UserData),
    userDataValue :: C f A.Value,
    userId :: C f (Id Lib.Yudhishthira.Types.Common.User),
    createdAt :: C f UTCTime,
    updatedAt :: C f UTCTime
  }
  deriving (Generic)

deriving instance Show UserData

userDataTTable :: UserDataT (FieldModification UserDataT)
userDataTTable =
  UserDataT
    { batchNumber = "batch_number",
      chakra = "chakra",
      eventId = "event_id",
      id = "id",
      userDataValue = "user_data_value",
      userId = "user_id",
      createdAt = "created_at",
      updatedAt = "updated_at"
    }

type UserData = UserDataT Identity

$(TH.mkClickhouseInstances ''UserDataT 'NO_SELECT_MODIFIER)

findAllByUserIdAndEventId ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  Id Lib.Yudhishthira.Types.Common.User ->
  Id Lib.Yudhishthira.Types.Event ->
  m [Lib.Yudhishthira.Types.UserData.UserData]
findAllByUserIdAndEventId userId eventId =
  fmap (fromCHType <$>) $
    CH.findAll $
      CH.select $
        CH.filter_
          ( \userData _ ->
              userData.userId CH.==. userId
                CH.&&. userData.eventId CH.==. eventId
          )
          (CH.all_ @CH.APP_SERVICE_CLICKHOUSE userDataTTable)

findAllByEventIdWithLimitOffset ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  Id Lib.Yudhishthira.Types.Event ->
  Int ->
  Int ->
  m [Lib.Yudhishthira.Types.UserData.UserData]
findAllByEventIdWithLimitOffset eventId limit offset =
  fmap (fromCHType <$>) $
    CH.findAll $
      CH.select $
        CH.limit_ limit $
          CH.offset_ offset $
            CH.filter_
              (\userData _ -> userData.eventId CH.==. eventId)
              (CH.all_ @CH.APP_SERVICE_CLICKHOUSE userDataTTable)

fromCHType :: UserData -> Lib.Yudhishthira.Types.UserData.UserData
fromCHType UserDataT {..} =
  Lib.Yudhishthira.Types.UserData.UserData {..}
