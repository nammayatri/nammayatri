module Storage.Clickhouse.PaymentOrder where

import Kernel.Prelude
import Kernel.Storage.ClickhouseV2 as CH
import qualified Kernel.Storage.ClickhouseV2.UtilsTH as TH

data PaymentOrderT f = PaymentOrderT
  { orderId :: C f Text,
    shortId :: C f Text,
    createdAt :: C f UTCTime
  }
  deriving (Generic)

deriving instance Show PaymentOrder

paymentOrderTTable :: PaymentOrderT (FieldModification PaymentOrderT)
paymentOrderTTable =
  PaymentOrderT
    { orderId = "id",
      shortId = "short_id",
      createdAt = "created_at"
    }

type PaymentOrder = PaymentOrderT Identity

$(TH.mkClickhouseInstances ''PaymentOrderT 'NO_SELECT_MODIFIER)

getShortIdsByOrderIds ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  [Text] ->
  m [Text]
getShortIdsByOrderIds orderIds =
  CH.findAll $
    CH.select_ (\po -> CH.notGrouped po.shortId) $
      CH.selectModifierOverride CH.NO_SELECT_MODIFIER $
        CH.filter_
          (\po -> po.orderId `CH.in_` orderIds)
          (CH.all_ @CH.APP_SERVICE_CLICKHOUSE paymentOrderTTable)
