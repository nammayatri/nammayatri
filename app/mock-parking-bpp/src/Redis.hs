module Redis where

import Beckn.Mock.App
import Beckn.Mock.Exceptions (OrderError (OrderNotFound))
import Beckn.Prelude
import qualified Beckn.Storage.Hedis as Hed
import Beckn.Types.Cache
import Beckn.Types.Core.Migration.Context
import Beckn.Utils.Error.Throwing
import Beckn.Utils.Logging
import Core.OnConfirm.Order
import Environment
import GHC.Records.Extra

data OnConfirmContextOrder = OnConfirmContextOrder
  { context :: Context,
    order :: Order
  }
  deriving (Generic, FromJSON, ToJSON)

toTuple :: OnConfirmContextOrder -> (Context, Order)
toTuple occo = (occo.context, occo.order)

instance Cache OnConfirmContextOrder (MockM AppEnv) where
  type CacheKey OnConfirmContextOrder = Text
  getKey key = Hed.get key
  setKey key val = Hed.set key val
  delKey key = Hed.del key

writeOrder :: Context -> Order -> MockM AppEnv ()
writeOrder ctx order = do
  let val = OnConfirmContextOrder ctx order
      id = order.id
  setKey id val
  logInfo $ "inserted context and order into cache; key = " <> id

readOrder :: Text -> MockM AppEnv (Context, Order)
readOrder orderId = do
  mRes <- fmap toTuple <$> getKey orderId
  fromMaybeM (OrderNotFound orderId) mRes

editOrder :: (Order -> Order) -> Text -> MockM AppEnv ()
editOrder func id = do
  (context, order) <- readOrder id
  writeOrder context $ func order
