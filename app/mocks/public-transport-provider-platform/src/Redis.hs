module Redis where

import "public-transport-rider-platform" Beckn.Spec.OnConfirm.Order
import Data.Aeson
import Environment
import GHC.Records.Extra
import Kernel.Mock.App
import Kernel.Mock.Exceptions (OrderError (OrderNotFound))
import qualified Kernel.Storage.Hedis as Hed
import Kernel.Types.Beckn.Context
import Kernel.Types.Cache
import Kernel.Utils.Error.Throwing
import Kernel.Utils.Logging
import Relude hiding (id, ord)

data OnConfirmContextOrder = OnConfirmContextOrder
  { context :: Context,
    order :: Order
  }
  deriving (Show, Generic, FromJSON, ToJSON)

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
  logOutput INFO $ "inserted context and order into cache; key = " <> id

readOrder :: Text -> MockM AppEnv (Context, Order)
readOrder orderId = do
  mRes <- fmap toTuple <$> getKey orderId
  fromMaybeM (OrderNotFound orderId) mRes

editOrder :: (Order -> Order) -> Text -> MockM AppEnv ()
editOrder func id = do
  (context, order) <- readOrder id
  writeOrder context $ func order
