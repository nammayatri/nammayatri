{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

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
  getKey = Hed.get
  setKey = Hed.set
  delKey = Hed.del

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
