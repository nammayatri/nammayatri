module Redis where

import Beckn.Mock.App
import Beckn.Mock.Environment
import Beckn.Mock.Exceptions
import Beckn.Mock.Redis
import Beckn.Mock.Utils
import Beckn.Types.Core.Migration.Context
import Beckn.Utils.Logging
import Core.OnConfirm
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.String.Conversions
import GHC.Records.Extra
import Relude hiding (id, ord)

data OnConfirmContextOrder = OnConfirmContextOrder
  { context :: Context,
    order :: Order
  }

contextKey, orderKey :: BS.ByteString
contextKey = "context"
orderKey = "order"

contextOrderToRedisHash :: OnConfirmContextOrder -> RedisHash
contextOrderToRedisHash val =
  let contextValue = BSL.toStrict $ encodeJSON val.context
      orderValue = BSL.toStrict $ encodeJSON val.order
   in [ (contextKey, contextValue),
        (orderKey, orderValue)
      ]

contextOrderFromRedisHash :: RedisHash -> Either Text OnConfirmContextOrder
contextOrderFromRedisHash hash = do
  -- Either Text
  context <- findAndDecode contextKey hash
  order <- findAndDecode orderKey hash
  pure $ OnConfirmContextOrder {..}

instance RedisHashable OnConfirmContextOrder where
  toRedisHash = contextOrderToRedisHash
  fromRedisHash = contextOrderFromRedisHash

writeOrder :: Context -> Order -> MockM AppEnv ()
writeOrder ctx order = do
  let val = OnConfirmContextOrder ctx order
      id = order.id
  write id val
  mockLog INFO $ "inserted context and order into redis; key = " <> id

readOrder :: Text -> MockM AppEnv (Context, Order)
readOrder orderId = do
  eithRes <- readHashableEither orderId
  toTuple <$> fromEitherM redisErrorToMockException eithRes
  where
    toTuple OnConfirmContextOrder {..} = (context, order)

editOrder :: (Order -> Order) -> Text -> MockM AppEnv ()
editOrder func id = do
  eithRes <- editHashable editSecond id
  fromEitherM redisErrorToMockException eithRes
  where
    editSecond :: OnConfirmContextOrder -> OnConfirmContextOrder
    editSecond ctxord = ctxord {order = func ctxord.order}

redisErrorToMockException :: RedisError -> MockException
redisErrorToMockException (NotFound msg) = OrderNotFound msg
redisErrorToMockException (DecodeError msg) = OtherError msg
