module API.Test where

import Environment
import Kernel.Prelude
import Kernel.Randomizer
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Utils.Common
import Servant

type API =
  "test"
    :> ( "setNx"
           :> QueryParam "iter" Integer
           :> Get '[JSON] Text
           :<|> "get"
             :> QueryParam "iter" Integer
             :> Get '[JSON] Text
           :<|> "del"
             :> QueryParam "iter" Integer
             :> Get '[JSON] Text
           :<|> "setNxEx"
             :> QueryParam "iter" Integer
             :> Get '[JSON] Text
       )

handler :: FlowServer API
handler = setNx :<|> get :<|> del :<|> setNxEx

keyPrefix :: Text
keyPrefix = "redis:test:"

setNx :: Maybe Integer -> FlowHandler Text
setNx iter = withFlowHandlerAPI $ do
  void $
    replicateM (fromMaybe 1 $ fromInteger <$> iter) $ do
      num <- getRandomInRange (10000000, 99999999 :: Int)
      let key = "test" <> show num
      let value :: Text = "test"
      res <- Hedis.setNx (keyPrefix <> key) value
      logDebug $ "setNx " <> "key: " <> key <> " value: " <> value <> " res: " <> show res
  pure "test complete"

get :: Maybe Integer -> FlowHandler Text
get iter = withFlowHandlerAPI $ do
  void $
    replicateM (fromMaybe 1 $ fromInteger <$> iter) $ do
      num <- getRandomInRange (10000000, 99999999 :: Int)
      let key = "test" <> show num
      let value :: Text = "test"
      res :: Maybe Text <- (Hedis.get (keyPrefix <> key))
      -- res <- fromMaybe False <$> Hedis.get (keyPrefix <> key)
      logDebug $ "get " <> "key: " <> key <> " value: " <> value <> " res: " <> show res
  pure "test complete"

-- let key = "test"
-- res <- redis $ \conn -> liftIO $ runRedis conn $ get key
-- pure $ show res

del :: Maybe Integer -> FlowHandler Text
del iter = withFlowHandlerAPI $ do
  void $
    replicateM (fromMaybe 1 $ fromInteger <$> iter) $ do
      num <- getRandomInRange (10000000, 99999999 :: Int)
      let key = "test" <> show num
      let value :: Text = "test"
      res <- Hedis.del (keyPrefix <> key)
      logDebug $ "del " <> "key: " <> key <> " value: " <> value <> " res: " <> show res

  -- let key = "test"
  -- res <- redis $ \conn -> liftIO $ runRedis conn $ del [key]
  -- pure $ show res
  pure "test complete"

setNxEx :: Maybe Integer -> FlowHandler Text
setNxEx iter = withFlowHandlerAPI $ do
  void $
    replicateM (fromMaybe 1 $ fromInteger <$> iter) $ do
      num <- getRandomInRange (10000000, 99999999 :: Int)
      let key = "test" <> show num
      let value :: Text = "test"
      res <- Hedis.setNxExpire (keyPrefix <> key) 10 value
      logDebug $ "setNxEx " <> "key: " <> key <> " value: " <> value <> " res: " <> show res
  -- let key = "test"
  -- let value = "test"
  -- res <- redis $ \conn -> liftIO $ runRedis conn $ setnxex key value 10
  -- pure $ show res
  pure "test complete"
