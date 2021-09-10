module Product.Sms where

import App.Types
import Beckn.External.MyValueFirst.Types
import Beckn.Utils.Error.FlowHandling (withFlowHandler)
import Control.Concurrent.MVar (modifyMVar, modifyMVar_)
import qualified Data.Map as Map
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Types.API.Sms

sendSms :: Text -> Text -> Text -> Text -> Text -> FlowHandler SubmitSmsRes
sendSms _username _password _from to text = withFlowHandler $ do
  asks smsMap >>= L.runIO . (`modifyMVar_` (pure . set))
  return Sent
  where
    set smss = Map.insert to (text : fromMaybe [] (Map.lookup to smss)) smss

readSms :: Text -> FlowHandler ReadSmsRes
readSms number = withFlowHandler $ do
  asks smsMap >>= L.runIO . (`modifyMVar` (pure . take'))
  where
    take' smss = Map.lookup number smss & maybe (smss, []) (Map.delete number smss,)
