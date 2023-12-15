{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Product.Sms where

import App.Types
import Control.Concurrent.MVar (modifyMVar, modifyMVar_)
import qualified Data.Map as Map
import EulerHS.Prelude
import Kernel.External.SMS.MyValueFirst.Types
import Kernel.Utils.Error.FlowHandling (withFlowHandler')
import Types.API.Sms

sendSms :: Text -> Text -> Text -> Text -> Text -> FlowHandler SubmitSmsRes
sendSms _username _password _from to text = withFlowHandler' $ do
  asks smsMap >>= liftIO . (`modifyMVar_` (pure . set))
  return Sent
  where
    set smss = Map.insert to (text : fromMaybe [] (Map.lookup to smss)) smss

readSms :: Text -> FlowHandler ReadSmsRes
readSms number = withFlowHandler' $ do
  asks smsMap >>= liftIO . (`modifyMVar` (pure . take'))
  where
    take' smss = Map.lookup number smss & maybe (smss, []) (Map.delete number smss,)
