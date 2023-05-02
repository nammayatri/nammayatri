module API.UI.LeaderBoard where

import qualified Domain.Action.UI.LeaderBoard as DLeaderBoard
import qualified Domain.Types.Person as SP
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

type API =
  "driver"
    :> "leaderBoard"
    :> TokenAuth
    :> QueryParam "limit" Integer
    :> Get '[JSON] DLeaderBoard.LeaderBoardRes

handler :: FlowServer API
handler =
  getDriverLeaderBoard

getDriverLeaderBoard :: Id SP.Person -> Maybe Integer -> FlowHandler DLeaderBoard.LeaderBoardRes
getDriverLeaderBoard personId mbLimit = withFlowHandlerAPI $ DLeaderBoard.getDriverLeaderBoard personId mbLimit
