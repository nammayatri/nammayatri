module API.UI.LeaderBoard where

import Data.Time
import qualified Domain.Action.UI.LeaderBoard as DLeaderBoard
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.LeaderBoardConfig as LConfig
import qualified Domain.Types.Person as SP
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

type API =
  "driver" :> "leaderBoard"
    :> ( TokenAuth
           :> "daily"
           :> MandatoryQueryParam "date" Day
           :> Get '[JSON] DLeaderBoard.LeaderBoardRes
           :<|> TokenAuth
           :> "weekly"
           :> MandatoryQueryParam "fromDate" Day
           :> MandatoryQueryParam "toDate" Day
           :> Get '[JSON] DLeaderBoard.LeaderBoardRes
       )
    :<|> "driver"
    :> "referral"
    :> "leaderBoard"
    :> ( TokenAuth
           :> "daily"
           :> MandatoryQueryParam "date" Day
           :> Get '[JSON] DLeaderBoard.LeaderBoardRes
           :<|> TokenAuth
           :> "weekly"
           :> MandatoryQueryParam "fromDate" Day
           :> MandatoryQueryParam "toDate" Day
           :> Get '[JSON] DLeaderBoard.LeaderBoardRes
       )

handler :: FlowServer API
handler =
  (getDailyDriverLeaderBoard :<|> getWeeklyDriverLeaderBoard)
    :<|> (getDailyDriverReferralLeaderBoard :<|> getWeeklyDriverReferralLeaderBoard)

getDailyDriverLeaderBoard :: (Id SP.Person, Id DM.Merchant) -> Day -> FlowHandler DLeaderBoard.LeaderBoardRes
getDailyDriverLeaderBoard (personId, merchantId) date = withFlowHandlerAPI $ DLeaderBoard.getDailyDriverLeaderBoard LConfig.RIDE (personId, merchantId) date

getWeeklyDriverLeaderBoard :: (Id SP.Person, Id DM.Merchant) -> Day -> Day -> FlowHandler DLeaderBoard.LeaderBoardRes
getWeeklyDriverLeaderBoard (personId, merchantId) fromDate toDate = withFlowHandlerAPI $ DLeaderBoard.getWeeklyDriverLeaderBoard LConfig.RIDE (personId, merchantId) fromDate toDate

getDailyDriverReferralLeaderBoard :: (Id SP.Person, Id DM.Merchant) -> Day -> FlowHandler DLeaderBoard.LeaderBoardRes
getDailyDriverReferralLeaderBoard (personId, merchantId) date = withFlowHandlerAPI $ DLeaderBoard.getDailyDriverLeaderBoard LConfig.REFERRAL (personId, merchantId) date

getWeeklyDriverReferralLeaderBoard :: (Id SP.Person, Id DM.Merchant) -> Day -> Day -> FlowHandler DLeaderBoard.LeaderBoardRes
getWeeklyDriverReferralLeaderBoard (personId, merchantId) fromDate toDate = withFlowHandlerAPI $ DLeaderBoard.getWeeklyDriverLeaderBoard LConfig.REFERRAL (personId, merchantId) fromDate toDate
