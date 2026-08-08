{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.FRFSDriverRatingInternal
  ( API,
    handler,
  )
where

import qualified API.Types.UI.FRFSDriverRatingInternal
import qualified Data.Text
import qualified Domain.Action.UI.FRFSDriverRatingInternal
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  ( "internal" :> "frfs" :> "driver" :> "rating" :> Header "token" Data.Text.Text :> ReqBody '[JSON] API.Types.UI.FRFSDriverRatingInternal.FRFSDriverRatingReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
      :<|> "internal"
      :> "frfs"
      :> "driver"
      :> "rating"
      :> QueryParam
           "merchantId"
           Data.Text.Text
      :> QueryParam
           "driverBadgeToken"
           Data.Text.Text
      :> QueryParam
           "fleetNumber"
           Data.Text.Text
      :> QueryParam
           "gtfsId"
           Data.Text.Text
      :> Header
           "token"
           Data.Text.Text
      :> Get
           '[JSON]
           API.Types.UI.FRFSDriverRatingInternal.FRFSDriverRatingAggRes
  )

handler :: Environment.FlowServer API
handler = postInternalFrfsDriverRating :<|> getInternalFrfsDriverRating

postInternalFrfsDriverRating :: (Kernel.Prelude.Maybe Data.Text.Text -> API.Types.UI.FRFSDriverRatingInternal.FRFSDriverRatingReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postInternalFrfsDriverRating a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.FRFSDriverRatingInternal.postInternalFrfsDriverRating a2 a1

getInternalFrfsDriverRating :: (Kernel.Prelude.Maybe Data.Text.Text -> Kernel.Prelude.Maybe Data.Text.Text -> Kernel.Prelude.Maybe Data.Text.Text -> Kernel.Prelude.Maybe Data.Text.Text -> Kernel.Prelude.Maybe Data.Text.Text -> Environment.FlowHandler API.Types.UI.FRFSDriverRatingInternal.FRFSDriverRatingAggRes)
getInternalFrfsDriverRating a5 a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.FRFSDriverRatingInternal.getInternalFrfsDriverRating a5 a4 a3 a2 a1
