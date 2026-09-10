{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.FRFSBookingRatingInternal
  ( API,
    handler,
  )
where

import qualified API.Types.UI.FRFSBookingRatingInternal
import qualified Data.Text
import qualified Domain.Action.UI.FRFSBookingRatingInternal
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  ( "internal" :> "frfs" :> "booking" :> "rating" :> Header "token" Data.Text.Text :> ReqBody ('[JSON]) API.Types.UI.FRFSBookingRatingInternal.FRFSBookingRatingReq
      :> Post
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
      :<|> "internal"
      :> "frfs"
      :> "booking"
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
           ('[JSON])
           API.Types.UI.FRFSBookingRatingInternal.FRFSBookingRatingAggRes
  )

handler :: Environment.FlowServer API
handler = postInternalFrfsBookingRating :<|> getInternalFrfsBookingRating

postInternalFrfsBookingRating :: (Kernel.Prelude.Maybe (Data.Text.Text) -> API.Types.UI.FRFSBookingRatingInternal.FRFSBookingRatingReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postInternalFrfsBookingRating a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.FRFSBookingRatingInternal.postInternalFrfsBookingRating a2 a1

getInternalFrfsBookingRating :: (Kernel.Prelude.Maybe (Data.Text.Text) -> Kernel.Prelude.Maybe (Data.Text.Text) -> Kernel.Prelude.Maybe (Data.Text.Text) -> Kernel.Prelude.Maybe (Data.Text.Text) -> Kernel.Prelude.Maybe (Data.Text.Text) -> Environment.FlowHandler API.Types.UI.FRFSBookingRatingInternal.FRFSBookingRatingAggRes)
getInternalFrfsBookingRating a5 a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.FRFSBookingRatingInternal.getInternalFrfsBookingRating a5 a4 a3 a2 a1
