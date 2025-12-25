{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.AttractionRecommend
  ( API,
    handler,
  )
where

import qualified API.Types.UI.AttractionRecommend
import qualified Control.Lens
import qualified Domain.Action.UI.AttractionRecommend
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  ( TokenAuth :> "attractions" :> "recommend" :> ReqBody '[JSON] API.Types.UI.AttractionRecommend.AttractionRecommendReq
      :> Post
           '[JSON]
           API.Types.UI.AttractionRecommend.AttractionRecommendResp
  )

handler :: Environment.FlowServer API
handler = postAttractionsRecommend

postAttractionsRecommend ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    API.Types.UI.AttractionRecommend.AttractionRecommendReq ->
    Environment.FlowHandler API.Types.UI.AttractionRecommend.AttractionRecommendResp
  )
postAttractionsRecommend a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.AttractionRecommend.postAttractionsRecommend (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
