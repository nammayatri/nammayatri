{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Action.UI.Reels 
( API,
handler )
where
import Storage.Beam.SystemConfigs ()
import EulerHS.Prelude
import Servant
import Tools.Auth
import Kernel.Utils.Common
import qualified Domain.Action.UI.Reels
import qualified Domain.Types.Person
import qualified Kernel.Prelude
import qualified Control.Lens
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Types.Id
import qualified Kernel.External.Types
import qualified API.Types.UI.Reels
import qualified Domain.Types.MerchantOperatingCity



type API = (TokenAuth :> "reels" :> "getAllReelVideos" :> QueryParam "language" Kernel.External.Types.Language :> MandatoryQueryParam "reelsKey" Kernel.Prelude.Text :> Get ('[JSON])
                                                                                                                                                                            API.Types.UI.Reels.ReelsResp)
handler :: Environment.FlowServer API
handler = getReelsGetAllReelVideos
getReelsGetAllReelVideos :: ((Kernel.Types.Id.Id Domain.Types.Person.Person,
                              Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                              Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> Kernel.Prelude.Maybe (Kernel.External.Types.Language) -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.UI.Reels.ReelsResp)
getReelsGetAllReelVideos a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.Reels.getReelsGetAllReelVideos (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1



