{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.File
  ( API,
    handler,
  )
where

import qualified Control.Lens
import qualified Domain.Action.UI.File
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified FileManagement.Common.UI.File
import qualified Kernel.Prelude
import qualified Kernel.ServantMultipart
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  ( TokenAuth :> "files" :> "upload" :> Kernel.ServantMultipart.MultipartForm Kernel.ServantMultipart.Tmp FileManagement.Common.UI.File.FileUploadReq
      :> Post
           '[JSON]
           FileManagement.Common.UI.File.FileUploadRes
      :<|> TokenAuth
      :> "files"
      :> "media"
      :> MandatoryQueryParam
           "filePath"
           Kernel.Prelude.Text
      :> Get
           '[JSON]
           Kernel.Prelude.Text
  )

handler :: Environment.FlowServer API
handler = postFilesUpload :<|> getFilesMedia

postFilesUpload ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    FileManagement.Common.UI.File.FileUploadReq ->
    Environment.FlowHandler FileManagement.Common.UI.File.FileUploadRes
  )
postFilesUpload a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.File.postFilesUpload (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

getFilesMedia ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Text ->
    Environment.FlowHandler Kernel.Prelude.Text
  )
getFilesMedia a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.File.getFilesMedia (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
