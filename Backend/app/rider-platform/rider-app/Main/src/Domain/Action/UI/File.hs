module Domain.Action.UI.File
  ( postFilesUpload,
    getFilesMedia,
  )
where

import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified FileManagement.Common.UI.File as FCommon
import qualified FileManagement.Domain.Action.UI.File as DFile
import qualified Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Beam.IssueManagement ()
import qualified Storage.CachedQueries.Merchant as CQM
import Tools.Error

postFilesUpload ::
  ( ( Kernel.Prelude.Maybe (Id Domain.Types.Person.Person),
      Id Domain.Types.Merchant.Merchant
    ) ->
    FCommon.FileUploadReq ->
    Environment.Flow FCommon.FileUploadRes
  )
postFilesUpload (_mbPersonId, merchantId) req = do
  merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  DFile.fileUploadToS3 merchant.mediaFileSizeUpperLimit merchant.mediaFileUrlPattern req

getFilesMedia ::
  ( ( Kernel.Prelude.Maybe (Id Domain.Types.Person.Person),
      Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Text ->
    Environment.Flow Kernel.Prelude.Text
  )
getFilesMedia (_mbPersonId, _merchantId) =
  DFile.fetchFileMedia
