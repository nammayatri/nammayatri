module Domain.Action.UI.File
  ( postFilesUpload,
    getFilesMedia,
  )
where

import qualified Domain.Action.UI.RideEvidence as RideEvidence
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified FileManagement.Common.UI.File as FCommon
import qualified FileManagement.Domain.Action.UI.File as DFile
import qualified Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import Storage.Beam.IssueManagement ()
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import Tools.Error

postFilesUpload ::
  ( ( Kernel.Prelude.Maybe (Id Domain.Types.Person.Person),
      Id Domain.Types.Merchant.Merchant,
      Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    FCommon.FileUploadReq ->
    Environment.Flow FCommon.FileUploadRes
  )
postFilesUpload (_mbPersonId, _merchantId, merchantOpCityId) req = do
  personId <- fromMaybeM (PersonNotFound "person id not present in token") _mbPersonId
  transporterConfig <-
    getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) Nothing
      >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  when (req.domain == FCommon.RideEvidence) $ do
    unless (RideEvidence.isRideEvidenceEnabled transporterConfig) $ throwError RideEvidenceDisabled
    RideEvidence.checkEvidenceUploadRate personId transporterConfig
  uploaded <- DFile.fileUploadToS3 transporterConfig.mediaFileSizeUpperLimit transporterConfig.mediaFileUrlPattern personId.getId req
  when (req.domain == FCommon.RideEvidence) $
    RideEvidence.storeThumbnail transporterConfig.mediaFileSizeUpperLimit transporterConfig.mediaFileUrlPattern personId.getId req uploaded
  pure uploaded

getFilesMedia ::
  ( ( Kernel.Prelude.Maybe (Id Domain.Types.Person.Person),
      Id Domain.Types.Merchant.Merchant,
      Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Text ->
    Environment.Flow Kernel.Prelude.Text
  )
getFilesMedia (_mbPersonId, _merchantId, _merchantOpCityId) filePath = do
  personId <- fromMaybeM (PersonNotFound "person id not present in token") _mbPersonId
  unless (FCommon.canReadFile personId.getId filePath) $
    throwError (InvalidRequest "File is not accessible")
  if FCommon.isInDomainFolder FCommon.RideEvidence filePath
    then DFile.fetchFileMediaOrNotFound filePath
    else DFile.fetchFileMedia filePath
