module Domain.Action.Dashboard.Management.Media (getMediaMediaImage) where

import qualified API.Types.ProviderPlatform.Management.Media
import qualified Data.Text as T
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified "shared-services" IssueManagement.Domain.Types.MediaFile as DMF
import qualified "shared-services" IssueManagement.Storage.Queries.MediaFile as QMF
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Error
import Storage.Beam.IssueManagement ()
import qualified Storage.Flow as Storage
import Storage.Types (FileType (..))
import Tools.Error

getMediaMediaImage :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id DMF.MediaFile -> Environment.Flow API.Types.ProviderPlatform.Management.Media.GetImageResponse)
getMediaMediaImage _merchantShortId _opCity imageId = do
  mediaFile <- QMF.findById imageId >>= fromMaybeM (FileDoNotExist imageId.getId)
  unless (mediaFile._type == Image) $ throwError $ InvalidRequest ("Only Image Media query allowed" <> imageId.getId)
  mediaFilePath <- mediaFile.s3FilePath & fromMaybeM (FileDoNotExist imageId.getId)
  image <- Storage.get $ T.unpack mediaFilePath
  pure $ API.Types.ProviderPlatform.Management.Media.GetImageResponse image
