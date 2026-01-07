module Domain.Action.Dashboard.Media (getMediaFile) where

import qualified API.Types.RiderPlatform.Management.Media
import qualified Data.Text
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import qualified Storage.Flow as Storage

getMediaFile :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Data.Text.Text -> Environment.Flow API.Types.RiderPlatform.Management.Media.GetMediaFileResponse)
getMediaFile _merchantShortId _opCity filePath = do
  content <- Storage.get $ Data.Text.unpack filePath
  pure $ API.Types.RiderPlatform.Management.Media.GetMediaFileResponse {content = content}
