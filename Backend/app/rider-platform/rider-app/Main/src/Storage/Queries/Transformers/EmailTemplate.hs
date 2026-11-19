module Storage.Queries.Transformers.EmailTemplate where

import qualified Data.Aeson as A
import Domain.Types.Extra.EmailTemplate (EmailTemplateConfig)
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common
import Kernel.Utils.JSON (valueToMaybe)

getEmailTemplateConfig :: (MonadFlow m) => A.Value -> m EmailTemplateConfig
getEmailTemplateConfig config = valueToMaybe config & fromMaybeM (InternalError ("Unable to decode EmailTemplateT.config: " <> show config))
