module Storage.Queries.Transformers.EmailTemplate where

import qualified Data.Aeson as A
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common
import Kernel.Utils.JSON (valueToMaybe)
import Lib.Finance.Invoice.Email (InvoiceEmailConfig)

getInvoiceEmailConfig :: (MonadFlow m) => A.Value -> m InvoiceEmailConfig
getInvoiceEmailConfig config = valueToMaybe config & fromMaybeM (InternalError ("Unable to decode EmailTemplateT.config: " <> show config))
