module Storage.Queries.Message.MessageTranslation where

import Kernel.Storage.Esqueleto
import qualified Kernel.Storage.Esqueleto as Esq
import Domain.Types.Message.MessageTranslation
import qualified Domain.Types.Message.Message as Msg
import Storage.Tabular.Message.MessageTranslation
import Kernel.Types.Id
import Kernel.Prelude
import Kernel.External.Types (Language)

create :: MessageTranslation -> SqlDB ()
create = Esq.create

findByMessageIdAndLanguage :: Transactionable m => Id Msg.Message -> Language -> m (Maybe MessageTranslation)
findByMessageIdAndLanguage messageId language =
  Esq.findOne $ do
    merchantServiceConfig <- from $ table @MessageTranslationT
    where_ $
      merchantServiceConfig ^. MessageTranslationTId ==. val (toKey (messageId, language))
    return merchantServiceConfig
