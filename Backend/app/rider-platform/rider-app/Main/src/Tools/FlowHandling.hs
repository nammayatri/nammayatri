module Tools.FlowHandling
  ( withFlowHandlerAPIPersonId,
    setTxnIdForPerson,
  )
where

import qualified Domain.Types.Person as Person
import Environment
import qualified EulerHS.Language as L
import Kernel.Beam.Types (TxnIdKey (..))
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Getter (PersonIdKey (..))

withFlowHandlerAPIPersonId :: Id Person.Person -> Flow a -> FlowHandler a
withFlowHandlerAPIPersonId personId action = withFlowHandlerAPI $ do
  mbTxnId <- getTxnIdForPerson personId
  L.setOptionLocal PersonIdKey (getId personId)
  whenJust mbTxnId $ \txnId -> L.setOptionLocal TxnIdKey txnId
  action

getTxnIdForPerson :: Id Person.Person -> Flow (Maybe Text)
getTxnIdForPerson personId = Hedis.get (mkPersonTxnIdKey personId)

setTxnIdForPerson :: (CacheFlow m r, MonadFlow m) => Id Person.Person -> Text -> m ()
setTxnIdForPerson personId txnId =
  Hedis.setExp (mkPersonTxnIdKey personId) txnId personTxnIdTtlSeconds

personTxnIdTtlSeconds :: Int
personTxnIdTtlSeconds = 21600 -- 6 hours

mkPersonTxnIdKey :: Id Person.Person -> Text
mkPersonTxnIdKey personId = "person:txnId:" <> getId personId
