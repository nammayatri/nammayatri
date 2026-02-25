module Tools.FlowHandling
  ( withFlowHandlerAPIPersonId,
  )
where

import qualified Domain.Types.Person as Person
import Environment
import qualified EulerHS.Language as L
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.ConfigPilot.Interface.Getter (PersonIdKey (..))

withFlowHandlerAPIPersonId :: Id Person.Person -> Flow a -> FlowHandler a
withFlowHandlerAPIPersonId personId action = withFlowHandlerAPI $ do
  mbTxnId <- getTxnIdForPerson personId
  L.setOptionLocal PersonIdKey (getId personId)
  let modifyEnv env =
        env{txnId = maybe env.txnId Just mbTxnId
           }
  local modifyEnv action

getTxnIdForPerson :: Id Person.Person -> Flow (Maybe Text)
getTxnIdForPerson personId = Hedis.get (mkPersonTxnIdKey personId)

mkPersonTxnIdKey :: Id Person.Person -> Text
mkPersonTxnIdKey personId = "person:txnId:" <> getId personId
