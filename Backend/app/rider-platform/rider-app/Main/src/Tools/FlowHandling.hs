module Tools.FlowHandling
  ( withFlowHandlerAPIPersonId,
  )
where

import qualified Domain.Types.Person as Person
import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common

withFlowHandlerAPIPersonId :: Id Person.Person -> Flow a -> FlowHandler a
withFlowHandlerAPIPersonId personId action = withFlowHandlerAPI $ do
  mbTxnId <- getTxnIdForPerson personId
  case mbTxnId of
    Just txnId' -> local (\env -> env {txnId = Just txnId'}) action
    Nothing -> action

getTxnIdForPerson :: Id Person.Person -> Flow (Maybe Text)
getTxnIdForPerson personId = Hedis.get (mkPersonTxnIdKey personId)

mkPersonTxnIdKey :: Id Person.Person -> Text
mkPersonTxnIdKey personId = "person:txnId:" <> getId personId
