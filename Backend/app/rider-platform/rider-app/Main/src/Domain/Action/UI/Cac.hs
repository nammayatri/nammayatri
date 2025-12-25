module Domain.Action.UI.Cac where

import qualified Data.Aeson
import qualified Data.Aeson.KeyMap
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions (runInReplica)
import qualified Kernel.Beam.Types as KBT
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Error.Throwing
import Kernel.Utils.Logging
import SharedLogic.Cac
import qualified Storage.Queries.Person as QPerson
import Tools.Error

postGetUiConfigs ::
  (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) ->
  Kernel.Prelude.Maybe Kernel.Prelude.Int ->
  Kernel.Prelude.Maybe Kernel.Prelude.Text ->
  Data.Aeson.Object ->
  Environment.Flow Data.Aeson.Object
postGetUiConfigs (mbPersonId, _) toss tenant context = do
  systemConfigs <- L.getOption KBT.Tables
  let useCACConfig = maybe False (.useCACForFrontend) systemConfigs
  if useCACConfig
    then case mbPersonId of
      Just personId -> do
        person <- runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
        fromMaybe Data.Aeson.KeyMap.empty <$> getFrontendConfigs person toss tenant context
      Nothing -> do
        logError "PersonId is null, hence context of city cannot be determined. Returning empty object."
        return Data.Aeson.KeyMap.empty
    else return Data.Aeson.KeyMap.empty
