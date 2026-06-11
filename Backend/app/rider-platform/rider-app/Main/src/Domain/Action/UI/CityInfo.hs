module Domain.Action.UI.CityInfo (getOperatingCity) where

import qualified API.Types.UI.CityInfo
import qualified Data.Text as T
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.MerchantOperatingCityExtra as QMOC

getOperatingCity ::
  ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
  ) ->
  Kernel.Prelude.Text ->
  Environment.Flow API.Types.UI.CityInfo.StdCodeResp
getOperatingCity _authInfo stdCode = do
  unless (T.isPrefixOf "std:" stdCode) $
    throwError (InvalidRequest "Invalid STD code format. Expected format: std:<code> (e.g. std:080)")
  moc <- QMOC.findByStdCode stdCode >>= fromMaybeM (InvalidRequest "City not found for provided STD code")
  pure $ API.Types.UI.CityInfo.StdCodeResp {stdCode = stdCode, cityName = show moc.city}
