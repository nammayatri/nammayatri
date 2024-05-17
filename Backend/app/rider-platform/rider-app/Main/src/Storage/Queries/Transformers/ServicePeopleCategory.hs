module Storage.Queries.Transformers.ServicePeopleCategory where

import qualified Data.Aeson
import qualified Domain.Types.ServicePeopleCategory
import Kernel.Prelude

convertCancellationChargesToTable :: (Kernel.Prelude.Maybe [Domain.Types.ServicePeopleCategory.CancellationCharge] -> Kernel.Prelude.Maybe Data.Aeson.Value)
convertCancellationChargesToTable mbCancelCharge = Data.Aeson.toJSON <$> mbCancelCharge

getCancellationChargesFromTable :: (Kernel.Prelude.Maybe Data.Aeson.Value -> Kernel.Prelude.Maybe [Domain.Types.ServicePeopleCategory.CancellationCharge])
getCancellationChargesFromTable = maybe Nothing valueToMaybe

valueToMaybe :: Data.Aeson.Value -> Kernel.Prelude.Maybe [Domain.Types.ServicePeopleCategory.CancellationCharge]
valueToMaybe val =
  case Data.Aeson.fromJSON val of
    Data.Aeson.Success a -> Just a
    Data.Aeson.Error _ -> Nothing
