{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}


module Storage.Queries.DriverQuote (module Storage.Queries.DriverQuote, module ReExport) where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import Storage.Queries.DriverQuoteExtra as ReExport
import Storage.Queries.Transformers.DriverQuote
import qualified Domain.Types.DriverQuote
import qualified Storage.Beam.DriverQuote as Beam
import qualified Kernel.Types.Id
import qualified Domain.Types.Person
import qualified Domain.Types.SearchTry
import qualified Sequelize as Se



deleteByDriverId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
deleteByDriverId driverId = do deleteWithKV [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]
findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.DriverQuote.DriverQuote -> m (Maybe Domain.Types.DriverQuote.DriverQuote))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]
findDriverQuoteBySTId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.SearchTry.SearchTry -> m (Maybe Domain.Types.DriverQuote.DriverQuote))
findDriverQuoteBySTId searchTryId = do findOneWithKV [Se.Is Beam.searchTryId $ Se.Eq (Kernel.Types.Id.getId searchTryId)]
findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.DriverQuote.DriverQuote -> m (Maybe Domain.Types.DriverQuote.DriverQuote))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]



