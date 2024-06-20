{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.KnowlarityTest where

import qualified Domain.Types.KnowlarityTest
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.KnowlarityTest as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.KnowlarityTest.KnowlarityTest -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.KnowlarityTest.KnowlarityTest] -> m ())
createMany = traverse_ create

findByFrom :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m (Maybe Domain.Types.KnowlarityTest.KnowlarityTest))
findByFrom callFrom = do findOneWithKV [Se.Is Beam.callFrom $ Se.Eq callFrom]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m (Maybe Domain.Types.KnowlarityTest.KnowlarityTest))
findByPrimaryKey callFrom = do findOneWithKV [Se.And [Se.Is Beam.callFrom $ Se.Eq callFrom]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.KnowlarityTest.KnowlarityTest -> m ())
updateByPrimaryKey (Domain.Types.KnowlarityTest.KnowlarityTest {..}) = do updateWithKV [Se.Set Beam.callTo callTo, Se.Set Beam.description description] [Se.And [Se.Is Beam.callFrom $ Se.Eq callFrom]]

instance FromTType' Beam.KnowlarityTest Domain.Types.KnowlarityTest.KnowlarityTest where
  fromTType' (Beam.KnowlarityTestT {..}) = do pure $ Just Domain.Types.KnowlarityTest.KnowlarityTest {callFrom = callFrom, callTo = callTo, description = description}

instance ToTType' Beam.KnowlarityTest Domain.Types.KnowlarityTest.KnowlarityTest where
  toTType' (Domain.Types.KnowlarityTest.KnowlarityTest {..}) = do Beam.KnowlarityTestT {Beam.callFrom = callFrom, Beam.callTo = callTo, Beam.description = description}
