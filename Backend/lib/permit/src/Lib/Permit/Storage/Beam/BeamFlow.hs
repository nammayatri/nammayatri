module Lib.Permit.Storage.Beam.BeamFlow where

import Kernel.Beam.Lib.UtilsTH as Reexport
import Kernel.Types.Common as Reexport hiding (id)
import Kernel.Utils.Common
import qualified Lib.Permit.Storage.Beam.PermitApplicant as BeamPA
import qualified Lib.Permit.Storage.Beam.PermitApplication as BeamPAp
import qualified Lib.Permit.Storage.Beam.PermitApplicationDocument as BeamPAD
import qualified Lib.Permit.Storage.Beam.PermitApplicationEvent as BeamPAE

type BeamFlow m r =
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    HasPermitTablesSchema
  )

type HasPermitTablesSchema =
  ( HasSchemaName BeamPA.PermitApplicantT,
    HasSchemaName BeamPAp.PermitApplicationT,
    HasSchemaName BeamPAD.PermitApplicationDocumentT,
    HasSchemaName BeamPAE.PermitApplicationEventT
  )
