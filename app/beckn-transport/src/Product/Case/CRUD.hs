module Product.Case.CRUD where

import Beckn.Types.App
import Beckn.Types.Common as BC
import qualified Data.Accessor as Lens
import Data.Aeson
import qualified Data.Text as T
import Data.Time.LocalTime
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Servant
import qualified Storage.Queries.Case as DB
import qualified Beckn.Types.Storage.Case as Storage
import           Types.API.Case
import System.Environment
import Types.API.Registration
import Types.App
import Utils.Routes


list :: CaseReq -> FlowHandler CaseListRes
list CaseReq {..} = withFlowHandler $ do
  DB.findAllByType _limit _offset _type _status

-- Provider Internal API: Update Case

--   1) Create Product with quote and state=TRANSPORTER_ACCEPTED
--   2) Create CaseProduct Entries
-- 	3) Get all products with state=TRANSPORTER_ACCEPTED
--   4) Send /on_search to BG

update :: Text -> FlowHandler Text
update = undefined