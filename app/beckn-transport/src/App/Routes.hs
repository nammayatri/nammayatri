module App.Routes where

import qualified Beckn.Data.Accessor                  as Accessor
import qualified Beckn.Product.Blacklist              as Blacklist
import qualified Beckn.Product.Comment                as Comment
import qualified Beckn.Product.Customer               as Customer
import qualified Beckn.Product.Document               as Document
import qualified Beckn.Product.HealthCheck            as HealthCheck
import qualified Beckn.Product.Location.CRUD          as Location
import qualified Beckn.Product.Organization           as Organization
import qualified Beckn.Product.Pass                   as Pass
import qualified Beckn.Product.PassApplication.Create as PassApplication
import qualified Beckn.Product.PassApplication.Fetch  as PassApplication
import qualified Beckn.Product.PassApplication.Update as PassApplication
import qualified Beckn.Product.Quota                  as Quota
import qualified Beckn.Product.Registration           as Registration
import qualified Beckn.Product.Tag                    as Tag
import qualified Beckn.Product.User.CRUD              as User
import qualified Beckn.Types.API.Blacklist            as Blacklist
import qualified Beckn.Types.API.Comment              as Comment
import           Beckn.Types.API.Customer
import           Beckn.Types.API.Document
import           Beckn.Types.API.Location.CRUD
import           Beckn.Types.API.Organization
import           Beckn.Types.API.Pass
import           Beckn.Types.API.PassApplication
import qualified Beckn.Types.API.Quota                as Quota
import           Beckn.Types.API.Registration
import qualified Beckn.Types.API.Tag                  as Tag
import qualified Beckn.Types.API.User                 as User
import           Beckn.Types.App
import           Beckn.Types.Common
import qualified Beckn.Types.Storage.Organization     as SO
import qualified Beckn.Types.Storage.Pass             as SP
import qualified Beckn.Types.Storage.User             as User
import qualified Beckn.Types.Storage.PassApplication  as PA
import           Data.Aeson
import qualified Data.Vault.Lazy                      as V
import           EulerHS.Prelude
import           Network.Wai.Parse
import           Servant
import           Servant.Multipart

import qualified Beckn.Types.Storage.Pass             as SP
import qualified Beckn.Types.Storage.PassApplication  as PA
import qualified Beckn.Types.Storage.User             as SU

type TransporterAPIs
    = "v1" :> (    Get '[ JSON] Text
              )

transporterAPIs :: Proxy TransporterAPIs
transporterAPIs = Proxy

transporterServer' :: V.Key (HashMap Text Text) -> FlowServer TransporterAPIs
transporterServer' key =
        HealthCheck.healthCheckApp