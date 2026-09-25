{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Clickhouse.DriverPlan where

import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Plan as DPlan
import Kernel.Prelude
import Kernel.Storage.ClickhouseV2 as CH
import qualified Kernel.Storage.ClickhouseV2.UtilsTH as TH
import Kernel.Types.Id

data DriverPlanT f = DriverPlanT
  { driverId :: C f (Id DP.Person),
    planId :: C f Text,
    merchantOpCityId :: C f (Maybe Text),
    serviceName :: C f (Maybe Text)
  }
  deriving (Generic)

deriving instance Show DriverPlan

driverPlanTTable :: DriverPlanT (FieldModification DriverPlanT)
driverPlanTTable =
  DriverPlanT
    { driverId = "driver_id",
      planId = "plan_id",
      merchantOpCityId = "merchant_op_city_id",
      serviceName = "service_name"
    }

type DriverPlan = DriverPlanT Identity

$(TH.mkClickhouseInstances ''DriverPlanT 'SELECT_FINAL_MODIFIER)

findDriverIdsByPlanId ::
  CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m =>
  Id DPlan.Plan ->
  Id DMOC.MerchantOperatingCity ->
  m [Id DP.Person]
findDriverIdsByPlanId planId merchantOpCityId =
  CH.findAll $
    CH.select_
      ( \dp -> do
          CH.groupBy dp.driverId $ \driverIdAgg -> driverIdAgg
      )
      $ CH.filter_
        ( \dp ->
            dp.planId CH.==. planId.getId
              CH.&&. dp.merchantOpCityId CH.==. Just merchantOpCityId.getId
        )
        (CH.all_ @CH.APP_SERVICE_CLICKHOUSE driverPlanTTable)
