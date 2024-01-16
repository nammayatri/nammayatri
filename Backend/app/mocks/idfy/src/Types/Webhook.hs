{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Types.Webhook where

import qualified Data.Aeson as A
import EulerHS.Prelude
import Kernel.External.Verification.Interface.Idfy
import Kernel.Randomizer
import Kernel.Utils.Time

buildSuccessRC :: (MonadIO m) => RCVerificationRequest -> Text -> UTCTime -> m VerificationResponse
buildSuccessRC IdfyRequest {..} request_id now = do
  idNumberRnd <- ("KA-" <>) . show <$> getRandomInRange (10000000, 99999999 :: Int)
  let result =
        RCVerificationOutput
          { avg_gross_vehicle_weight = Nothing,
            axle_configuration = Nothing,
            chassis_number = Just "MB8DP12DMM89XXXXX",
            emission_norms = Just "BHARAT STAGE VI",
            engine_number = Just "AF2127XXXXX",
            fitness_upto = Just "2036-12-27",
            fuel_type = Just "PETROL",
            insurance_details = Nothing,
            insurance_validity = Just "3026-12-21",
            manufacturer = Just "SUZUKI MOTORCYCLE INDIA PVT LTD",
            mv_tax_upto = Nothing,
            owner_name = Just "PRITHIVI RAJ KOTA",
            permit_issue_date = Just "1900-01-01",
            permit_number = Just "",
            permit_type = Nothing,
            permit_validity_upto = Just "3000-01-01",
            puc_validity_upto = Just "1900-01-01",
            registration_date = Just "2021-12-28",
            registration_number = Just idNumberRnd,
            rto_name = Nothing,
            status = Just "id_found",
            vehicle_class = Just "3WT_CAB",
            vehicle_financier = Nothing,
            noc_valid_upto = Just "noc_valid_upto",
            seating_capacity = Just $ A.String "seating_capacity",
            variant = Just "variant",
            npermit_upto = Just "npermit_upto",
            manufacturer_model = Just "manufacturer_model",
            standing_capacity = Just "standing_capacity",
            status_message = Just "status_message",
            number_of_cylinder = Just "number_of_cylinder",
            colour = Just "colour",
            color = Just "color",
            puc_valid_upto = Just "puc_valid_upto",
            permanent_address = Just "permanent_address",
            permit_no = Just "permit_no",
            father_name = Just "father_name",
            status_verfy_date = Just "status_verfy_date",
            m_y_manufacturing = Just "m_y_manufacturing",
            gross_vehicle_weight = Just "gross_vehicle_weight",
            registered_place = Just "registered_place",
            insurance_policy_no = Just "insurance_policy_no",
            noc_details = Just "noc_details",
            npermit_issued_by = Just "npermit_issued_by",
            sleeper_capacity = Just "sleeper_capacity",
            current_address = Just "current_address",
            status_verification = Just "status_verification",
            permit_validity_from = Just "permit_validity_from",
            puc_number = Just "puc_number",
            owner_mobile_no = Just "owner_mobile_no",
            blacklist_status = Just "blacklist_status",
            body_type = Just "body_type",
            unladden_weight = Just "unladden_weight",
            insurance_name = Just "insurance_name",
            owner_serial_number = Just "owner_serial_number",
            vehicle_category = Just "vehicle_category",
            npermit_no = Just "npermit_no",
            cubic_capacity = Just "cubic_capacity",
            norms_type = Just "norms_type",
            financer = Just "financer",
            wheelbase = Just "wheelbase"
          }

  pure
    IdfyResponse
      { action = "verify_with_source",
        completed_at = now,
        created_at = now,
        group_id = group_id,
        request_id = request_id,
        status = "completed",
        task_id = task_id,
        _type = "ind_rc",
        result = Just Output {source_output = Nothing, extraction_output = Just result}
      }

buildSuccessDL :: MonadIO m => DLVerificationRequest -> Text -> UTCTime -> m VerificationResponse
buildSuccessDL IdfyRequest {..} request_id now = do
  idNumberRnd <- ("MH-" <>) . show <$> getRandomInRange (10000000, 99999999 :: Int)
  let result =
        DLVerificationOutput
          { address = Just "Address as available on Gov. Source",
            badge_details = Just "Badge details",
            card_serial_no = Just "Card serial no",
            city = Just "ABC",
            cov_details =
              Just
                [ CovDetail
                    { category = Just "NT",
                      cov = "MCWG",
                      issue_date = Just "2015-05-20"
                    },
                  CovDetail
                    { category = Just "NT",
                      cov = "LMV",
                      issue_date = Just "2015-05-20"
                    }
                ],
            date_of_issue = Just "2015-05-20",
            date_of_last_transaction = Nothing,
            dl_status = Just "Active",
            dob = Just "1985-02-15",
            face_image = Nothing,
            gender = Nothing,
            hazardous_valid_till = Nothing,
            hill_valid_till = Nothing,
            id_number = Just idNumberRnd,
            issuing_rto_name = Just "MH, MOTIHARI",
            last_transacted_at = Nothing,
            name = Just "JOHN CARL DOE",
            nt_validity_from = Just "2015-05-20",
            nt_validity_to = Just "2035-02-14",
            relatives_name = Nothing,
            source = Just "SARATHI",
            status = Just "id_found",
            t_validity_from = Nothing,
            t_validity_to = Nothing
          }

  pure
    IdfyResponse
      { action = "verify_with_source",
        completed_at = now,
        created_at = now,
        group_id = group_id,
        request_id = request_id,
        status = "completed",
        task_id = task_id,
        _type = "ind_driving_license",
        result = Just Output {source_output = Just result, extraction_output = Nothing}
      }
