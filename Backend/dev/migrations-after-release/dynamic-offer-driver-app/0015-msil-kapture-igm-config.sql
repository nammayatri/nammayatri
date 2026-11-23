-- MSIL Kapture: switch to MSILKapture flow variant with separate auth keys per API.
-- Auth values are raw base64 keys (without "Basic " prefix) — the shared-kernel prepends "Basic " at runtime.
-- The Auth keys are UAT passetto encrypted. Please encrypt again before running the migration
UPDATE atlas_driver_offer_bpp.merchant_service_config
SET config_json = (
      jsonb_set(
        jsonb_set(
          jsonb_set(
            jsonb_set(
              jsonb_set(
                config_json::text::jsonb,
                '{flowVariant}', '"MSILKapture"'
              ),
              '{addTicketAuth}', '"0.1.0|1|NPZPbB2O8BITmdi4/ml0ROzo7N8yD5fd2+uKQ9atXmzFiLmFRYAjlkh5hHhLyuZfbgyer3DHlHcdnSRjFFxLLXPUxzTipT6Nm/MfYGJEFWI8qprqlafznZ1p0h7kJFxzUrinDDLB4Em5F019RL2nTz3ew+t4uZM="'
            ),
            '{updateTicketAuth}', '"0.1.0|2|zq7n8sXez1xBJsEyLrQe62KbJK3s93USxD7D1RXv33qrlmBo+P43591zULHrhadmesCx8uMjR8FlGn9z3h8uoG+qaLR7zxeNeEbpM8li8sfCfPK27z5+HOw5NGVqOHZgPBFoUDZ5o8FDvMpabz6Gv2pvMZNlH3A="'
          ),
          '{getTicketAuth}', '"0.1.0|2|mtBqE4F1a9kQe99fLBVAjrsN/bOrzneUTjqjMGdNmVzQoKOcnyjqjtCXhpPkcsZWmjSFo+vvGBZM1qD7T24LYoqxnWglZgE5eGiEmN/b4/BjVl9bxhqfhWLjmxgE2DPffPOwaIGiXduY+pkRIxgj7B1EgpZf+zU="'
        ),
        '{pullTicketAuth}', '"0.1.0|0|6qlrQBLOTBBe3uRYzTLcnPizCTaqlKcvXMrzwapHOw3C+JZFbdB5TYy7ORsddVFLNbP/Fekej8hZzoUFqHMOkGQOqhXTvd0JBiw46gZ2wKQd6Ljhp1MwI5454ZDlMra7oP1lTo1RBcqZWOVMztuIGfnCpHpjoB8="'
      )
    )::json,
    updated_at = now()
WHERE merchant_id IN (
    SELECT merchant_id
    FROM atlas_driver_offer_bpp.merchant_operating_city
    WHERE merchant_short_id = 'MSIL_PARTNER'
)
  AND service_name = 'Ticket_Kapture';
