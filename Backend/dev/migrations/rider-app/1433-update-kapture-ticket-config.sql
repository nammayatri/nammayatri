-- this query is for master and (for prod we need to update the config_json with encryptionKey and apiKey with prod values)
UPDATE atlas_app.merchant_service_config
SET config_json = jsonb_set(
      jsonb_set(
        jsonb_set(
          config_json::jsonb,
          '{encryptionUrl}',
          to_jsonb('https://apps.designcan.in/kapture_db'::text)
        ),
        '{encryptionKey}',
        to_jsonb('0.1.0|1|zJ4e85dgsqBsPQ7St65rWHvj7HTNbC+6ro0KKrmfNx/YBnuOILQoe8Tnd+QdzchRmWhlHeBvCdMI3JdrJGoM2dHVPA=='::text)
      ),
      '{apiKey}',
      to_jsonb('0.1.0|1|IqWlYiy6U+r950kIOHPZtnJBZAHQ1aAMyx737x2c2EnXqemEKZpn1FwXz5GadPksTne9X6n6IgLFLtGN9mVFBJgbWwLxi5xPlvxh1zQq3MaNJWcp/j+8QC/7EfmTbf5P9RT/fEevmmz7K5fYXRmv51WoMy4xdz6Rr+fg0Ak='::text)
    )
WHERE service_name = 'Ticket_Kapture';
