-- NOTE: POST SUCCESSFUL RELEASE, RUN BELOW QUERY, As `template` is already moved to `merchant_message` table it is safe to remove it from `partner_org_config`
UPDATE atlas_app.partner_org_config
SET config_json = config_json :: jsonb - 'template'
WHERE config_type = 'TICKET_SMS';