Alter table atlas_app.payment_order alter column client_auth_token_encrypted drop not null;
Alter table atlas_app.payment_order alter column client_auth_token_hash drop not null;
Alter table atlas_app.payment_order alter column client_auth_token_expiry drop not null;