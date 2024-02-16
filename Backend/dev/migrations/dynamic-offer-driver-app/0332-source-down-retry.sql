
alter table atlas_driver_offer_bpp.onboarding_document_configs add column max_retry_count int not null default 4;
alter table atlas_driver_offer_bpp.idfy_verification add column retry_count int;