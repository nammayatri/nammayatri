ALTER TABLE atlas_app.quote
  ADD CONSTRAINT quote_unique_reqid_bppid_quoteid UNIQUE (request_id, provider_id);