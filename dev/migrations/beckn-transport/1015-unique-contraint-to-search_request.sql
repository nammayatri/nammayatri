ALTER TABLE atlas_transporter.search_request
  ADD CONSTRAINT search_request_unique_txnid_bapid_bppid UNIQUE (transaction_id, bap_id, provider_id);