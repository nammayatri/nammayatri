ALTER TABLE atlas_app.product_instance
  ADD COLUMN discount double precision;

ALTER TABLE atlas_app.product_instance
  ADD COLUMN estimated_total_fare numeric(30,2);

ALTER TABLE atlas_app.product_instance
  ADD COLUMN total_fare numeric(30,2);

UPDATE atlas_app.product_instance AS T1
  SET estimated_total_fare =
    ( SELECT T1.price
        FROM atlas_app.product_instance AS T2
        WHERE T1.id = T2.id
    );
