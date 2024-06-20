
CREATE TABLE atlas_app.knowlarity_test ();

------- SQL updates -------

ALTER TABLE atlas_app.knowlarity_test ADD COLUMN description text ;
ALTER TABLE atlas_app.knowlarity_test ADD COLUMN call_to text NOT NULL;
ALTER TABLE atlas_app.knowlarity_test ADD COLUMN call_from text NOT NULL;
ALTER TABLE atlas_app.knowlarity_test DROP CONSTRAINT knowlarity_test_pkey;
ALTER TABLE atlas_app.knowlarity_test ADD PRIMARY KEY ( call_from);