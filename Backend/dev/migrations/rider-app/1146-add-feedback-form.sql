-- add the migrations as it is without changing the id's
INSERT INTO atlas_app.feedback_form (category_name, id, rating, question, answer, answer_type)
VALUES('RIDE','1',1,'Give feedback for your ride ?',ARRAY['Felt Unsafe','Too Many Calls','Late Drop Off','Late Pick Up'], 'Checkbox'),
      ('RIDE','2',2,'Give feedback for your ride ?',ARRAY['Felt Unsafe','Too Many Calls','Late Drop Off','Late Pick Up'], 'Checkbox'),
      ('RIDE','3',3,'Give feedback for your ride ?',ARRAY['Trip Got Delayed','Felt Unsafe'], 'Checkbox'),
      ('RIDE','4',4,'Give feedback for your ride ?',ARRAY['Trip Got Delayed','Safe Ride'], 'Checkbox'),
      ('RIDE','5',5,'Give feedback for your ride ?',ARRAY['Expert Driving','Safe Ride'], 'Checkbox'),
      ('DRIVER','6',1,'Give feedback for your driver ?', ARRAY ['Rude Driver','Reckless Driving','Driver Charged More'],'Checkbox'),
      ('DRIVER','7',2,'Give feedback for your driver ?', ARRAY ['Rude Driver','Reckless Driving','Driver Charged More'],'Checkbox'),
      ('DRIVER','8',3,'Give feedback for your driver ?', ARRAY ['Unprofessional Driver','Rash Driving','Driver Charged More'],'Checkbox'),
      ('DRIVER','9',4,'Give feedback for your driver ?', ARRAY ['Polite Driver','Expert Driving','Asked For Extra Fare'],'Checkbox'),
      ('DRIVER','10',5,'Give feedback for your driver ?', ARRAY ['Polite Driver','On Time','Skilled Navigator'],'Checkbox'),
      ('VEHICLE','11',3,'Give feedback for the vehicle ?', ARRAY ['Uncomfortable Auto'],'Checkbox'),
      ('VEHICLE','12',4,'Give feedback for the vehicle ?', ARRAY ['Uncomfortable Auto'],'Checkbox'),
      ('VEHICLE','13',5,'Give feedback for the vehicle ?', ARRAY ['Clean Auto'],'Checkbox');