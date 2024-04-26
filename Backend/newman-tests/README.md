## Running tests

In nix dev shell,

- Run the servers using `, run-mobility-stack-dev`.

- Once the servers are up and running, run the tests using `, run-integration-tests`.

## Workflow to add new tests:

- Create a new postman collection with all the APIs involved in sequence

- Add the following code to every API tests section in postman. This checks that API returned successfully

  ```javascript
  // Assert if request was successful
  pm.test("Expect response code to be 200", () => {
      if (pm.response.code !== 200) {
          console.log('req', request)
          console.log('res', responseBody)
          pm.response.to.have.status(200)
      }
  });
  ```

- You can programmatically control which API to be called next using `postman.setNextRequest()` function.

> [!NOTE]
> `postman.setNextRequest()` function will only be executed once all the other js code in the tests section has been executed.

- You can program polling of an API using the following pattern:

  ```javascript
  const retry = 5
  let count = postman.getEnvironmentVariable("ncrr_count"); // set the retry count in the environment variable
  if (count == null || count == undefined) {
      count = 0;
      postman.setEnvironmentVariable("ncrr_count", count);
  }

  if (count > retry) {
      // Polling failed case. You can raise an error here which stops the test
  }

  if (count <= retry && body.searchRequestsForDriver.length ==0) {
      count++;
      postman.setEnvironmentVariable("ncrr_count", count); // increment counter
      postman.setNextRequest("<api name>"); // schedule the same API again
  }
  ```

- After setting up the API sequence in postman, you can export the collection into newman-tests/tests folder

> [!NOTE]
> Follow the naming convention for adding new test folder - `<test-number>-<test-name>`


- You can use `pre-test.sh` and `post-test.sh` to setup and validate the various states like DB, Redis etc. These scripts will be run by `run-tests.sh`

  There are existing utils commands in `/utils` folder which can be used in pre and post test hooks. Ex: `checkStuckEntities.sh` can be used to check if there are any stuck booking/ride in the DB and fail the test case if there are any.