# Nammayatri Load Testing

Load testing is automatted via Nix.

### To run the load-test

You can run the load test by running the following in the Nix dev shell:

```
 , run-load-test-dev
```

### To make changes in the load-test

You can check `load-test.nix` file


#### Params which can be modified in the locust command.

`--users 50` (Modify this based on the no of tokens you have)  <br/>
`--run-time 2m` (Modify this based on the time you want to run the script)

####  To create more users

You can go to `.env` and modify the `NUM_DRIVERS` option


### Checking the results

You can check the results at `/load-test/output/driverOffer.html` and `/load-test/output/riderApp.html`. The output files consists of `latency, number of requests and errors` of all the APIs.
