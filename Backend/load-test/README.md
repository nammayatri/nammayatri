### To set up your environment for load-test

```
cd ./Backend/load-test
bash setup.sh
source ./bin/activate
pip3 install -r requirements.txt
```

### To run the load-test

```
, run-load-test
```


### To make changes in the load-test

You can check `scripts.nix` file

```
locust --headless --run-time 1m --users 50  --only-summary --html ./output/riderApp.html -f ./scripts/riderApp.py
```

#### Params which can be modified in the locust command.

`--users 50` (Modify this based on the no of tokens you have)  <br/>
`--run-time 2m` (Modify this based on the time you want to run the script)

####  To create more users

You can go to `.env` and modify the `NUM_DRIVERS` option


### Checking the results

You can check the results at `/load-test/output/driverOffer.html` and `/load-test/output/riderApp.html` <br/>
The output files consists of `latency, number of requests and errors` of all the APIs.


Note:- If there are background process running kill using the below command
`ps | grep ".py" | awk '{print $1}' | xargs -r kill`