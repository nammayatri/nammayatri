## ART (Automated Response Testing)

### Introduction
ART (Automated Response Testing) is a tool designed to automate the testing of API responses for discrepancies between expected and actual responses. It facilitates monitoring the consistency and accuracy of API endpoints by comparing responses with predefined expectations.

### Prerequisites
Before using ART, ensure that you have installed all the necessary Python dependencies. Navigate to the Backend/ART directory and install dependencies using the following command:

`pip install -r requirements.txt`

### Configuration
To enable ART functionalities, follow these steps:

Export the following commands into your terminal:
`export IS_ART_REPLAYER_ENABLED=True`
`export SHOULD_LOG_REQUEST_ID=True`
Run all services using the command:
`, run-mobility-stack-dev`

Ensure that all services are running correctly before proceeding.

### Running ART
To initiate the ART process, execute the following command within the ART directory in your terminal:

`python3 ARTAPI.py`

Wait for the process to complete. ART will display API response statuses (success/failure) and perform a diff check to identify discrepancies in the responses of various API calls.

### Reviewing Results
After the ART process is completed, you will receive summarized information regarding API response discrepancies. For detailed insights, navigate to the `Backend/ART/ArtLogs` folder. Here, you'll find files detailing the identified differences and the number of API calls made.

### Taking Action
Review the generated logs to identify any discrepancies between expected and actual API responses. Based on the findings, take necessary actions to rectify issues or inconsistencies in the API endpoints.

### Additional Notes
- Ensure that the environment is properly set up before running ART to obtain accurate results.
- Regularly monitor API responses using ART to maintain the integrity and reliability of your APIs.

