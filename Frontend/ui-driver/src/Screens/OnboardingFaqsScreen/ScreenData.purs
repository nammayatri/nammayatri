{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.OnboardingFaqsScreen.ScreenData where

import Data.Maybe
import Prelude
import Screens.Types as ST
import ConfigProvider
import Common.Types.App (LazyCheck(..))

initData :: ST.OnboardingFaqsScreenState
initData = {
  data: {
    categoryToQuestionAnsMap : dummyQuestions,
    selectedSectionQnAList : []
  },
  props: {
    selectedCategory : Nothing,
    selectedCategoryIndex : Nothing,
    showAns : false,
    selectedQnA : {question : "", answer : ""}
  }
} 


dummyQuestions :: Array ST.CategoryToQuestionAnsMap
dummyQuestions = [ 
    { category : "Setting up my account"
    , questionAnsMap : [
        { question : "How do I become a register as a Fleet operator on MSIL FleetX?"
        , answer : "To register as a Fleet Operator, visit the <span style='color:#2194FF'>msilfleetx.com</span> and follow the on-screen instructions to complete the sign-up process."
        },
        { question : "How do I choose my preferred language?"
        , answer : "Navigate to the top right corner of the Welcome page, and select your desired language from the list."
        },
        { question : "Can I Sign-Up with my Email?"
        , answer : "For registering onto MSIL FleetX, you need a valid mobile no. <br><br>Additionally you can enter your email ID in the \"Get Started\" section after registration to receive timely updates on mail."
        },
        { question : "I am not able to receive OTP for login"
        , answer : "If you're having trouble receiving an OTP: <br>1. Click \"Resend OTP\" <br>2.If the issue continues, you can contact us at you can call us +91xxxxxxxxx or reach out to us as <span style='color:#2194FF'>msilfleetx@maruti.co.in</span>"
        },
        { question : "What is the difference between Individual Fleet and Business Fleet?"
        , answer : "You can register as a Business Fleet only if your fleet is a registered fleet business and you hold the necessary documents (Business License, GST Number, Business PAN). <br><br>If not, you can register as an Individual Fleet Operator."
        },
        { question : "What details do I need to provide to complete my registration?"
        , answer : "As a business fleet operator you need to enter/upload the following: <br>1. Fleet Name <br>2. Registered Mobile Number <br>3. City of Operations <br>4. Email ID <br>5. Operator Code (Optional) <br>6. Business PAN number and document <br>7. GST number and document <br>8. Business License number and document  <br><br>For an Individual Fleet Operator you need to enter/upload the following: <br>1. Fleet Owner Name <br>2. Registered Mobile Number <br>3. City of Operations <br>4. Email ID <br>5. Operator Code (Optional) <br>6. Aadhaar Card number and document <br>7. Personal PAN number and document"
        },
        { question : "What cities can I operate in?"
        , answer : "As of now, you can operate within the Delhi-NCR region."
        },
        { question : "What is the operator referral code?"
        , answer : "The Operator Referral Code is a unique 4-digit code given to you by your onboarding partner. <br><br>To ensure you're correctly tagged to your onboarding partner, enter the code in the 'Operator code' field during the onboarding process."
        },
        { question : "What documents am I required to upload?"
        , answer : "In case of a Business Fleet operator: <br>1. Business PAN Card <br>2. GST Certificate <br>3. Business License <br><br>In case of an Individual Fleet Operator:  <br>1. Aadhaar Card <br>2. Personal PAN card"
        },
        { question : "What do I do if I need assistance during the registration process? "
        , answer : "Click the Help option in the top right corner of the screen to access a dropdown menu with: <br><br>1. Contact MSIL Support <br>2. Change Language <br>3. Logout Option <br><br>Click on contact MSIL support or reach out to us as <span style='color:#2194FF'msilfleetx@maruti.co.in</span>"
        }
      ]
    },
    { category : "Managing my account"
    , questionAnsMap : [
        { question : "How to update my documents?"
        , answer : "Go to Profile > Profile Details and click on Edit button. Upload the documents that need update and click Save."
        },
        { question : "Where can I view my profile details?"
        , answer : "Navigate to the bottom of you screen, and click on the profile photo icon to view your profile details."
        },
        { question : "How can I logout of my fleet account?"
        , answer : "Navigate to the bottom of you screen, and click on the profile photo icon to find the log-out option."
        },
        { question : "How do I add or remove a profile picture?"
        , answer : "Go to the \"Profile\" tab from the navigation bar. Tap on the photo icon and click the edit photo icon, upload photo and click Save"
        }
      ]
    },
    { category : "Adding and managing drivers"
    , questionAnsMap : [
        { question : "Will MSIL provide me with drivers for my fleet?"
        , answer : "No, MSIL does not provide drivers for your fleet."
        },
        { question : "Can I add muliple drivers to my fleet?"
        , answer : "Yes, you can onboard as many drivers as you like, provided they have valid documentation."
        },
        { question : "How do I onboard a driver on my fleet?"
        , answer : "Once you have completed the sign up process, you will be directed to your Fleet Dashboard. <br><br>Click on 'Drivers' on the left-side panel. Click on \"Add driver\" on the top right corner of the screen and follow the on screen instructions to add drivers to your fleet."
        },
        { question : "How do I add a driver manually?"
        , answer : "To add drivers manually: <br><br>1. Go to your Driver Dashboard <br>2. Click on \"Add Driver\" at the top right corner <br>3. Select \"Single Driver\" <br>4. Enter the driver’s contact number and click on \"Send OTP\" <br>5.Once the OTP is received and entered, a driver profile will be created and automatically linked to your fleet <br><br>Note: Driver documents must be uploaded manually after the driver’s name appears under your fleet. You can search for the driver using his/her DL no. or mobile no. to start adding documents for your driver."
        },
        { question : "How does driver bulk upload help?"
        , answer : "To onboard multiple drivers at once:<br><br> 1. Go to your Driver Dashboard <br>2. Click on \"Add Driver\" at the top right corner <br>3. Select \"Bulk Upload\" <br>4. Upload a CSV file containing driver names and contact numbers <br>5. Upon verification, each driver will receive a joining link. This link redirects them to the Play Store to download the MSIL Fleetx app making onboarding a one-time and seamless process <br>6. Drivers will be automatically linked to your fleet <br><br>Note: Driver documents will still need to be uploaded separately for each driver, either by the drivver or by you. You can search for the driver using his/her DL no. or mobile no. to start adding documents for your driver."
        },
        { question : "What documents do I need to onboard a driver?"
        , answer : "To onboard a driver, you will need the following documents:<br><br>1. Driving License <br>2. Aadhaar Card <br>3. PAN Card"
        },
        { question : "What is the Driver tab?"
        , answer : "The Driver tab provides you with a list of drivers under operating under your fleet. The page also includes driver status, driver contact, active vehicle, and document verification status. The Driver dashboard also allows you to change and filter your driver view as per your needs."
        },
        { question : "Can I track my driver's documentation and onboarding verification on my dashboard?"
        , answer : "Yes, on the driver dashboard, view the column \"Driver Docs\". Under the column, along side each driver, you can track their document verification and onboarding status. "
        },
        { question : "Can I add or remove drivers from my fleet profile?"
        , answer : "Yes, go to \"Drivers\" tab <br><br>To add a driver, click on \"Add New Driver\", upload the required documents and link them to a vehicle. <br><br>To remove a driver, select the driver from the list, click \"Remove\", and confirm the action. "
        },
        { question : "How do I manage driver documents?"
        , answer : "Navigate to \"Drivers\" tab and view the coloumn of Vehicle Docs. Click to view driver documents and status."
        }
      ]
    },
    { category : "Adding and managing vehicles"
    , questionAnsMap : [
        { question : "How do I onboard a vehicle to my fleet?"
        , answer : "Once you have completed the sign up process, you will be directed to your Fleet Dashboard. <br>1. Click on \"Vehicles\" in the left-side panel <br>2. Then click on \"Add Vehicle\" at the top right corner <br>3. Follow the on-screen instructions to complete the vehicle addition process"
        },
        { question : "Can I add multiple vehicles to my fleet at once?"
        , answer : "Yes, you can add multipe vehicles to your fleet by clicking on Bulk upload.You can refer the bulk upload template (.csv) file and add the vehicles to be onboarded in the same csv format"
        },
        { question : "What documents do I need to Onboard a vehicle?"
        , answer : "Vehicle Documents required for onboaring vehicle: <br><br>1. Registration Certificate <br>2. Vehicle Permit <br>3. Vehicle PUC Certificate <br>4. Vehicle Fitness Certificate <br>5. Vehicle Insurance <br>6. Vehicle Photos"
        },
        { question : "Can I re-add a vehicle that I had previously deleted?"
        , answer : "Yes, you can add the vehicle which was previously deleted but you have to re-upload the vehicle documents for verification and vehicle will be active only post successful verification"
        },
        { question : "What is vehicle inspection and why is it required?"
        , answer : "In order to complete vehicle onboarding, you need to visit a MSIL Vehicle inspection hub in order to complete a short and simple vehicle quality check. <br><br>You will be guided by a designated MSIL Onboarding partner throughout the process to ensure a seamless onboarding experience. <br><br>In order to schedule a vehicle inspection, select a MSIL vehicle inspection hub from the drop down list and select continue. You will be a contacted by a MSIL onboarding partner within 24 hours to complete your vehicle inspection"
        },
        { question : "Is there a cost for vehicle inspection?"
        , answer : "No, vehicle inspection is completely free of cost. <br><br>MSIL will arrange for vehicle inspection as part of the onboarding, and you will not be charged for it."
        },
        { question : "How do I manage vehicle documents?"
        , answer : "Navigate to \"Vehicles\" tab from the left side panel. You can track, view and update vehicle documents under the Vehicle Docs column along side each vehicle."
        },
        { question : "How can I track pending vehicle document uploads across my fleet?"
        , answer : "Navigate to \"Vehicles\" tab from the left side panel. You can track, view and update vehicle documents under the Vehicle Docs column along side each vehicle."
        }
      ]
    },
    { category : "Mapping drivers and vehicles"
    , questionAnsMap : [
        { question : "Can I assign a specific driver to a specific vehicle?"
        , answer : "Yes, you can assign any driver to any vehicle. You can even assign multiple drivers to the same vehicle. However, please note that 2 drivers cannot be active on the same vehicle at the same time"
        },
        { question : "How can I remove or delete a vehicle from my fleet?"
        , answer : "To remove a vehicle: <br>1. Go to the Vehicle Dashboard <br>2. Click on the 3 dots next to the vehicle <br>3. Select \"Delete\" from the drop down"
        },
        { question : "How do I edit details of an existing vehicle?"
        , answer : "To edit any vehicle details: <br>1. Click the 3 dots next to the vehicle on your dashboard <br>2. Choose \"Edit\" from the pop-up menu <br>3. You can then update vehicle information as needed. <br><br>Driver assigned to the vehicle can also edit the details throught the MSIL Fleetx app."
        },
        { question : "How to update the status of a certain driver as active/inactive?"
        , answer : "To update the status of driver: <br>1.Go to the Driver Dashboard <br>2. Click on the 3 dots next to the driver status to be changed and select \"Edit\" <br>3. Update the status and vehicle assignment to driver via the dropdown."
        }
      ]
    }
  ]