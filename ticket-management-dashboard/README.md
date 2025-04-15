# Ticket Management Dashboard

A React-based dashboard for managing ticket places, business hours, services, categories, and special occasions.

## Features

- **Ticket Place Management**: Edit details about the ticket place, including name, description, address, and coordinates.
- **Business Hours Management**: Create, edit, and delete business hours with support for time slots or duration-based hours.
- **Special Occasions Management**: Create, edit, and delete special occasions with support for specific dates or weekly occurrences.
- **JSON Preview**: Real-time preview of the JSON data that will be sent to the API.
- **Data Persistence**: All data is saved to localStorage and can be downloaded as a JSON file.
- **File Upload**: Load data from a JSON file.
- **API Integration**: Integration with backend API endpoints for creating, reading, and updating ticket places.
- **API Configuration**: Configure API base URL and token in the application.

## Project Structure

```
ticket-management-dashboard/
├── public/                  # Public assets
├── src/                     # Source code
│   ├── assets/              # Static assets
│   ├── components/          # React components
│   │   ├── BusinessHours/   # Business hours components
│   │   ├── Categories/      # Categories components
│   │   ├── Common/          # Common components
│   │   ├── Services/        # Services components
│   │   ├── SpecialOccasions/# Special occasions components
│   │   └── TicketPlace/     # Ticket place components
│   ├── contexts/            # React contexts
│   ├── hooks/               # Custom hooks
│   ├── utils/               # Utility functions
│   ├── App.js               # Main App component
│   └── index.js             # Entry point
└── package.json             # Dependencies and scripts
```

## API Endpoints

The application uses the following API endpoints:

- `GET /ticketdashboard/ticketplaces` - Get a list of all ticket places
- `GET /ticketdashboard/ticketplace/{ticketPlaceId}/info` - Get detailed information about a specific ticket place
- `POST /ticketdashboard/ticketplace/update` - Create or update a ticket place

## Data Model

The application uses the following data model:

### Ticket Place
- `id`: Unique identifier (UUID)
- `name`: Name of the place
- `description`: Description of the place
- `shortDesc`: Short description of the place
- `address`: Address of the place
- `latitude`: Latitude coordinate
- `longitude`: Longitude coordinate
- `status`: Status of the place ("Active" or "Inactive")
- `priority`: Priority of the place
- `placeType`: Type of the place ("Museum", "ThemePark", etc.)
- `allowSameDayBooking`: Whether same-day booking is allowed
- `gallery`: Array of image URLs
- `iconUrl`: URL of the icon image
- `mapImageUrl`: URL of the map image
- `termsAndConditions`: Array of terms and conditions
- `termsAndConditionsUrl`: URL to the terms and conditions page
- `openTimings`: Opening time in "HH:MM:SS" format
- `closeTimings`: Closing time in "HH:MM:SS" format
- `services`: Array of services
- `businessHours`: Array of business hours
- `serviceCategories`: Array of service categories
- `servicePeopleCategories`: Array of service people categories
- `specialOccasions`: Array of special occasions

### Business Hours
- `id`: Unique identifier
- `name`: Name of the business hours
- `btype`: Type of business hours
  - `tag`: Type of business hours ("Slot" or "Duration")
  - `contents`: For "Slot", a string in "HH:MM:SS" format. For "Duration", an array of two strings in "HH:MM:SS" format (start and end time).
- `categoryId`: Array of service category IDs
- `bookingClosingTime`: Time when booking closes in "HH:MM:SS" format

### Service
- `id`: Unique identifier (UUID)
- `service`: Name of the service
- `shortDesc`: Short description of the service
- `operationalDays`: Array of days of the week ("Monday", "Tuesday", etc.)
- `operationalDate`: Specific date when the service is operational, in "YYYY-MM-DD" format
- `maxVerification`: Maximum number of verifications allowed
- `allowFutureBooking`: Whether future booking is allowed
- `allowCancellation`: Whether cancellation is allowed
- `expiry`: Expiry details for the service
  - `tag`: Type of expiry ("InstantExpiry" or "VisitDate")
  - `contents`: For "InstantExpiry", number of minutes after which the service expires. For "VisitDate", time of day in "HH:MM:SS" format after which the service expires.
- `businessHours`: Array of business hour IDs

### Service Category
- `id`: Unique identifier (UUID)
- `name`: Name of the service category
- `description`: Description of the service category
- `allowedSeats`: Total number of seats allowed
- `availableSeats`: Number of seats currently available
- `peopleCategory`: Array of service people category IDs

### Service People Category
- `id`: Unique identifier (UUID)
- `name`: Name of the service people category
- `description`: Description of the service people category
- `pricingType`: Type of pricing ("AllDays" or "SameDay")
- `priceAmount`: Price amount
- `priceCurrency`: Currency of the price (e.g., "INR")
- `timeBounds`: Time bounds for the service people category
  - `tag`: Type of time bounds ("Unbounded", "BoundedByDay", or "BoundedByWeekday")
  - `contents`: For "Unbounded", no additional data. For "BoundedByDay", a list of tuples with a date and a list of time ranges. For "BoundedByWeekday", an object with fields for each day of the week.
- `vendorSplitDetails`: Array of vendor split details
  - `vendorId`: Unique identifier for the vendor
  - `splitAmount`: Amount to be split to the vendor (can include decimal values)
  - `splitType`: Type of split (currently only "FIXED" is supported)

### Special Occasions
- `id`: Unique identifier (UUID)
- `name`: Name of the special occasion
- `description`: Description of the special occasion
- `specialDayType`: Type of special occasion ("Open" or "Closed")
- `date`: Specific date for the occasion in "YYYY-MM-DD" format
- `dayOfWeek`: Day of the week for weekly occasions ("Monday", "Tuesday", etc.)
- `entityId`: ID of the entity (usually a service category) associated with the special occasion
- `businessHours`: Array of business hour IDs
- `placeId`: ID of the ticket place associated with the special occasion

## Technologies Used

- React
- React Bootstrap
- UUID
- Axios
- React Router DOM
- FontAwesome

This project was bootstrapped with [Create React App](https://github.com/facebook/create-react-app).

## Available Scripts

In the project directory, you can run:

### `npm start`

Runs the app in the development mode.\
Open [http://localhost:3000](http://localhost:3000) to view it in your browser.

The page will reload when you make changes.\
You may also see any lint errors in the console.

### `npm test`

Launches the test runner in the interactive watch mode.\
See the section about [running tests](https://facebook.github.io/create-react-app/docs/running-tests) for more information.

### `npm run build`

Builds the app for production to the `build` folder.\
It correctly bundles React in production mode and optimizes the build for the best performance.

The build is minified and the filenames include the hashes.\
Your app is ready to be deployed!

See the section about [deployment](https://facebook.github.io/create-react-app/docs/deployment) for more information.

### `npm run eject`

**Note: this is a one-way operation. Once you `eject`, you can't go back!**

If you aren't satisfied with the build tool and configuration choices, you can `eject` at any time. This command will remove the single build dependency from your project.

Instead, it will copy all the configuration files and the transitive dependencies (webpack, Babel, ESLint, etc) right into your project so you have full control over them. All of the commands except `eject` will still work, but they will point to the copied scripts so you can tweak them. At this point you're on your own.

You don't have to ever use `eject`. The curated feature set is suitable for small and middle deployments, and you shouldn't feel obligated to use this feature. However we understand that this tool wouldn't be useful if you couldn't customize it when you are ready for it.

## Learn More

You can learn more in the [Create React App documentation](https://facebook.github.io/create-react-app/docs/getting-started).

To learn React, check out the [React documentation](https://reactjs.org/).

### Code Splitting

This section has moved here: [https://facebook.github.io/create-react-app/docs/code-splitting](https://facebook.github.io/create-react-app/docs/code-splitting)

### Analyzing the Bundle Size

This section has moved here: [https://facebook.github.io/create-react-app/docs/analyzing-the-bundle-size](https://facebook.github.io/create-react-app/docs/analyzing-the-bundle-size)

### Making a Progressive Web App

This section has moved here: [https://facebook.github.io/create-react-app/docs/making-a-progressive-web-app](https://facebook.github.io/create-react-app/docs/making-a-progressive-web-app)

### Advanced Configuration

This section has moved here: [https://facebook.github.io/create-react-app/docs/advanced-configuration](https://facebook.github.io/create-react-app/docs/advanced-configuration)

### Deployment

This section has moved here: [https://facebook.github.io/create-react-app/docs/deployment](https://facebook.github.io/create-react-app/docs/deployment)

### `npm run build` fails to minify

This section has moved here: [https://facebook.github.io/create-react-app/docs/troubleshooting#npm-run-build-fails-to-minify](https://facebook.github.io/create-react-app/docs/troubleshooting#npm-run-build-fails-to-minify)
