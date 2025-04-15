import React from 'react';
import { TicketProvider } from './contexts/TicketContext';
import { ThemeProvider } from './contexts/ThemeContext';
import TicketManagementDashboard from './components/TicketManagementDashboard';
import 'bootstrap/dist/css/bootstrap.min.css';
import './styles/theme.css';
import './styles/Dashboard.css';



function App() {
  return (
    <ThemeProvider>
      <TicketProvider>
        <TicketManagementDashboard />
      </TicketProvider>
    </ThemeProvider>
  );
}

export default App;
