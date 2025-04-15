import React from 'react';
import { render, screen, fireEvent, waitFor } from '@testing-library/react';
import TicketManagementDashboard from '../components/TicketManagementDashboard';
import { TicketProvider } from '../contexts/TicketContext';
import { ThemeProvider } from '../contexts/ThemeContext';

// Mock components to simplify testing
jest.mock('../components/TreeView/TreeViewDashboard', () => {
  return function MockTreeViewDashboard() {
    return <div data-testid="tree-view">Tree View Dashboard</div>;
  };
});

jest.mock('../components/TicketPlace/TicketPlaceList', () => {
  return function MockTicketPlaceList({ onSelectPlace }) {
    return (
      <div data-testid="ticket-place-list">
        Ticket Place List
        <button
          data-testid="select-place-button"
          onClick={() => onSelectPlace({ id: 'test-place-id', name: 'Test Place' })}
        >
          Select Place
        </button>
      </div>
    );
  };
});

jest.mock('../components/TicketPlace/TicketPlaceForm', () => {
  return function MockTicketPlaceForm() {
    return <div data-testid="ticket-place-form">Ticket Place Form</div>;
  };
});

jest.mock('../components/Services/ServicesList', () => {
  return function MockServicesList() {
    return <div data-testid="services-list">Services List</div>;
  };
});

jest.mock('../components/BusinessHours/BusinessHoursList', () => {
  return function MockBusinessHoursList() {
    return <div data-testid="business-hours-list">Business Hours List</div>;
  };
});

describe('TicketManagementDashboard', () => {
  beforeEach(() => {
    // Clear localStorage before each test
    window.localStorage.clear();
  });

  test('should render dashboard title', () => {
    render(
      <ThemeProvider>
        <TicketProvider>
          <TicketManagementDashboard />
        </TicketProvider>
      </ThemeProvider>
    );

    expect(screen.getByText('Ticket Management Dashboard')).toBeInTheDocument();
  });

  test('should show back button', () => {
    render(
      <ThemeProvider>
        <TicketProvider>
          <TicketManagementDashboard />
        </TicketProvider>
      </ThemeProvider>
    );

    // Check that the back button is shown
    expect(screen.getByText('← Back to Ticket Places')).toBeInTheDocument();
  });

  test('should toggle between tree and list views', async () => {
    render(
      <ThemeProvider>
        <TicketProvider>
          <TicketManagementDashboard />
        </TicketProvider>
      </ThemeProvider>
    );

    // Check that tree view is shown by default
    expect(screen.getByTestId('tree-view')).toBeInTheDocument();

    // Switch to list view
    fireEvent.click(screen.getByText('List View'));

    // Check that tree view is no longer shown
    await waitFor(() => {
      expect(screen.queryByTestId('tree-view')).not.toBeInTheDocument();
    });

    // Switch back to tree view
    fireEvent.click(screen.getByText('Tree View'));

    // Check that tree view is shown again
    await waitFor(() => {
      expect(screen.getByTestId('tree-view')).toBeInTheDocument();
    });
  });

  test('should save view state to localStorage', async () => {
    render(
      <ThemeProvider>
        <TicketProvider>
          <TicketManagementDashboard />
        </TicketProvider>
      </ThemeProvider>
    );

    // Switch to list view
    fireEvent.click(screen.getByText('List View'));

    // Check that the view state was saved to localStorage
    expect(window.localStorage.setItem).toHaveBeenCalledWith('activeView', 'list');

    // Switch back to tree view
    fireEvent.click(screen.getByText('Tree View'));

    // Check that the view state was updated in localStorage
    expect(window.localStorage.setItem).toHaveBeenCalledWith('activeView', 'tree');
  });

  test('should save showDashboard state to localStorage when clicking back button', async () => {
    // Mock the initial state to true
    window.localStorage.getItem.mockReturnValue('true');

    render(
      <ThemeProvider>
        <TicketProvider>
          <TicketManagementDashboard />
        </TicketProvider>
      </ThemeProvider>
    );

    // Go back to ticket places
    fireEvent.click(screen.getByText('← Back to Ticket Places'));

    // Check that the showDashboard state was updated in localStorage
    expect(window.localStorage.setItem).toHaveBeenCalledWith('showDashboard', 'false');
  });
});
