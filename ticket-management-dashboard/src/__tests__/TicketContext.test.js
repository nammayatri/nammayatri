import React from 'react';
import { render, screen, fireEvent, waitFor } from '@testing-library/react';
import { TicketProvider, useTicket } from '../contexts/TicketContext';

// Test component that uses the TicketContext
const TestComponent = () => {
  const { 
    getTicketPlaceIds, 
    getActiveTicketPlace, 
    setActiveTicketPlace,
    addTicketPlace,
    updateTicketPlace,
    deleteTicketPlace,
    clearAllTicketData
  } = useTicket();

  const ticketPlaceIds = getTicketPlaceIds();
  const activePlace = getActiveTicketPlace();

  const handleAddPlace = () => {
    const newPlace = {
      name: 'Test Place',
      description: 'A test place',
      placeType: 'Museum',
      services: [],
      businessHours: [],
      serviceCategories: [],
      servicePeopleCategories: [],
      specialOccasions: []
    };
    addTicketPlace(newPlace);
  };

  const handleUpdatePlace = () => {
    if (activePlace) {
      updateTicketPlace({
        ...activePlace,
        name: 'Updated Place'
      });
    }
  };

  const handleDeletePlace = () => {
    if (activePlace) {
      deleteTicketPlace(activePlace.id);
    }
  };

  const handleClearData = () => {
    clearAllTicketData();
  };

  return (
    <div>
      <h1>Ticket Places</h1>
      <button onClick={handleAddPlace}>Add Place</button>
      <button onClick={handleUpdatePlace}>Update Place</button>
      <button onClick={handleDeletePlace}>Delete Place</button>
      <button onClick={handleClearData}>Clear Data</button>
      
      <div data-testid="place-count">{ticketPlaceIds.length}</div>
      
      {activePlace && (
        <div data-testid="active-place-name">{activePlace.name}</div>
      )}
      
      <ul>
        {ticketPlaceIds.map(id => (
          <li key={id} data-testid={`place-${id}`}>
            {id}
          </li>
        ))}
      </ul>
    </div>
  );
};

describe('TicketContext', () => {
  test('should provide default ticket places', () => {
    render(
      <TicketProvider>
        <TestComponent />
      </TicketProvider>
    );
    
    // Check that we have 3 default ticket places
    expect(screen.getByTestId('place-count').textContent).toBe('3');
  });

  test('should add a new ticket place', async () => {
    render(
      <TicketProvider>
        <TestComponent />
      </TicketProvider>
    );
    
    const initialCount = parseInt(screen.getByTestId('place-count').textContent);
    
    // Add a new place
    fireEvent.click(screen.getByText('Add Place'));
    
    // Check that the count increased
    await waitFor(() => {
      expect(parseInt(screen.getByTestId('place-count').textContent)).toBe(initialCount + 1);
    });
  });

  test('should update a ticket place', async () => {
    render(
      <TicketProvider>
        <TestComponent />
      </TicketProvider>
    );
    
    // Update the active place
    fireEvent.click(screen.getByText('Update Place'));
    
    // Check that the name was updated
    await waitFor(() => {
      expect(screen.getByTestId('active-place-name').textContent).toBe('Updated Place');
    });
  });

  test('should delete a ticket place', async () => {
    render(
      <TicketProvider>
        <TestComponent />
      </TicketProvider>
    );
    
    const initialCount = parseInt(screen.getByTestId('place-count').textContent);
    
    // Delete the active place
    fireEvent.click(screen.getByText('Delete Place'));
    
    // Check that the count decreased
    await waitFor(() => {
      expect(parseInt(screen.getByTestId('place-count').textContent)).toBe(initialCount - 1);
    });
  });

  test('should clear all ticket data', async () => {
    render(
      <TicketProvider>
        <TestComponent />
      </TicketProvider>
    );
    
    // Clear all data
    fireEvent.click(screen.getByText('Clear Data'));
    
    // Check that we have 3 default ticket places again
    await waitFor(() => {
      expect(screen.getByTestId('place-count').textContent).toBe('3');
    });
  });
});
