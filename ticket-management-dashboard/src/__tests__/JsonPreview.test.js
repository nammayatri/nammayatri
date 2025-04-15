import React from 'react';
import { render, screen, fireEvent, waitFor } from '@testing-library/react';
import JsonPreview from '../components/Common/JsonPreview';
import { TicketProvider } from '../contexts/TicketContext';

// Mock the useTicket hook
jest.mock('../contexts/TicketContext', () => {
  const originalModule = jest.requireActual('../contexts/TicketContext');
  
  return {
    ...originalModule,
    useTicket: () => ({
      jsonPreview: JSON.stringify({ id: 'test-id', name: 'Test Place' }, null, 2),
      setJsonPreview: jest.fn(),
      downloadJsonFile: jest.fn(),
      generateApiJson: jest.fn(),
      updateTicketPlace: jest.fn(),
      getActiveTicketPlace: jest.fn().mockReturnValue({ id: 'test-id', name: 'Test Place' })
    })
  };
});

describe('JsonPreview', () => {
  test('should render JSON preview', () => {
    render(<JsonPreview />);
    
    expect(screen.getByText(/Test Place/)).toBeInTheDocument();
  });

  test('should enter edit mode when Edit button is clicked', async () => {
    render(<JsonPreview />);
    
    // Click the Edit button
    fireEvent.click(screen.getByTitle('Edit JSON'));
    
    // Check that we're in edit mode
    await waitFor(() => {
      expect(screen.getByText('Save')).toBeInTheDocument();
      expect(screen.getByText('Cancel')).toBeInTheDocument();
    });
    
    // Check that the textarea contains the JSON
    const textarea = screen.getByRole('textbox');
    expect(textarea).toBeInTheDocument();
    expect(textarea.value).toContain('Test Place');
  });

  test('should exit edit mode when Cancel button is clicked', async () => {
    render(<JsonPreview />);
    
    // Enter edit mode
    fireEvent.click(screen.getByTitle('Edit JSON'));
    
    // Click the Cancel button
    fireEvent.click(screen.getByText('Cancel'));
    
    // Check that we're back in view mode
    await waitFor(() => {
      expect(screen.getByTitle('Edit JSON')).toBeInTheDocument();
      expect(screen.queryByText('Save')).not.toBeInTheDocument();
    });
  });

  test('should show error when invalid JSON is entered', async () => {
    render(<JsonPreview />);
    
    // Enter edit mode
    fireEvent.click(screen.getByTitle('Edit JSON'));
    
    // Enter invalid JSON
    const textarea = screen.getByRole('textbox');
    fireEvent.change(textarea, { target: { value: '{ invalid json }' } });
    
    // Try to save
    fireEvent.click(screen.getByText('Save'));
    
    // Check that an error is shown
    await waitFor(() => {
      expect(screen.getByText(/Invalid JSON/)).toBeInTheDocument();
    });
  });
});
