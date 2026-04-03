import React from 'react';
import { render, screen } from '@testing-library/react';
import App from './App';

test('renders dashboard with flow selector', () => {
  render(<App />);
  const flowElement = screen.getByText(/Ride Flow/i);
  expect(flowElement).toBeInTheDocument();
});

test('renders config section', () => {
  render(<App />);
  const configElement = screen.getByText(/Loading test context from DB/i);
  expect(configElement).toBeInTheDocument();
});

test('renders driver setup section', () => {
  render(<App />);
  const driverSetupElement = screen.getByText(/Driver Setup/i);
  expect(driverSetupElement).toBeInTheDocument();
});
