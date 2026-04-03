import React from 'react';
import { render, screen } from '@testing-library/react';
import App from './App';

test('renders dashboard header', () => {
  render(<App />);
  const headerElement = screen.getByText(/NammaYatri Test Dashboard/i);
  expect(headerElement).toBeInTheDocument();
});

test('renders config section', () => {
  render(<App />);
  const configElement = screen.getByText(/Config/i);
  expect(configElement).toBeInTheDocument();
});
