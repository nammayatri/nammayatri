import { render, screen } from '@testing-library/react';
import App from './App';

test('renders ticket management dashboard', () => {
  render(<App />);
  const titleElement = screen.getByText(/Ticket Management Dashboard/i);
  expect(titleElement).toBeInTheDocument();
});
