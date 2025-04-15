import React, { useMemo } from 'react';
import { Alert, Table, Badge } from 'react-bootstrap';
import { useTicket } from '../../contexts/TicketContext';
import { formatDate } from '../../utils/helpers';

const UpcomingSpecialOccasions = () => {
  const { getActiveTicketPlace } = useTicket();
  const activePlace = getActiveTicketPlace();

  // Get upcoming special occasions for the next 30 days
  const upcomingOccasions = useMemo(() => {
    if (!activePlace || !activePlace.specialOccasions) return [];

    const today = new Date();
    const thirtyDaysLater = new Date();
    thirtyDaysLater.setDate(today.getDate() + 30);

    const upcoming = [];

    // Process each special occasion
    activePlace.specialOccasions.forEach(occasion => {
      if (occasion.specificDate) {
        // Handle specific date occasions
        const occasionDate = new Date(occasion.specificDate);
        if (occasionDate >= today && occasionDate <= thirtyDaysLater) {
          upcoming.push({
            ...occasion,
            occurrenceDate: occasionDate
          });
        }
      } else if (occasion.weeklyDay) {
        // Handle weekly occasions
        const dayIndex = ['Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday']
          .findIndex(day => day === occasion.weeklyDay);

        if (dayIndex === -1) return;

        // Find all occurrences of this day in the next 30 days
        const date = new Date(today);

        // Move to the next occurrence of the day
        while (date.getDay() !== dayIndex) {
          date.setDate(date.getDate() + 1);
        }

        // Add all occurrences within the 30-day window
        while (date <= thirtyDaysLater) {
          upcoming.push({
            ...occasion,
            occurrenceDate: new Date(date)
          });

          // Move to next week
          date.setDate(date.getDate() + 7);
        }
      }
    });

    // Sort by date
    return upcoming.sort((a, b) => a.occurrenceDate - b.occurrenceDate);
  }, [activePlace]);

  // Get service or category name
  const getApplyToName = (occasion) => {
    if (!activePlace) return 'N/A';

    if (occasion.applyTo === 'service' && occasion.serviceId) {
      const service = activePlace.services.find(s => s.id === occasion.serviceId);
      return service ? (service.service || service.name) : 'N/A';
    } else if (occasion.applyTo === 'category' && occasion.categoryId) {
      const category = activePlace.serviceCategories.find(c => c.id === occasion.categoryId);
      return category ? category.name : 'N/A';
    }

    return 'All Services';
  };

  if (upcomingOccasions.length === 0) {
    return (
      <Alert variant="info" className="mb-4">
        No upcoming special occasions in the next 30 days.
      </Alert>
    );
  }

  return (
    <div className="mb-4">
      <h6 className="mb-3">Upcoming Special Occasions (Next 30 Days)</h6>
      <Table responsive hover size="sm" className="border">
        <thead className="table-light">
          <tr>
            <th>Date</th>
            <th>Name</th>
            <th>Type</th>
            <th>Applies To</th>
          </tr>
        </thead>
        <tbody>
          {upcomingOccasions.map((occasion, index) => (
            <tr key={`${occasion.id}-${index}`}>
              <td>{formatDate(occasion.occurrenceDate.toISOString().split('T')[0])}</td>
              <td>{occasion.name}</td>
              <td>
                <Badge bg={occasion.type === 'Open' ? 'success' : 'danger'}>
                  {occasion.type}
                </Badge>
              </td>
              <td>
                <Badge bg="secondary">
                  {getApplyToName(occasion)}
                </Badge>
              </td>
            </tr>
          ))}
        </tbody>
      </Table>
    </div>
  );
};

export default UpcomingSpecialOccasions;
