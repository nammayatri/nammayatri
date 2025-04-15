import React, { useState, useEffect } from 'react';
import { Card, Button, Table, Badge } from 'react-bootstrap';
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome';
import { faEdit, faTrash, faPlus, faCalendarAlt } from '@fortawesome/free-solid-svg-icons';
import { useTicket } from '../../contexts/TicketContext';
import { formatDate, findEntityById } from '../../utils/helpers';
import SpecialOccasionModal from './SpecialOccasionModal';
import UpcomingSpecialOccasions from './UpcomingSpecialOccasions';

const SpecialOccasionsList = () => {
  const { getActiveTicketPlace, deleteSpecialOccasion } = useTicket();
  const [showModal, setShowModal] = useState(false);
  const [currentOccasion, setCurrentOccasion] = useState(null);
  const [showUpcoming, setShowUpcoming] = useState(false);
  const [formData, setFormData] = useState({
    id: '',
    name: '',
    description: '',
    type: '',
    occurrence: '',
    appliesTo: '',
    businessHours: []
  });

  // Get the active place directly from context
  const activePlace = getActiveTicketPlace();

  const handleAddClick = () => {
    setFormData({
      id: '',
      name: '',
      description: '',
      type: '',
      occurrence: '',
      appliesTo: '',
      businessHours: []
    });
    setShowModal(true);
  };

  const handleEditClick = (occasion) => {
    setFormData({
      id: occasion.id,
      name: occasion.name,
      description: occasion.description,
      type: occasion.type,
      occurrence: occasion.occurrence,
      appliesTo: occasion.appliesTo,
      businessHours: occasion.businessHours
    });
    setShowModal(true);
  };

  const handleDeleteClick = (id) => {
    if (window.confirm('Are you sure you want to delete this special occasion?')) {
      deleteSpecialOccasion(id);
      console.log(`Deleted special occasion with ID: ${id}`);
    }
  };

  const handleCloseModal = () => {
    setShowModal(false);
  };

  const toggleUpcoming = () => {
    setShowUpcoming(!showUpcoming);
  };

  // Get entity name
  const getEntityName = (occasion) => {
    if (!activePlace) return 'N/A';

    // Handle new structure
    if (occasion.applyTo === 'service' && occasion.serviceId) {
      const service = findEntityById(activePlace.services, occasion.serviceId);
      if (service) return service.service || service.name || 'N/A';
    } else if (occasion.applyTo === 'category' && occasion.categoryId) {
      const category = findEntityById(activePlace.serviceCategories, occasion.categoryId);
      if (category) return category.name || 'N/A';
    }

    // Handle old structure
    if (occasion.entityId) {
      // Try to find in services first
      const service = findEntityById(activePlace.services, occasion.entityId);
      if (service) return service.service || service.name || 'N/A';

      // If not found in services, try categories
      const category = findEntityById(activePlace.serviceCategories, occasion.entityId);
      if (category) return category.name || 'N/A';
    }

    return 'All Services';
  };

  // Get business hours names
  const getBusinessHoursNames = (businessHourIds) => {
    if (!activePlace || !businessHourIds) return [];

    return businessHourIds.map(id => {
      const businessHour = findEntityById(activePlace.businessHours, id);
      return businessHour ? businessHour.name : 'Unknown';
    });
  };

  return (
    <>
      <Card className="mb-4">
        <Card.Header className="d-flex justify-content-between align-items-center">
          <h5 className="mb-0">Special Occasions</h5>
          <div>
            <Button
              variant="outline-info"
              size="sm"
              className="me-2"
              onClick={toggleUpcoming}
            >
              <FontAwesomeIcon icon={faCalendarAlt} className="me-1" />
              {showUpcoming ? 'Hide Upcoming' : 'Show Upcoming'}
            </Button>
            <Button variant="primary" size="sm" onClick={handleAddClick}>
              <FontAwesomeIcon icon={faPlus} className="me-1" /> Add Special Occasion
            </Button>
          </div>
        </Card.Header>
        <Card.Body>
          {showUpcoming && (
            <UpcomingSpecialOccasions />
          )}

          {activePlace && activePlace.specialOccasions && activePlace.specialOccasions.length > 0 ? (
            <Table responsive hover>
              <thead>
                <tr>
                  <th>Name</th>
                  <th>Type</th>
                  <th>Date</th>
                  <th>Applies To</th>
                  <th>Business Hours</th>
                  <th>Actions</th>
                </tr>
              </thead>
              <tbody>
                {activePlace.specialOccasions.map((occasion) => (
                  <tr key={occasion.id}>
                    <td>{occasion.name}</td>
                    <td>
                      <Badge bg={(occasion.type || occasion.specialDayType) === 'Open' ? 'success' : 'danger'}>
                        {occasion.type || occasion.specialDayType}
                      </Badge>
                    </td>
                    <td>
                      {occasion.specificDate || occasion.date ? (
                        formatDate(occasion.specificDate || occasion.date)
                      ) : (
                        <Badge bg="info">Weekly: {occasion.weeklyDay || occasion.dayOfWeek}</Badge>
                      )}
                    </td>
                    <td>
                      <Badge bg="secondary">
                        {getEntityName(occasion)}
                      </Badge>
                    </td>
                    <td>
                      {getBusinessHoursNames(occasion.businessHourIds || occasion.businessHours).map((name, index) => (
                        <Badge key={index} bg="primary" className="me-1">
                          {name}
                        </Badge>
                      ))}
                    </td>
                    <td>
                      <Button
                        variant="outline-primary"
                        size="sm"
                        className="me-2"
                        onClick={() => handleEditClick(occasion)}
                      >
                        <FontAwesomeIcon icon={faEdit} />
                      </Button>
                      <Button
                        variant="outline-danger"
                        size="sm"
                        onClick={() => handleDeleteClick(occasion.id)}
                      >
                        <FontAwesomeIcon icon={faTrash} />
                      </Button>
                    </td>
                  </tr>
                ))}
              </tbody>
            </Table>
          ) : (
            <div className="text-center py-4">
              <p className="text-muted">No special occasions defined yet.</p>
              <Button variant="primary" onClick={handleAddClick}>
                <FontAwesomeIcon icon={faPlus} className="me-1" /> Add Special Occasion
              </Button>
            </div>
          )}
        </Card.Body>
      </Card>

      <SpecialOccasionModal
        show={showModal}
        onHide={handleCloseModal}
        occasion={formData.id ? formData : null}
      />
    </>
  );
};

export default SpecialOccasionsList;
