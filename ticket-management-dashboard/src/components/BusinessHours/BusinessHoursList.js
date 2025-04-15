import React, { useState } from 'react';
import { Card, Button, Table, Badge } from 'react-bootstrap';
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome';
import { faEdit, faTrash, faPlus, faCheckCircle, faTimesCircle } from '@fortawesome/free-solid-svg-icons';
import { useTicket } from '../../contexts/TicketContext';
import { formatTime, findEntityById } from '../../utils/helpers';
import BusinessHoursModal from './BusinessHoursModal';

const BusinessHoursList = () => {
  // Component initialization

  // Get context values directly
  const { getActiveTicketPlace } = useTicket();

  // Component state
  const [showModal, setShowModal] = useState(false);
  const [currentBusinessHour, setCurrentBusinessHour] = useState(null);

  // Get the active place from context
  const activePlace = getActiveTicketPlace();

  const handleAddClick = () => {
    setCurrentBusinessHour(null);
    setShowModal(true);
  };

  const handleEditClick = (businessHour) => {
    setCurrentBusinessHour(businessHour);
    setShowModal(true);
  };

  const handleDeleteClick = (id) => {
    if (window.confirm('Are you sure you want to delete this business hour?')) {
      // TODO: Implement delete business hour functionality
      console.log(`Delete business hour with ID: ${id}`);
    }
  };

  const handleCloseModal = () => {
    setShowModal(false);
  };

  // Render business hour type info
  const renderTimeInfo = (businessHour) => {
    if (!businessHour.btype) return 'N/A';

    if (businessHour.btype.tag === 'Slot') {
      return formatTime(businessHour.btype.contents);
    } else if (businessHour.btype.tag === 'Duration') {
      return `${formatTime(businessHour.btype.contents[0])} - ${formatTime(businessHour.btype.contents[1])}`;
    }
    return 'N/A';
  };

  // Get service category names
  const getCategoryNames = (categoryIds) => {
    if (!activePlace || !categoryIds) return [];

    return categoryIds.map(id => {
      const category = findEntityById(activePlace.serviceCategories, id);
      return category ? {
        id: category.id,
        name: category.name
      } : { id: id, name: null };
    });
  };

  // Check if business hour is used by any service
  const isBusinessHourUsed = (businessHourId) => {
    if (!activePlace || !activePlace.services) return false;

    return activePlace.services.some(service =>
      service.businessHours && service.businessHours.includes(businessHourId)
    );
  };

  return (
    <>
      <Card className="mb-4">
        <Card.Header className="d-flex justify-content-between align-items-center">
          <h5 className="mb-0">Business Hours</h5>
          <Button variant="primary" size="sm" onClick={handleAddClick}>
            <FontAwesomeIcon icon={faPlus} className="me-1" /> Add Business Hours
          </Button>
        </Card.Header>
        <Card.Body>
          {activePlace && activePlace.businessHours && activePlace.businessHours.length > 0 ? (
            <Table responsive hover>
              <thead>
                <tr>
                  <th>Name</th>
                  <th>Type</th>
                  <th>Time</th>
                  <th>Booking Closing</th>
                  <th>Categories</th>
                  {/* Note: Days column was removed as the 'days' field doesn't exist in the backend BusinessHour type */}
                  <th>Actions</th>
                </tr>
              </thead>
              <tbody>
                {activePlace.businessHours.map((businessHour) => (
                  <tr key={businessHour.id} className={isBusinessHourUsed(businessHour.id) ? '' : 'table-secondary'}>
                    <td>
                      <div className="d-flex align-items-center">
                        <FontAwesomeIcon
                          icon={isBusinessHourUsed(businessHour.id) ? faCheckCircle : faTimesCircle}
                          className={`me-2 ${isBusinessHourUsed(businessHour.id) ? 'text-success' : 'text-warning'}`}
                        />
                        {businessHour.name ? (
                          <>
                            {businessHour.name}
                            <span className="text-muted ms-2">({businessHour.id})</span>
                          </>
                        ) : (
                          businessHour.id
                        )}
                        {!isBusinessHourUsed(businessHour.id) && (
                          <Badge bg="warning" className="ms-2">Unused</Badge>
                        )}
                      </div>
                    </td>
                    <td>
                      <Badge bg={businessHour.btype?.tag === 'Slot' ? 'primary' : 'info'}>
                        {businessHour.btype?.tag === 'Slot' ? 'Slot' : 'Duration'}
                      </Badge>
                    </td>
                    <td>{renderTimeInfo(businessHour)}</td>
                    <td>{formatTime(businessHour.bookingClosingTime)}</td>
                    <td>
                      {getCategoryNames(businessHour.categoryId).map((category, index) => (
                        <Badge key={index} bg="success" className="me-1">
                          {category.name ? (
                            <>
                              {category.name}
                              <span className="ms-1">({category.id})</span>
                            </>
                          ) : (
                            category.id
                          )}
                        </Badge>
                      ))}
                    </td>
                    {/* Note: Days cell was removed as the 'days' field doesn't exist in the backend BusinessHour type */}
                    <td>
                      <Button
                        variant="outline-primary"
                        size="sm"
                        className="me-2"
                        onClick={() => handleEditClick(businessHour)}
                      >
                        <FontAwesomeIcon icon={faEdit} />
                      </Button>
                      <Button
                        variant="outline-danger"
                        size="sm"
                        onClick={() => handleDeleteClick(businessHour.id)}
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
              <p className="text-muted">No business hours defined yet.</p>
              <Button variant="primary" onClick={handleAddClick}>
                <FontAwesomeIcon icon={faPlus} className="me-1" /> Add Business Hours
              </Button>
            </div>
          )}
        </Card.Body>
      </Card>

      <BusinessHoursModal
        show={showModal}
        onHide={handleCloseModal}
        businessHour={currentBusinessHour}
      />
    </>
  );
};

export default BusinessHoursList;
