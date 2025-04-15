import React, { useState } from 'react';
import { Card, Button, Table, Badge } from 'react-bootstrap';
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome';
import { faEdit, faTrash, faPlus, faCheckCircle, faTimesCircle } from '@fortawesome/free-solid-svg-icons';
import { useTicket } from '../../contexts/TicketContext';
import { findEntityById } from '../../utils/helpers';
import ServiceModal from './ServiceModal';

const ServicesList = () => {
  // Component initialization

  // Get context values directly
  const { getActiveTicketPlace } = useTicket();

  // Component state
  const [showModal, setShowModal] = useState(false);
  const [currentService, setCurrentService] = useState(null);

  // Get the active place from context
  const activePlace = getActiveTicketPlace();

  const handleAddClick = () => {
    setCurrentService(null);
    setShowModal(true);
  };

  const handleEditClick = (service) => {
    setCurrentService(service);
    setShowModal(true);
  };

  const handleDeleteClick = (id) => {
    if (window.confirm('Are you sure you want to delete this service?')) {
      // TODO: Implement delete service functionality
      console.log(`Delete service with ID: ${id}`);
    }
  };

  const handleCloseModal = () => {
    setShowModal(false);
  };

  // Get business hour names
  const getBusinessHourNames = (businessHourIds) => {
    if (!activePlace || !businessHourIds) return [];

    return businessHourIds.map(id => {
      const businessHour = findEntityById(activePlace.businessHours, id);
      return businessHour ? businessHour.name : 'Unknown';
    });
  };

  // Format expiry
  const formatExpiry = (expiry) => {
    if (!expiry) return 'N/A';

    if (expiry.tag === 'VisitDate') {
      return `Visit Date (${expiry.contents})`;
    } else if (expiry.tag === 'InstantExpiry') {
      return `Instant (${expiry.contents} minutes)`;
    } else if (expiry.tag === 'FixedDateExpiry') {
      return `Fixed Date (${expiry.contents})`;
    } else if (expiry.tag === 'DaysAfterPurchaseExpiry') {
      return `Days After Purchase (${expiry.contents})`;
    }

    return 'N/A';
  };

  // For services, we don't have a direct way to check if they're used in bookings
  // So we'll consider all services as "used" for now
  // In a real application, you would check against ticket bookings
  const isServiceUsed = () => {
    return true; // All services are considered used
  };

  return (
    <>
      <Card className="mb-4">
        <Card.Header className="d-flex justify-content-between align-items-center">
          <h5 className="mb-0">Ticket Services</h5>
          <Button variant="primary" size="sm" onClick={handleAddClick}>
            <FontAwesomeIcon icon={faPlus} className="me-1" /> Add Service
          </Button>
        </Card.Header>
        <Card.Body>
          {activePlace && activePlace.services && activePlace.services.length > 0 ? (
            <Table responsive hover>
              <thead>
                <tr>
                  <th>Name</th>
                  <th>Description</th>
                  <th>Operational Days</th>
                  <th>Operational Date</th>
                  <th>Expiry</th>
                  <th>Business Hours</th>
                  <th>Actions</th>
                </tr>
              </thead>
              <tbody>
                {activePlace.services.map((service) => (
                  <tr key={service.id}>
                    <td>
                      <div className="d-flex align-items-center">
                        <FontAwesomeIcon
                          icon={isServiceUsed(service.id) ? faCheckCircle : faTimesCircle}
                          className={`me-2 ${isServiceUsed(service.id) ? 'text-success' : 'text-warning'}`}
                        />
                        <div>
                          <span className="fw-bold">{service.service}</span>
                          {!isServiceUsed(service.id) && (
                            <Badge bg="warning" className="ms-2">Unused</Badge>
                          )}
                        </div>
                        <small className="text-muted d-block">ID: {service.id}</small>
                      </div>
                    </td>
                    <td>{service.shortDesc}</td>
                    <td>
                      {service.operationalDays && service.operationalDays.length > 0 ? (
                        service.operationalDays.map((day, index) => (
                          <Badge key={index} bg="secondary" className="me-1">
                            {day}
                          </Badge>
                        ))
                      ) : (
                        <span className="text-muted">All days</span>
                      )}
                    </td>
                    <td>
                      {service.operationalDate ? (
                        <div>
                          <Badge bg="info" className="me-1">
                            {service.operationalDate.startDate}
                          </Badge>
                          {service.operationalDate.eneDate !== service.operationalDate.startDate && (
                            <>
                              <span className="mx-1">to</span>
                              <Badge bg="info">
                                {service.operationalDate.eneDate}
                              </Badge>
                            </>
                          )}
                        </div>
                      ) : (
                        <span className="text-muted">Not specified</span>
                      )}
                    </td>
                    <td>{formatExpiry(service.expiry)}</td>
                    <td>
                      {getBusinessHourNames(service.businessHours).map((name, index) => (
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
                        onClick={() => handleEditClick(service)}
                      >
                        <FontAwesomeIcon icon={faEdit} />
                      </Button>
                      <Button
                        variant="outline-danger"
                        size="sm"
                        onClick={() => handleDeleteClick(service.id)}
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
              <p className="text-muted">No services defined yet.</p>
              <Button variant="primary" onClick={handleAddClick}>
                <FontAwesomeIcon icon={faPlus} className="me-1" /> Add Service
              </Button>
            </div>
          )}
        </Card.Body>
      </Card>

      <ServiceModal
        show={showModal}
        onHide={handleCloseModal}
        service={currentService}
      />
    </>
  );
};

export default ServicesList;
