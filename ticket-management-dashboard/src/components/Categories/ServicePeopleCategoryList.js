import React, { useState, useEffect } from 'react';
import { Card, Button, Table, Badge } from 'react-bootstrap';
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome';
import { faEdit, faTrash, faPlus, faCheckCircle, faTimesCircle } from '@fortawesome/free-solid-svg-icons';
import { useTicket } from '../../contexts/TicketContext';
import ServicePeopleCategoryModal from './ServicePeopleCategoryModal';

const ServicePeopleCategoryList = () => {
  const { getActiveTicketPlace } = useTicket();
  const [showModal, setShowModal] = useState(false);
  const [currentPeopleCategory, setCurrentPeopleCategory] = useState(null);

  // Get the active place directly from context
  const activePlace = getActiveTicketPlace();

  const handleAddClick = () => {
    setCurrentPeopleCategory(null);
    setShowModal(true);
  };

  const handleEditClick = (peopleCategory) => {
    setCurrentPeopleCategory(peopleCategory);
    setShowModal(true);
  };

  const handleDeleteClick = (id) => {
    if (window.confirm('Are you sure you want to delete this people category?')) {
      // TODO: Implement delete service people category functionality
      console.log(`Delete service people category with ID: ${id}`);
    }
  };

  const handleCloseModal = () => {
    setShowModal(false);
  };

  // Format price
  const formatPrice = (amount, currency) => {
    if (amount === undefined || amount === null) return 'N/A';
    return `${amount} ${currency}`;
  };

  // Check if people category is used by any service category
  const isPeopleCategoryUsed = (peopleCategoryId) => {
    if (!activePlace || !activePlace.serviceCategories) return false;

    return activePlace.serviceCategories.some(category =>
      category.peopleCategory && category.peopleCategory.includes(peopleCategoryId)
    );
  };

  return (
    <>
      <Card className="mb-4">
        <Card.Header className="d-flex justify-content-between align-items-center">
          <h5 className="mb-0">People Categories</h5>
          <Button variant="primary" size="sm" onClick={handleAddClick}>
            <FontAwesomeIcon icon={faPlus} className="me-1" /> Add People Category
          </Button>
        </Card.Header>
        <Card.Body>
          {activePlace && activePlace.servicePeopleCategories && activePlace.servicePeopleCategories.length > 0 ? (
            <Table responsive hover>
              <thead>
                <tr>
                  <th>Name</th>
                  <th>Description</th>
                  <th>Price Per Unit</th>
                  <th>Pricing Type</th>
                  <th>Actions</th>
                </tr>
              </thead>
              <tbody>
                {activePlace.servicePeopleCategories.map((peopleCategory) => (
                  <tr key={peopleCategory.id} className={isPeopleCategoryUsed(peopleCategory.id) ? '' : 'table-secondary'}>
                    <td>
                      <div className="d-flex align-items-center">
                        <FontAwesomeIcon
                          icon={isPeopleCategoryUsed(peopleCategory.id) ? faCheckCircle : faTimesCircle}
                          className={`me-2 ${isPeopleCategoryUsed(peopleCategory.id) ? 'text-success' : 'text-warning'}`}
                        />
                        {peopleCategory.name ? (
                          <>
                            {peopleCategory.name}
                            <span className="text-muted ms-2">({peopleCategory.id})</span>
                          </>
                        ) : (
                          peopleCategory.id
                        )}
                        {!isPeopleCategoryUsed(peopleCategory.id) && (
                          <Badge bg="warning" className="ms-2">Unused</Badge>
                        )}
                      </div>
                    </td>
                    <td>{peopleCategory.description}</td>
                    <td>{formatPrice(peopleCategory.priceAmount, peopleCategory.priceCurrency)}</td>
                    <td>
                      <Badge bg={peopleCategory.pricingType === 'AllDays' ? 'success' : 'info'}>
                        {peopleCategory.pricingType}
                      </Badge>
                    </td>
                    <td>
                      <Button
                        variant="outline-primary"
                        size="sm"
                        className="me-2"
                        onClick={() => handleEditClick(peopleCategory)}
                      >
                        <FontAwesomeIcon icon={faEdit} />
                      </Button>
                      <Button
                        variant="outline-danger"
                        size="sm"
                        onClick={() => handleDeleteClick(peopleCategory.id)}
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
              <p className="text-muted">No people categories defined yet.</p>
              <Button variant="primary" onClick={handleAddClick}>
                <FontAwesomeIcon icon={faPlus} className="me-1" /> Add People Category
              </Button>
            </div>
          )}
        </Card.Body>
      </Card>

      <ServicePeopleCategoryModal
        show={showModal}
        onHide={handleCloseModal}
        peopleCategory={currentPeopleCategory}
      />
    </>
  );
};

export default ServicePeopleCategoryList;
