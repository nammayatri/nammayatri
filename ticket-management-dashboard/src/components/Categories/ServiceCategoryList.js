import React, { useState } from 'react';
import { Card, Button, Table, Badge } from 'react-bootstrap';
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome';
import { faEdit, faTrash, faPlus, faCheckCircle, faTimesCircle } from '@fortawesome/free-solid-svg-icons';
import { useTicket } from '../../contexts/TicketContext';
import { findEntityById } from '../../utils/helpers';
import ServiceCategoryModal from './ServiceCategoryModal';

const ServiceCategoryList = () => {
  // Component initialization

  // Get context values directly
  const { getActiveTicketPlace } = useTicket();

  // Component state
  const [showModal, setShowModal] = useState(false);
  const [currentCategory, setCurrentCategory] = useState(null);

  // Get the active place from context
  const activePlace = getActiveTicketPlace();

  const handleAddClick = () => {
    setCurrentCategory(null);
    setShowModal(true);
  };

  const handleEditClick = (category) => {
    setCurrentCategory(category);
    setShowModal(true);
  };

  const handleDeleteClick = (id) => {
    if (window.confirm('Are you sure you want to delete this service category?')) {
      // TODO: Implement delete service category functionality
      console.log(`Delete service category with ID: ${id}`);
    }
  };

  const handleCloseModal = () => {
    setShowModal(false);
  };

  // Get people category names
  const getPeopleCategoryNames = (peopleCategoryIds) => {
    if (!activePlace || !peopleCategoryIds) return [];

    return peopleCategoryIds.map(id => {
      const peopleCategory = findEntityById(activePlace.servicePeopleCategories, id);
      return peopleCategory ? peopleCategory.name : 'Unknown';
    });
  };

  // Check if service category is used by any business hour
  const isServiceCategoryUsed = (categoryId) => {
    if (!activePlace || !activePlace.businessHours) return false;

    return activePlace.businessHours.some(hour =>
      hour.categoryId && hour.categoryId.includes(categoryId)
    );
  };

  return (
    <>
      <Card className="mb-4">
        <Card.Header className="d-flex justify-content-between align-items-center">
          <h5 className="mb-0">Service Categories</h5>
          <Button variant="primary" size="sm" onClick={handleAddClick}>
            <FontAwesomeIcon icon={faPlus} className="me-1" /> Add Service Category
          </Button>
        </Card.Header>
        <Card.Body>
          {activePlace && activePlace.serviceCategories && activePlace.serviceCategories.length > 0 ? (
            <Table responsive hover>
              <thead>
                <tr>
                  <th>Name</th>
                  <th>Description</th>
                  <th>Available Seats</th>
                  <th>People Categories</th>
                  <th>Actions</th>
                </tr>
              </thead>
              <tbody>
                {activePlace.serviceCategories.map((category) => (
                  <tr key={category.id} className={isServiceCategoryUsed(category.id) ? '' : 'table-secondary'}>
                    <td>
                      <div className="d-flex align-items-center">
                        <FontAwesomeIcon
                          icon={isServiceCategoryUsed(category.id) ? faCheckCircle : faTimesCircle}
                          className={`me-2 ${isServiceCategoryUsed(category.id) ? 'text-success' : 'text-warning'}`}
                        />
                        {category.name ? (
                          <>
                            {category.name}
                            <span className="text-muted ms-2">({category.id})</span>
                          </>
                        ) : (
                          category.id
                        )}
                        {!isServiceCategoryUsed(category.id) && (
                          <Badge bg="warning" className="ms-2">Unused</Badge>
                        )}
                      </div>
                    </td>
                    <td>{category.description}</td>
                    <td>
                      {category.availableSeats !== null && category.availableSeats !== undefined
                        ? category.availableSeats
                        : 'Unlimited'}
                    </td>
                    <td>
                      {getPeopleCategoryNames(category.peopleCategory).map((name, index) => (
                        <Badge key={index} bg="info" className="me-1">
                          {name}
                        </Badge>
                      ))}
                    </td>
                    <td>
                      <Button
                        variant="outline-primary"
                        size="sm"
                        className="me-2"
                        onClick={() => handleEditClick(category)}
                      >
                        <FontAwesomeIcon icon={faEdit} />
                      </Button>
                      <Button
                        variant="outline-danger"
                        size="sm"
                        onClick={() => handleDeleteClick(category.id)}
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
              <p className="text-muted">No service categories defined yet.</p>
              <Button variant="primary" onClick={handleAddClick}>
                <FontAwesomeIcon icon={faPlus} className="me-1" /> Add Service Category
              </Button>
            </div>
          )}
        </Card.Body>
      </Card>

      <ServiceCategoryModal
        show={showModal}
        onHide={handleCloseModal}
        category={currentCategory}
      />
    </>
  );
};

export default ServiceCategoryList;
