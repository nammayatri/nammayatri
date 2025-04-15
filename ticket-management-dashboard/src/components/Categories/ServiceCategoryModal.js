import React, { useState, useEffect } from 'react';
import { Modal, Button, Form, Row, Col } from 'react-bootstrap';
import { useTicket } from '../../contexts/TicketContext';

const ServiceCategoryModal = ({ show, onHide, category }) => {
  const { getActiveTicketPlace, addServiceCategory, updateServiceCategory } = useTicket();
  const activePlace = getActiveTicketPlace();
  const [formData, setFormData] = useState({
    name: '',
    description: '',
    availableSeats: '',
    allowedSeats: '',
    peopleCategory: []
  });

  // Initialize form data when editing an existing category
  useEffect(() => {
    if (category) {
      setFormData({
        name: category.name || '',
        description: category.description || '',
        availableSeats: category.availableSeats !== null && category.availableSeats !== undefined
          ? category.availableSeats.toString()
          : '',
        allowedSeats: category.allowedSeats !== null && category.allowedSeats !== undefined
          ? category.allowedSeats.toString()
          : '',
        peopleCategory: category.peopleCategory || []
      });
    } else {
      // Reset form for new category
      setFormData({
        name: '',
        description: '',
        availableSeats: '',
        allowedSeats: '',
        peopleCategory: []
      });
    }
  }, [category, show]);

  const handleChange = (e) => {
    const { name, value } = e.target;
    setFormData(prev => ({
      ...prev,
      [name]: value
    }));
  };

  const handlePeopleCategoryChange = (id) => {
    setFormData(prev => {
      const currentSelection = [...prev.peopleCategory];

      if (currentSelection.includes(id)) {
        // Remove if already selected
        return {
          ...prev,
          peopleCategory: currentSelection.filter(item => item !== id)
        };
      } else {
        // Add if not selected
        return {
          ...prev,
          peopleCategory: [...currentSelection, id]
        };
      }
    });
  };

  const handleSubmit = () => {
    // Validate form
    if (!formData.name) {
      alert('Please enter a name for the service category');
      return;
    }

    // People categories are optional

    // Prepare category data
    const categoryData = {
      name: formData.name,
      description: formData.description,
      availableSeats: formData.availableSeats ? parseInt(formData.availableSeats, 10) : null,
      allowedSeats: formData.allowedSeats ? parseInt(formData.allowedSeats, 10) : null,
      peopleCategory: formData.peopleCategory
    };

    // Save category
    if (category) {
      updateServiceCategory(category.id, categoryData);
    } else {
      addServiceCategory(categoryData);
    }

    onHide();
  };

  return (
    <Modal show={show} onHide={onHide} size="lg">
      <Modal.Header closeButton>
        <Modal.Title>
          {category ? 'Edit Service Category' : 'Add Service Category'}
        </Modal.Title>
      </Modal.Header>
      <Modal.Body>
        <Form>
          <Form.Group className="mb-3">
            <Form.Label>Name <span className="text-danger">*</span></Form.Label>
            <Form.Control
              type="text"
              name="name"
              value={formData.name}
              onChange={handleChange}
              placeholder="e.g., Regular Entry"
            />
          </Form.Group>

          <Form.Group className="mb-3">
            <Form.Label>Description</Form.Label>
            <Form.Control
              as="textarea"
              name="description"
              value={formData.description}
              onChange={handleChange}
              rows={2}
              placeholder="Enter a description of the service category"
            />
          </Form.Group>

          <Row className="mb-3">
            <Col md={6}>
              <Form.Group>
                <Form.Label>Available Seats</Form.Label>
                <Form.Control
                  type="number"
                  name="availableSeats"
                  value={formData.availableSeats}
                  onChange={handleChange}
                  min="0"
                  placeholder="Leave empty for unlimited"
                />
                <Form.Text className="text-muted">
                  Total number of available seats (leave empty for unlimited)
                </Form.Text>
              </Form.Group>
            </Col>
            <Col md={6}>
              <Form.Group>
                <Form.Label>Allowed Seats</Form.Label>
                <Form.Control
                  type="number"
                  name="allowedSeats"
                  value={formData.allowedSeats}
                  onChange={handleChange}
                  min="0"
                  placeholder="Leave empty for unlimited"
                />
                <Form.Text className="text-muted">
                  Maximum number of allowed seats (leave empty for unlimited)
                </Form.Text>
              </Form.Group>
            </Col>
          </Row>

          <Form.Group className="mb-3">
            <Form.Label>People Categories</Form.Label>
            <div className="border rounded p-3" style={{ maxHeight: '200px', overflowY: 'auto' }}>
              {activePlace && activePlace.servicePeopleCategories && activePlace.servicePeopleCategories.length > 0 ? (
                activePlace.servicePeopleCategories.map(category => (
                  <Form.Check
                    key={category.id}
                    type="checkbox"
                    id={category.id}
                    label={
                      <>
                        {category.name ? (
                          <>
                            {category.name}
                            <span className="text-muted ms-2">({category.id})</span>
                          </>
                        ) : (
                          category.id
                        )}
                        {category.priceAmount && (
                          <span className="text-muted ms-2">- {category.priceAmount} {category.priceCurrency}</span>
                        )}
                      </>
                    }
                    checked={formData.peopleCategory.includes(category.id)}
                    onChange={() => handlePeopleCategoryChange(category.id)}
                    className="mb-2"
                  />
                ))
              ) : (
                <p className="text-muted mb-0">No people categories available. Please create some first.</p>
              )}
            </div>
            {activePlace && activePlace.servicePeopleCategories && activePlace.servicePeopleCategories.length > 0 && (
              <div className="d-flex justify-content-between mt-2">
                <Form.Text className="text-muted">
                  Select all applicable people categories for this service category
                </Form.Text>
                <Button
                  variant="link"
                  size="sm"
                  onClick={() => {
                    // Toggle all: If all are selected, deselect all. Otherwise, select all.
                    const allIds = activePlace.servicePeopleCategories.map(pc => pc.id);
                    const allSelected = allIds.every(id => formData.peopleCategory.includes(id));

                    setFormData(prev => ({
                      ...prev,
                      peopleCategory: allSelected ? [] : allIds
                    }));
                  }}
                >
                  {formData.peopleCategory.length === activePlace.servicePeopleCategories.length ? 'Deselect All' : 'Select All'}
                </Button>
              </div>
            )}
          </Form.Group>
        </Form>
      </Modal.Body>
      <Modal.Footer>
        <Button variant="secondary" onClick={onHide}>
          Cancel
        </Button>
        <Button variant="primary" onClick={handleSubmit}>
          {category ? 'Update' : 'Add'}
        </Button>
      </Modal.Footer>
    </Modal>
  );
};

export default ServiceCategoryModal;
