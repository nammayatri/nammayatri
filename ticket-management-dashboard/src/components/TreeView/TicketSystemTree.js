import React, { useState, useEffect } from 'react';
import { Button, Spinner } from 'react-bootstrap';
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome';
import {
  faChevronRight,
  faChevronDown,
  faMapMarkerAlt,
  faTicketAlt,
  faTag,
  faUsers,
  faClock,
  faCalendarAlt
} from '@fortawesome/free-solid-svg-icons';
import { useTicket } from '../../contexts/TicketContext';
import { findEntityById } from '../../utils/helpers';
import './TreeView.css';

const TicketSystemTree = ({ onNodeSelect }) => {
  // Component initialization

  // Get context values directly
  const { getActiveTicketPlace, isLoading } = useTicket();

  // Get the active place from context
  const activePlace = getActiveTicketPlace();

  // Set up expanded nodes state
  const [expandedNodes, setExpandedNodes] = useState({
    place: true,
    services: {},
    categories: {},
    businessHours: {}
  });

  // Initialize expanded nodes when active place changes
  useEffect(() => {
    if (activePlace) {
      // Auto-expand all business hours by default
      const expandedHours = {};
      if (activePlace.businessHours && Array.isArray(activePlace.businessHours)) {
        activePlace.businessHours.forEach(hour => {
          if (hour && hour.id) {
            expandedHours[hour.id] = true;
          }
        });
      }

      // Auto-expand all services by default
      const expandedServices = {};
      if (activePlace.services && Array.isArray(activePlace.services)) {
        activePlace.services.forEach(service => {
          if (service && service.id) {
            expandedServices[service.id] = true;
          }
        });
      }

      // Auto-expand all categories by default
      const expandedCategories = {};
      if (activePlace.serviceCategories && Array.isArray(activePlace.serviceCategories)) {
        activePlace.serviceCategories.forEach(category => {
          if (category && category.id) {
            expandedCategories[category.id] = true;
          }
        });
      }

      setExpandedNodes(prev => ({
        ...prev,
        services: expandedServices,
        businessHours: expandedHours,
        categories: expandedCategories
      }));
    }
  }, [activePlace]);
  const [selectedNode, setSelectedNode] = useState(null);

  // Debug logging
  useEffect(() => {
    console.log('Active Ticket Place:', activePlace);
  }, [activePlace]);

  // Handle node expansion toggle
  const toggleNode = (nodeType, nodeId) => {
    console.log(`Toggling node: ${nodeType} with ID: ${nodeId}`);

    setExpandedNodes(prev => {
      let newState;

      if (nodeType === 'place') {
        newState = { ...prev, place: !prev.place };
      } else if (nodeType === 'service') {
        newState = {
          ...prev,
          services: {
            ...prev.services,
            [nodeId]: !prev.services[nodeId]
          }
        };
      } else if (nodeType === 'category') {
        // For categories, ensure we have a value to toggle
        const currentValue = prev.categories[nodeId] !== undefined ? prev.categories[nodeId] : true;
        newState = {
          ...prev,
          categories: {
            ...prev.categories,
            [nodeId]: !currentValue
          }
        };
        console.log(`Category ${nodeId} expanded state changing from ${currentValue} to ${!currentValue}`);
      } else if (nodeType === 'businessHour') {
        newState = {
          ...prev,
          businessHours: {
            ...prev.businessHours,
            [nodeId]: !prev.businessHours[nodeId]
          }
        };
      } else {
        newState = prev;
      }

      console.log('New expanded nodes state:', newState);
      return newState;
    });
  };

  // Handle node selection
  const handleNodeSelect = (nodeType, node) => {
    setSelectedNode({ type: nodeType, data: node });
    if (onNodeSelect) {
      onNodeSelect(nodeType, node);
    }
  };

  // Get icon for node type
  const getNodeIcon = (nodeType) => {
    switch (nodeType) {
      case 'place':
        return faMapMarkerAlt;
      case 'service':
        return faTicketAlt;
      case 'category':
        return faTag;
      case 'peopleCategory':
        return faUsers;
      case 'businessHour':
        return faClock;
      case 'specialOccasion':
        return faCalendarAlt;
      default:
        return faChevronRight;
    }
  };

  // Render tree node
  const renderTreeNode = (nodeType, node, level = 0, parentExpanded = true) => {
    const isExpanded =
      nodeType === 'place'
        ? expandedNodes.place
        : nodeType === 'service'
          ? expandedNodes.services[node.id]
          : nodeType === 'category'
            ? expandedNodes.categories[node.id]
            : nodeType === 'businessHour'
              ? expandedNodes.businessHours[node.id]
              : false;

    // Determine if node has children
    let hasChildren = false;

    if (nodeType === 'place') {
      hasChildren = activePlace.services && Array.isArray(activePlace.services) && activePlace.services.length > 0;
    }
    else if (nodeType === 'service') {
      const hasCategories = activePlace.serviceCategories && Array.isArray(activePlace.serviceCategories) &&
        activePlace.serviceCategories.filter(c => c && getServiceForCategory(c) === node.id).length > 0;

      const hasBusinessHours = activePlace.businessHours && Array.isArray(activePlace.businessHours) &&
        node.businessHours && Array.isArray(node.businessHours) &&
        activePlace.businessHours.filter(bh => bh && node.businessHours.some(id => id === bh.id)).length > 0;

      hasChildren = hasCategories || hasBusinessHours;
    }
    else if (nodeType === 'category') {
      hasChildren = activePlace.servicePeopleCategories && Array.isArray(activePlace.servicePeopleCategories) &&
        node.peopleCategory && Array.isArray(node.peopleCategory) && node.peopleCategory.length > 0;

      // Debug log for category children
      if (node.name === 'Regular Entry') {
        console.log('Regular Entry category check:', {
          id: node.id,
          peopleCategory: node.peopleCategory,
          hasChildren,
          isExpanded: expandedNodes.categories[node.id]
        });
      }
    }

    const isSelected = selectedNode && selectedNode.type === nodeType &&
      selectedNode.data && selectedNode.data.id === node.id;

    if (!parentExpanded) return null;

    // Add specific class based on node type
    const nodeTypeClass = `${nodeType}-node`;

    return (
      <div key={node.id || 'place'} className={`tree-node ${nodeTypeClass}`} style={{ marginLeft: `${level * 20}px` }}>
        <div
          className={`tree-node-content ${isSelected ? 'selected' : ''}`}
          onClick={() => handleNodeSelect(nodeType, node)}
        >
          {hasChildren && (
            <Button
              variant="link"
              className="toggle-btn"
              onClick={(e) => {
                e.stopPropagation();
                toggleNode(nodeType, node.id);
              }}
              aria-label={isExpanded ? 'Collapse node' : 'Expand node'}
            >
              <FontAwesomeIcon icon={isExpanded ? faChevronDown : faChevronRight} size="sm" />
            </Button>
          )}
          <span className="node-icon">
            <FontAwesomeIcon icon={getNodeIcon(nodeType)} />
          </span>
          <span className="node-label">
            {getNodeLabel(nodeType, node)}
            {nodeType === 'category' && node.availableSeats &&
              <span className="node-details"> (Seats: {node.availableSeats})</span>
            }
          </span>
        </div>

        {/* Render children if expanded */}
        {isExpanded && hasChildren && (
          <div className="tree-node-children">
            {nodeType === 'place' && renderServices(node, level + 1)}
            {nodeType === 'service' && (
              <>
                {renderBusinessHoursForService(node, level + 1)}
              </>
            )}
            {nodeType === 'category' && renderPeopleCategoriesForCategory(node, level + 1)}
          </div>
        )}
      </div>
    );
  };

  // Get appropriate label for node
  const getNodeLabel = (nodeType, node) => {
    switch (nodeType) {
      case 'place':
        return node.name;
      case 'service':
        return node.service;
      case 'category':
        // If the category has a businessHourName property, show it
        return node.businessHourName ?
          `${node.name} (${node.businessHourName})` :
          node.name;
      case 'peopleCategory':
        return `${node.name} - ${node.priceAmount} ${node.priceCurrency}`;
      case 'businessHour':
        return node.name;
      default:
        return 'Unknown';
    }
  };

  // Helper to find service ID for a category
  const getServiceForCategory = (category) => {
    // In our current data model, we need to infer this from business hours
    if (!activePlace || !activePlace.businessHours || !category) return null;

    // Find business hours that include this category
    const relevantHours = activePlace.businessHours.filter(
      bh => bh.categoryId && Array.isArray(bh.categoryId) && bh.categoryId.some(catId =>
        catId === category.id || findEntityById([category], catId) !== undefined
      )
    );

    if (relevantHours.length === 0) return null;

    // Find services that use these business hours
    if (Array.isArray(activePlace.services)) {
      for (const service of activePlace.services) {
        if (service.businessHours && Array.isArray(service.businessHours)) {
          for (const hourId of service.businessHours) {
            if (relevantHours.some(h => h.id === hourId || findEntityById(relevantHours, hourId) !== undefined)) {
              return service.id;
            }
          }
        }
      }
    }

    return null;
  };

  // Render services
  const renderServices = (place, level) => {
    if (!activePlace || !activePlace.services || !Array.isArray(activePlace.services)) return null;

    return activePlace.services.map(service =>
      renderTreeNode('service', service, level, true)
    );
  };

  // Render business hours for a service
  const renderBusinessHoursForService = (service, level) => {
    if (!service.businessHours || !Array.isArray(service.businessHours) ||
        !activePlace || !activePlace.businessHours || !Array.isArray(activePlace.businessHours)) {
      return null;
    }

    const businessHours = activePlace.businessHours.filter(bh => {
      if (!bh || !bh.id) return false;
      return service.businessHours.some(hourId =>
        hourId === bh.id || findEntityById([bh], hourId) !== undefined
      );
    });

    if (businessHours.length === 0) return null;

    return (
      <div key={`${service.id}-hours`} className="tree-node" style={{ marginLeft: `${level * 20}px` }}>
        <div className="tree-node-content">
          <span className="node-icon">
            <FontAwesomeIcon icon={faClock} />
          </span>
          <span className="node-label">Business Hours</span>
        </div>
        <div className="tree-node-children">
          {businessHours.map(hour =>
            renderBusinessHourWithCategories(hour, level + 1)
          )}
        </div>
      </div>
    );
  };

  // Render a business hour with its categories
  const renderBusinessHourWithCategories = (businessHour, level) => {
    const isExpanded = expandedNodes.businessHours[businessHour.id];
    const hasCategories = businessHour.categoryId && Array.isArray(businessHour.categoryId) && businessHour.categoryId.length > 0;

    // Get business hour type info for display
    const getTimeInfo = (bh) => {
      if (!bh.btype) return '';

      if (bh.btype.tag === 'Slot') {
        return ` (${bh.btype.contents})`;
      } else if (bh.btype.tag === 'Duration') {
        return ` (${bh.btype.contents[0]} - ${bh.btype.contents[1]})`;
      }
      return '';
    };

    return (
      <div key={businessHour.id} className="tree-node business-hour-node" style={{ marginLeft: `${level * 20}px` }}>
        <div
          className={`tree-node-content ${selectedNode && selectedNode.type === 'businessHour' && selectedNode.data.id === businessHour.id ? 'selected' : ''}`}
          onClick={() => handleNodeSelect('businessHour', businessHour)}
        >
          {hasCategories && (
            <Button
              variant="link"
              className="toggle-btn"
              onClick={(e) => {
                e.stopPropagation();
                toggleNode('businessHour', businessHour.id);
              }}
              aria-label={isExpanded ? 'Collapse business hour' : 'Expand business hour'}
            >
              <FontAwesomeIcon icon={isExpanded ? faChevronDown : faChevronRight} size="sm" />
            </Button>
          )}
          <span className="node-icon">
            <FontAwesomeIcon icon={faClock} />
          </span>
          <span className="node-label">
            {businessHour.name}{getTimeInfo(businessHour)}
          </span>
        </div>

        {isExpanded && hasCategories && (
          <div className="tree-node-children business-hour-children">
            {renderCategoriesForBusinessHour(businessHour, level + 1)}
          </div>
        )}
      </div>
    );
  };

  // Render categories for a business hour
  const renderCategoriesForBusinessHour = (businessHour, level) => {
    if (!activePlace || !activePlace.serviceCategories || !Array.isArray(activePlace.serviceCategories) || !businessHour || !businessHour.categoryId) {
      console.log('Cannot render categories: missing data', { businessHour });
      return null;
    }

    console.log('Rendering categories for business hour:', businessHour.name, businessHour.id, businessHour.categoryId);

    // Find categories that belong to this business hour
    const businessHourCategories = activePlace.serviceCategories.filter(category => {
      if (!category) return false;
      const matches = businessHour.categoryId.some(catId =>
        catId === category.id || findEntityById([category], catId) !== undefined
      );
      console.log('Category match check:', category.name, category.id, matches);
      return matches;
    });

    if (businessHourCategories.length === 0) return null;

    // Add a wrapper div with a class to indicate these categories belong to this business hour
    return (
      <div className="categories-for-business-hour">
        {businessHourCategories.map(category => {
          // Add a label to show which business hour this category belongs to
          const categoryWithLabel = {...category, businessHourName: businessHour.name};
          return renderTreeNode('category', categoryWithLabel, level, true);
        })}
      </div>
    );
  };

  // Render people categories for a category
  const renderPeopleCategoriesForCategory = (category, level) => {
    if (!category || !category.peopleCategory || !Array.isArray(category.peopleCategory) ||
        !activePlace || !activePlace.servicePeopleCategories || !Array.isArray(activePlace.servicePeopleCategories)) {
      console.log('Cannot render people categories: missing data', { category });
      return null;
    }

    console.log('Rendering people categories for category:', category.name, category.id, category.peopleCategory);

    const peopleCategories = activePlace.servicePeopleCategories.filter(pc => {
      if (!pc || !pc.id) return false;
      const matches = category.peopleCategory.some(peopleId =>
        peopleId === pc.id || findEntityById([pc], peopleId) !== undefined
      );
      console.log('People category match check:', pc.name, pc.id, matches);
      return matches;
    });

    if (peopleCategories.length === 0) return null;

    // Render people categories horizontally in a flex container
    return (
      <div className="people-categories-container">
        <div className="people-categories-label">
          <span className="node-icon">
            <FontAwesomeIcon icon={faUsers} />
          </span>
          <span>People Categories:</span>
        </div>
        <div className="people-categories-row">
          {peopleCategories.map(peopleCategory => (
            <div
              key={peopleCategory.id}
              className={`people-category-card ${selectedNode?.type === 'peopleCategory' && selectedNode?.data?.id === peopleCategory.id ? 'selected' : ''}`}
              onClick={() => handleNodeSelect('peopleCategory', peopleCategory)}
            >
              <div className="people-category-icon">
                <FontAwesomeIcon icon={faUsers} />
              </div>
              <div className="people-category-name">{peopleCategory.name}</div>
              <div className="people-category-price">
                {peopleCategory.priceAmount} {peopleCategory.priceCurrency}
              </div>
            </div>
          ))}
        </div>
      </div>
    );
  };

  // activePlace is already defined at the top of the component

  // If no data is available, show a message
  if (!activePlace) {
    return (
      <div className="ticket-system-tree">
        <div className="text-center p-5">
          <p className="text-muted">No ticket data available. Please add a ticket place to get started.</p>
        </div>
      </div>
    );
  }

  // Show loading indicator if data is loading
  if (isLoading) {
    return (
      <div className="text-center p-5">
        <Spinner animation="border" role="status" variant="primary">
          <span className="visually-hidden">Loading...</span>
        </Spinner>
        <p className="mt-3">Loading ticket place data...</p>
      </div>
    );
  }

  // Log the active place for debugging
  console.log('Active place in TicketSystemTree:', activePlace);

  // Show message if no active place is selected
  if (!activePlace) {
    return (
      <div className="alert alert-info">
        <p className="mb-0">No ticket place selected. Please select a ticket place from the list.</p>
      </div>
    );
  }

  // Log the structure for debugging
  console.log('Rendering tree with place:', activePlace);

  // Create a safe copy of the active place with default values for missing properties
  const safePlace = {
    ...activePlace,
    id: activePlace.id || 'unknown',
    name: activePlace.name || 'Unnamed Place',
    description: activePlace.description || '',
    services: activePlace.services || [],
    serviceCategories: activePlace.serviceCategories || [],
    servicePeopleCategories: activePlace.servicePeopleCategories || [],
    businessHours: activePlace.businessHours || [],
    specialOccasions: activePlace.specialOccasions || []
  };

  // Check if required properties exist
  const services = safePlace.services;
  const serviceCategories = safePlace.serviceCategories;
  const servicePeopleCategories = safePlace.servicePeopleCategories;
  const businessHours = safePlace.businessHours;

  console.log('Services:', services);
  console.log('Service Categories:', serviceCategories);
  console.log('People Categories:', servicePeopleCategories);
  console.log('Business Hours:', businessHours);

  try {
    return (
      <div className="ticket-system-tree">
        {renderTreeNode('place', safePlace)}
      </div>
    );
  } catch (error) {
    console.error('Error rendering tree:', error);
    return (
      <div className="alert alert-danger">
        <h4>Error Rendering Tree</h4>
        <p>{error.message}</p>
        <p>Please try refreshing the data or go back to the list view.</p>
      </div>
    );
  }
};

export default TicketSystemTree;
