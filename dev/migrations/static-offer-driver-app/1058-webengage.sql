CREATE TABLE atlas_transporter.webengage (
    id character(36) NOT NULL PRIMARY KEY,
    version Text,
    content_template_id character(36),
    principal_entity_id character(36),
    info_message_id character(36),
    web_message_id character(36),
    to_number character(36),
    status character(36) DEFAULT NUll

);