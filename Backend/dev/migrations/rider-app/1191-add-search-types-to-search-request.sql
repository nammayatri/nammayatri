ALTER TABLE
    atlas_app.search_request
ADD
    COLUMN search_types text NOT NULL DEFAULT '[ON_DEMAND]'