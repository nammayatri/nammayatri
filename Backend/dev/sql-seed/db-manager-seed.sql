DO $$
BEGIN
  IF NOT EXISTS (SELECT 1 FROM pg_roles WHERE rolname = 'db_user') THEN
    CREATE ROLE db_user;
  END IF;
END $$;

CREATE SCHEMA IF NOT EXISTS dual_db_manager;
GRANT ALL ON SCHEMA dual_db_manager TO db_user;

CREATE TABLE IF NOT EXISTS dual_db_manager.users (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    username VARCHAR(255) NOT NULL UNIQUE,
    password_hash TEXT NOT NULL,
    email VARCHAR(255) NOT NULL UNIQUE,
    name VARCHAR(255) NOT NULL,
    -- full role set through migration 004 (ADMIN added last)
    role VARCHAR(50) NOT NULL DEFAULT 'READER'
      CHECK (role IN ('MASTER', 'ADMIN', 'USER', 'READER', 'CKH_MANAGER', 'RELEASE_MANAGER')),
    is_active BOOLEAN DEFAULT false,
    picture TEXT,
    created_at TIMESTAMP NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMP NOT NULL DEFAULT NOW(),
    last_login_at TIMESTAMP
);
CREATE INDEX IF NOT EXISTS idx_users_username ON dual_db_manager.users(username);
CREATE INDEX IF NOT EXISTS idx_users_email ON dual_db_manager.users(email);
CREATE INDEX IF NOT EXISTS idx_users_active ON dual_db_manager.users(is_active) WHERE is_active = true;

CREATE TABLE IF NOT EXISTS dual_db_manager.query_history (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    user_id UUID REFERENCES dual_db_manager.users(id) ON DELETE CASCADE,
    query TEXT NOT NULL,
    database_name VARCHAR(50) NOT NULL,
    execution_mode VARCHAR(50) NOT NULL,
    cloud_results JSONB NOT NULL DEFAULT '{}',
    created_at TIMESTAMP DEFAULT NOW()
);
CREATE INDEX IF NOT EXISTS idx_query_history_user_id ON dual_db_manager.query_history(user_id);
CREATE INDEX IF NOT EXISTS idx_query_history_created_at ON dual_db_manager.query_history(created_at DESC);
CREATE INDEX IF NOT EXISTS idx_query_history_database ON dual_db_manager.query_history(database_name);

GRANT ALL ON ALL TABLES IN SCHEMA dual_db_manager TO db_user;
GRANT ALL ON ALL SEQUENCES IN SCHEMA dual_db_manager TO db_user;
ALTER DEFAULT PRIVILEGES IN SCHEMA dual_db_manager GRANT ALL ON TABLES TO db_user;
ALTER DEFAULT PRIVILEGES IN SCHEMA dual_db_manager GRANT ALL ON SEQUENCES TO db_user;

-- Dev login: admin / admin (bcrypt hash of "admin"; any valid bcrypt-of-"admin"
-- verifies, so this fixed hash is fine for a local seed).
INSERT INTO dual_db_manager.users (username, password_hash, email, name, role, is_active)
VALUES ('admin', '$2a$10$17QSYnLF5x80BZKpVJ2w8eYrKls0tG.YSdYPsCR79/UejeFP94ZL2',
        'admin@local', 'Admin', 'ADMIN', true)
ON CONFLICT (username) DO NOTHING;
