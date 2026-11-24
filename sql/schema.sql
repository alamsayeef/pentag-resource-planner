-- PenTAG Resource Planner schema (SQLite). Dates are stored as ISO text: YYYY-MM-DD.
-- Statements are separated by semicolons. Do not put semicolons inside comments.

CREATE TABLE IF NOT EXISTS roles (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  name TEXT NOT NULL UNIQUE,
  system_key TEXT UNIQUE,
  is_active INTEGER NOT NULL DEFAULT 1
);

CREATE TABLE IF NOT EXISTS employees (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  name TEXT NOT NULL,
  email TEXT UNIQUE,
  fte REAL NOT NULL DEFAULT 1.0,
  is_active INTEGER NOT NULL DEFAULT 1,
  primary_role_id INTEGER REFERENCES roles(id) ON DELETE SET NULL
);

CREATE TABLE IF NOT EXISTS employee_leave (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  employee_id INTEGER NOT NULL REFERENCES employees(id) ON DELETE CASCADE,
  start_date TEXT NOT NULL,
  end_date TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS org_holidays (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  name TEXT NOT NULL,
  date TEXT NOT NULL UNIQUE,
  holiday_type TEXT NOT NULL DEFAULT 'Bank Holiday'
);

CREATE TABLE IF NOT EXISTS project_types (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  name TEXT NOT NULL UNIQUE
);

CREATE TABLE IF NOT EXISTS role_hours (
  project_type_id INTEGER NOT NULL REFERENCES project_types(id) ON DELETE CASCADE,
  role_id INTEGER NOT NULL REFERENCES roles(id) ON DELETE CASCADE,
  hours REAL NOT NULL DEFAULT 50,
  PRIMARY KEY (project_type_id, role_id)
);

CREATE TABLE IF NOT EXISTS projects (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  code TEXT UNIQUE,
  name TEXT NOT NULL,
  project_type_id INTEGER REFERENCES project_types(id) ON DELETE SET NULL,
  status TEXT NOT NULL DEFAULT 'Incoming',
  start_date TEXT NOT NULL,
  end_date TEXT NOT NULL,
  pm_id INTEGER REFERENCES employees(id) ON DELETE SET NULL,
  pd_id INTEGER REFERENCES employees(id) ON DELETE SET NULL
);

CREATE TABLE IF NOT EXISTS project_assignments (
  project_id INTEGER NOT NULL REFERENCES projects(id) ON DELETE CASCADE,
  employee_id INTEGER NOT NULL REFERENCES employees(id) ON DELETE CASCADE,
  role_id INTEGER NOT NULL REFERENCES roles(id) ON DELETE CASCADE,
  PRIMARY KEY (project_id, employee_id, role_id)
);

CREATE TABLE IF NOT EXISTS app_config (
  key TEXT PRIMARY KEY,
  value TEXT NOT NULL
)
