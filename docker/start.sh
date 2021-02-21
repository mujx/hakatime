#!/usr/bin/env sh

set -euo pipefail

echo "HAKA_DB_HOST: ${HAKA_DB_HOST}"
echo "HAKA_DB_PORT: ${HAKA_DB_PORT}"
echo "HAKA_DB_USER: ${HAKA_DB_USER}"
echo "HAKA_DB_NAME: ${HAKA_DB_NAME}"
echo "Waiting for the PostgreSQL service to become available ..."

while ! nc -z "${HAKA_DB_HOST}" "${HAKA_DB_PORT}"; do
  sleep 1;
done;

echo "Connected to PostgreSQL"

# If the database is not created, initialze with the database schema.
if PGPASSWORD="${HAKA_DB_PASS}" psql -w -U "${HAKA_DB_USER}" -h "${HAKA_DB_HOST}" -d "${HAKA_DB_NAME}" -c "select count(*) from users" 2>&1 > /dev/null; then
  echo "Database ${HAKA_DB_NAME} already exists"
else
  echo "Database ${HAKA_DB_NAME} does not exist. Initializing database ..."
  PGPASSWORD=${HAKA_DB_PASS} psql -w -U "${HAKA_DB_USER}" -h "${HAKA_DB_HOST}" -d "${HAKA_DB_NAME}" < /app/init.sql
  echo "Database ${HAKA_DB_NAME} initialized"
fi

echo "Starting hakatime server ..."

/app/bin/hakatime run

