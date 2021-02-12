# hakatime

[![CircleCI](https://circleci.com/gh/mujx/hakatime.svg?style=svg)](https://circleci.com/gh/mujx/hakatime)
[![Docker build](https://img.shields.io/docker/cloud/build/mujx/hakatime)](https://hub.docker.com/r/mujx/hakatime/builds)
[![BuiltWithNix](https://img.shields.io/badge/Built_With-Nix-5277C3.svg?logo=nixos&labelColor=73C3D5)](https://nixos.org/)
[![License: Unlicense](https://img.shields.io/badge/license-Unlicense-blue.svg)](http://unlicense.org/)

Hakatime is a server implementation of [Wakatime](https://wakatime.com/). It provides a single API endpoint
(`/api/v1/users/current/heartbeats.bulk`) that the Wakatime client can use to send heartbeats containing info
about your coding activity.

It comes together with a simple dashboard which provides a graphical representation of the collected data.

## Features

- Total activity view.
- Per project activity view.
- User registration & login.
- Import data from Wakatime.
- Badge generation for a project (using shields.io compatible endpoint). [![my-app](https://hakatime-demo.mtx-dev.xyz/badge/svg/f8c0f834-3747-4d3f-965b-6fa08c6dea94?days=7)](https://hakatime-demo.mtx-dev.xyz/badge/svg/f8c0f834-3747-4d3f-965b-6fa08c6dea94?days=7)
- API token management.

## Demo

There is demo instance with fake data [here](https://hakatime-demo.mtx-dev.xyz).

Use `demo` as username and `demodemo` as password to login. You can create an API token and try sending your
data.

## Client setup

After you've generated a new API token from Hakatime's UI, update your `~/.wakatime.cfg` file like below:

```ini
[settings]
api_key=<generated_token>

# e.g api_url=https://hakatime-demo.mtx-dev.xyz/api/v1/users/current/heartbeats.bulk for the demo instance.
api_url=<hakatime_instance_url>/api/v1/users/current/heartbeats.bulk
```

The file's location might be different depending on your system and editor. Please consult the [client docs](https://wakatime.com/plugins)
for further information.

## Deployment

You can use the following docker-compose setup for testing locally or an actual
deployment. Change `HAKA_BADGE_URL` to match the actual external endpoint of your
instance.

Deploying on ARM is also possible using `mujx/hakatime:latest-arm64` for the `arm64` architecture
(builds on every commit) or with `mujx/hakatime:latest-arm` (manual semi-regular updates for `arm/v7` & `arm64`)
or by building the image yourself with the dedicated Dockerfile (`Dockerfile.arm`).

:warning: This setup relies on `./docker` directory located in this repository, please run it after making `git clone` to evade issues.

```yaml
version: "3"
services:
  server:
    container_name: hakatime
    image: mujx/hakatime:latest
    environment:
      # DB settings.
      HAKA_DB_HOST: haka_db
      HAKA_DB_PORT: 5432
      HAKA_DB_NAME: test
      HAKA_DB_PASS: test
      HAKA_DB_USER: test
      # Server settings.
      # Fill out this field if the api is behind another path (e.g behind a reverse proxy).
      # This will adjust the Set-Cookie path for all the /auth related API calls.
      HAKA_API_PREFIX: ""
      HAKA_BADGE_URL: "http://localhost:8080"
      HAKA_PORT: 8080
      HAKA_SHIELDS_IO_URL: "https://img.shields.io"
      HAKA_ENABLE_REGISTRATION: "true" # Toggle after you've created your account.
      # Number of hours after which inactive browser sessions will expire (login required).
      HAKA_SESSION_EXPIRY: "24"
      HAKA_LOG_LEVEL: "info" # Control the verbosity of the logger.
      HAKA_ENV: "prod" # Use a json logger for production, otherwise key=value pairs.
      HAKA_HTTP_LOG: "true" # If you want to log http requests.
    ports:
      - "127.0.0.1:8080:8080"
  haka_db:
    container_name: haka_db
    image: postgres:11-alpine
    environment:
      POSTGRES_DB: test
      POSTGRES_PASSWORD: test
      POSTGRES_USER: test
    volumes:
      - ./docker/:/docker-entrypoint-initdb.d/
      - deploy_db_data:/var/lib/postgresql/data

volumes:
  deploy_db_data: {}
```

To start all the services run:

```bash
$ docker-compose -f ./docker-compose-deploy.yml up
```

and navigate to [http://localhost:8080](http://localhost:8080) to access the UI.

## Building

### Server

Requirements:

- [GHC](https://www.haskell.org/ghc/) (tested with 8.8)
- [libpq](https://www.postgresql.org/docs/11/libpq.html) (for PostgreSQL bindings)
- [cabal-install](https://www.haskell.org/cabal/) (If building with cabal)

Using [nix](https://nixos.org/nix/) requires the least amount of manual intervention (installing packages etc)

#### nix

```bash
nix-build release.nix
```

#### cabal

```bash
cabal build
```

### Dashboard

Requirements:

- Node.js
- npm / yarn

```bash
cd dashboard

npm install # yarn install

npm run prod # Optimized build for production usage.

npm run dev # Development server with hot reloading.
```

## Running

### Database

The server needs a database to store its data, so we will have to create a PostgreSQL instance and initialize
it with the schema found in the [docker/](docker/) directory. You can use the provided solution using
docker-compose (`docker-compose up -d`) or do it manually, depending on your system.

### Server

Start the server and point it to the database.

```bash
# We assume that the docker-compose setup is used.
# Change these values according to your actual setup.
export HAKA_DB_USER=test
export HAKA_DB_PASS=test
export HAKA_DB_NAME=test
export HAKA_DB_HOST=localhost
export HAKA_DB_PORT=5432

hakatime run
```

### Dashboard

1. Point your browser to [http://localhost:8080](http://localhost:8080)
2. Create a new user.
3. Create an API token and set up your Wakatime client with it.

## CLI options

```
hakatime :: v0.1.0

Usage: hakatime COMMAND
  Wakatime server implementation

Available options:
  -h,--help                Show this help text

Available commands:
  create-token             Create a new auth token
  create-user              Create a new user account
  run-migrations           Apply pending database migrations
  run                      Start the Server
```

## Screens

### Overview

![Overview Page](img/overview.png "Overview Page")

### Projects

![Projects Page](img/projects.png "Projects Page")

## Contributing

Any kind of contribution is greatly appreciated. This could be:

- Bug fixes
- Suggesting/Implementing new features
- UI/UX improvements/suggestions
- Code refactoring
