# hakatime

[![CircleCI](https://circleci.com/gh/mujx/hakatime.svg?style=svg)](https://circleci.com/gh/mujx/hakatime)
[![Docker build](https://img.shields.io/docker/cloud/build/mujx/hakatime)](https://hub.docker.com/r/mujx/hakatime/builds)
[![Latest version](https://img.shields.io/github/v/release/mujx/hakatime)](https://github.com/mujx/hakatime/releases)
[![BuiltWithNix](https://img.shields.io/badge/Built_With-Nix-5277C3.svg?logo=nixos&labelColor=73C3D5)](https://nixos.org/)

Hakatime is a server implementation of [Wakatime](https://wakatime.com/). You can use it as a self-hosted alternative.

It comes together with a dashboard which provides a graphical representation of the collected data.

## Features

- Import Wakatime activity using an API token and a range of dates.
- Group projects together with tags (e.g `#work`, `#personal`) and view their aggregated statistics.
- User registration & login through the UI.
- Badge generation for a project that displays that total amount of hours spent for a configurable
  time period. [![my-app](https://hakatime-demo.mtx-dev.xyz/badge/svg/f8c0f834-3747-4d3f-965b-6fa08c6dea94?days=14)](https://hakatime-demo.mtx-dev.xyz/badge/svg/f8c0f834-3747-4d3f-965b-6fa08c6dea94?days=7)
- Global and per project charts
  - Breakdown by project or language.
  - Breakdown by day of week and hour of the day.
  - Timeline of activity for a configurable time-frame.
  - Total time spent per file.
- API token management & generation.

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

Deploying on ARM is also possible using the dedicated Dockerfile ([`Dockerfile.arm`](/Dockerfile.arm)) to build the image.

```yaml
version: "3"
services:
  server:
    container_name: hakatime
    image: mujx/hakatime:1.1.0
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
      # Update this with the external endpoint that you use to access hakatime.
      HAKA_BADGE_URL: "http://localhost:8080"
      HAKA_PORT: 8080
      HAKA_SHIELDS_IO_URL: "https://img.shields.io"
      HAKA_ENABLE_REGISTRATION: "true" # Toggle after you've created your account.
      # Number of hours after which inactive browser sessions will expire (login required).
      HAKA_SESSION_EXPIRY: "24"
      HAKA_LOG_LEVEL: "info" # Control the verbosity of the logger.
      HAKA_ENV: "dev" # Use a json logger for production, otherwise key=value pairs.
      HAKA_HTTP_LOG: "true" # If you want to log http requests.
    ports:
      - "127.0.0.1:8080:8080"
  haka_db:
    container_name: haka_db
    image: postgres:12-alpine
    environment:
      POSTGRES_DB: test
      POSTGRES_PASSWORD: test
      POSTGRES_USER: test
    volumes:
      - deploy_db_data:/var/lib/postgresql/data

volumes:
  deploy_db_data: {}
```

To start all the services run:

```bash
$ docker-compose -f ./docker-compose-deploy.yml up
```

and navigate to [http://localhost:8080](http://localhost:8080) to access the dashboard.

## Building

### Server

Requirements:

- [GHC](https://www.haskell.org/ghc/) (tested with 8.8 & 8.10)
- [libpq](https://www.postgresql.org/docs/11/libpq.html) (PostgreSQL bindings)
- [cabal-install](https://www.haskell.org/cabal/) (Build system for Haskell)

```bash
cabal build
cabal run exe:hakatime -- run
```

### Dashboard

The output files will be located at `dashboard/dist`.

Requirements:

- Node.js & npm

```bash
cd dashboard

npm install
npm run prod
```

## CLI options

```
hakatime :: v1.1.0

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
