# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [1.7.3] - 2023-04-13

### Bug fixes

- Fix off by one error when grouping activity by hour (@ablearthy #74)
- Add psql client on the ARM image (#71)

## [1.7.2] - 2022-09-17

### Bug fixes

- Convert projects with empty name to 'Unknown project' (#62)

## [1.7.1] - 2022-04-10

### Bug fixes

- Add `url` heartbeat type (thanks @georgy-komarov, #59)

## [1.7.0] - 2021-10-25

### Features

- Add endpoint to show the time spent on the current day on the VS Code statusbar. (#50)

## [1.6.3] - 2021-10-16

This release should fix the issues encountered using the new wakatime-cli.

### Bug fixes

- Use `Int64` to represent the `cursorpos` field.
- Respond with the correct response format when single heartbeats are sent. (#49)

## [1.6.2] - 2021-10-10

### Bug fixes

- Parse the `cursorpos` heartbeat field as either `Text` or `Int` (#46)
- Reply with 202 status code when new heartbeats are inserted (#48)

## [1.6.1] - 2021-08-31

### Bug fixes

- Switched from `api.wakatime.com` to `wakatime.com` during data import. The former endpoint was
  returning HTML instead of JSON and wasn't working over HTTPS. (#43)

## [1.6.0] - 2021-08-31

### Features

- Add ability to view aggregated project data with tags (instead of aliases) (#32)
  To use, set a common tag on the projects you want to view as one and select the tag on the
  Projects page.

## [1.5.0] - 2021-08-08

### Features

- Add ability to set names on API tokens (#38)

## [1.4.1] - 2021-06-14

### Bug fixes

- Allow remote write http endpoints and custom port (#36)
- Install the `ca-certificates` package in the arm Dockerfile (#36)

## [1.4.0] - 2021-06-11

### Features

- Add ability to forward incoming heartbeats to another Wakatime compatible server.

### Improvements

- Docker images for ARM64 are built automatically by CircleCI.

## [1.3.2] - 2021-04-11

### Bug fixes

- Update project list according to the selected date range.

## [1.3.1] - 2021-04-04

### Improvements

- Don't display coding time for merge commits.
- Show hours & minutes on all charts.
- Update some dashboard dependencies.

## [1.3.0] - 2021-03-19

### Features

- Added API endpoint & UI modal to see time spent per GitHub commit.

### Improvements

- Converted all tooltips to use the Bootstrap theme.
- Added explanation about the cut-off limit (renamed to timeout).

## [1.2.0] - 2021-03-14

### Features

- Added leaderboards for the users of the instance.

### Improvements

- Small visual improvements on the login & register pages.
- Removed very low percentages from pie charts to reduce noise.

## [1.1.0] - 2021-03-01

### Features

- API endpoint & UI controls to attach tags to projects. (e.g `work`, `web`, `ui`)
- Ability to filter projects by tag and view their aggregated statistics.

### Improvements

- Reduced the amount of api calls made by the UI.
- Error popups on the UI when queries fail.
- The docker image will apply the database migrations on boot.

### Bug fixes

- Removed a small UI freeze by not redrawing immediately upon dropdown selection.

## [1.0.1] - 2021-02-23

### Improvements

- The following file extensions are now recognized (the wakatime plugin sends no values for these):
  - `.gotmpl` -> `Go template`
  - `.tfvars` -> `Terraform`
  - `.dhall` -> `Dhall`
  - `.zig` -> `Zig`
  - `.org` -> `Org`
  - `.purs` -> `PureScript`
  - `.cabal` -> `Cabal config`
  - `.jinja|.jinja2` -> `Jinja`
- Updated the docker image to use GHC 8.10.
- Reduced the size of the docker image down to ~85 MB.
- Removed some unnecessary direct dependencies.

### Bug fixes

- Fixed the Dockerfile for ARM (#21). (TravisCI experiences random failures and it's currently not possible to
  provide regular docker builds for ARM)

## [1.0.0] - 2021-02-16

Initial versioned release.

### Features

- Import Wakatime activity using an API token and a range of dates.
- User registration & login through the UI.
- Badge generation for a project that displays that total amount of hours spent for a configurable
  time period.
- Global and per project charts
  - Breakdown by project or language.
  - Breakdown by day of week and hour of the day.
  - Timeline of activity for a configurable time-frame.
  - Total time spent per file.
- API token management & generation.
