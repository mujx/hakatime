# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Feature

- Added leaderboards for all the user of the instance.

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
