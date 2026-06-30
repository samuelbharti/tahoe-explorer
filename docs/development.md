# Development Guide

## Structure

- Keep global setup in `global.R`.
- Keep tab/page layouts in `userInterface/`.
- Keep reusable module pairs in `modules/`.
- Keep utility helpers in `R/`.

## Workflow

1. Create a branch from `dev`.
2. Add UI or server changes in the appropriate folders.
3. Run the app locally.
4. Restore dependencies with `renv::restore()` whenever package versions change.

## Deployment

This template is meant for direct app publishing or Docker-based deployment.
CI/CD workflows can be added later per project, but they are not assumed here.
