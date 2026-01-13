# Branching and Release Strategy

This fork of the Mental Wellness Index (MWI) repository is used for the Software Maintenance Engineering group project.

The goals of this versioning strategy are:
- Keep the `main` branch stable.
- Use a dedicated branch for active development.
- Document releases clearly for future contributors.

## Branches

### main
- Stable branch.
- Contains reviewed and tested changes.
- Used to generate tagged releases.

### develop
- Branch for active development.
- New features and maintenance tasks are merged here before going to `main`.

### feature/<name>
- Short-lived branches for tasks.
- Example: `feature/ci-setup`, `feature/refactor-comments`.

## Releases and tags

Releases in this fork use semantic versioning:

- `v1.0.0` — Initial fork from MITRE’s repository.
- `v1.1.0` — Assignment 1 SME Project update: added versioning documentation and CI smoke test pipeline.

## How to create a tagged release

```bash
git checkout main
git pull origin main
git tag -a v1.1.0 -m "SME Assignment: Version control and CI pipeline"
git push origin v1.1.0
