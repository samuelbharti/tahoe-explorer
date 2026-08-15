# Security Policy

## Supported versions

I maintain this project on my own. I fix security problems on `main` and in the
next release. I do not backport fixes to older tags.

## Reporting a problem

**Please do not open a public issue for a security problem.**

Email me at <samuelbharti.io@gmail.com>. Tell me what you found and, if you
can, how to reproduce it. I will acknowledge your report within a few days and
tell you what I plan to do about it.

## Worth knowing before you report

- The app needs no credentials to run. `HF_TOKEN` is optional and only raises a
  HuggingFace rate limit.
- The assistant is optional. It reads Google Cloud credentials from your own
  `gcloud` login, or a key that a user pastes for their session. The repository
  stores no key and no token, and `.Renviron` is git-ignored.
- The assistant answers only from a fixed set of tools over the app's metadata.
  No tool reads files, environment variables, or secrets.
- If you deploy the app yourself, the credentials and the environment you
  deploy into are yours to secure. Serve it over HTTPS, since a pasted key
  travels from the browser to the server.
