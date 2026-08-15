# Security Policy

## Supported versions

Fixes land on `main` and go out with the next release. Older tags are not
patched.

## Reporting a problem

Please do not open a public issue for a security problem. Email
<samuelbharti.io@gmail.com> instead, describing what you found and, where you
can, the steps to reproduce it. You will get an acknowledgement within a few
days, along with what happens next.

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
