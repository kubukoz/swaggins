# swagins-cli

> A wizard is never late, Frodo Swaggins.

Swaggins is an opinionated code generator for [OpenAPI](https://github.com/OAI/OpenAPI-Specification).

The idea is to generate client/server code for Scala backends written with http4s, fs2 and circe.

Swaggins borrows heavily from similar tools, most notably [API Builder](https://apibuilder.io/), but with some key differences:

- the spec is independently hosted (e.g. on GitHub)
- the spec file is passed as a path when running the CLI tool, or hardcoded in the config file
- the spec format is OpenApi 3.0.x
- the CLI runs without a server

and more.

Roadmap:

- http4s server generator
- axios client generator
- android (retrofit) client generator
- ???

## Packaging
`sbt stage` for getting a raw binary, `sbt universal:packageBin` for zipfile
