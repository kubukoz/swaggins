# swagins-cli

> A wizard is never late, Frodo Swaggins.

Swaggins is an opinionated code generator for [OpenAPI](https://github.com/OAI/OpenAPI-Specification).

The idea is to generate client/server code for Scala backends written with http4s, fs2 and circe.

## Limitations
- JSON only, no support for XML (because it's 2018)

## Inspiration

Swaggins borrows heavily from similar tools, most notably [API Builder](https://apibuilder.io/), but with some key differences:

-                 |Apibuilder | Swaggins
------------------|-----------|-----------------
Spec hosting      | Remote    | Local or remote
Generator hosting | Remote    | Local
Spec format       | Custom    | Standard (OpenApi)
Execution         | Remote    | Local (CLI)

and more.

Roadmap:

- http4s server generator
- axios client generator
- android (retrofit) client generator
- http4s client generator

## Packaging
`sbt stage` for getting a raw binary, `sbt universal:packageBin` for zipfile
