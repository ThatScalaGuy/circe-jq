# circe-jq

[![circe-jq Scala version support](https://index.scala-lang.org/thatscalaguy/circe-jq/circe-jq/latest-by-scala-version.svg?platform=jvm)](https://index.scala-lang.org/thatscalaguy/circe-jq/circe-jq) 
[![circe-jq Scala version support](https://index.scala-lang.org/thatscalaguy/circe-jq/circe-jq/latest-by-scala-version.svg?platform=sjs1)](https://index.scala-lang.org/thatscalaguy/circe-jq/circe-jq)
[![Maven Central](https://img.shields.io/maven-central/v/de.thatscalaguy/circe-jq_2.13.svg)](https://maven-badges.herokuapp.com/maven-central/de.thatscalaguy/circe-jq_2.13)


Run jq filter on circe json objects.

For more detail go [here](https://thatscalaguy.github.io/circe-jq/).

Inspired by [scalajq](https://github.com/6u1ll4um3/scalajq)

## Current state
- Identity \
```json.jq(".")```
- Object Identifier-Index \
```json.jq(".name")``` \
```json.jq(".[name]")``` \
```json.jq(""".["name"]""")```
- Optional Object Identifier-Index \
```json.jq(".name?")``` \
```json.jq(".[name]?")``` \
```json.jq(""".["name"]?""")```
- Generic Object Index \
```json.jq(""".["name"]""")```\
```json.jq(""".["name"]?""")```
- Array Index \
```json.jq(".[1]")```
- Array/String Slice \
```json.jq(".[1:3]")```
- Pipe \
```json.jq(".user | .name")```

## License

circe-jq\
Copyright 2022 ThatScalaGuy\
Licensed under Apache License 2.0 (see LICENSE)