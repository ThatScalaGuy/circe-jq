# circe-jq

Run jq filter on circe json objects

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