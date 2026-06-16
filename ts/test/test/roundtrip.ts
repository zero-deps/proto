// Round-trip / byte-compatibility test against fixtures encoded by the JVM `proto` library.
// Run with: npx -y tsx test/roundtrip.ts   (from ts/test/)

import { encodeTestSchema } from "./schemaEncode"
import { decodeTestSchema } from "./schemaDecode"
import { TestSchema } from "./schemaCommon"
import * as C from "./cases"

function eq(a: unknown, b: unknown): boolean {
  if (a === b) return true
  if (typeof a === "bigint" || typeof b === "bigint") return a === b
  if (a instanceof Uint8Array && b instanceof Uint8Array) {
    if (a.length !== b.length) return false
    for (let i = 0; i < a.length; i++) if (a[i] !== b[i]) return false
    return true
  }
  if (Array.isArray(a) && Array.isArray(b)) {
    if (a.length !== b.length) return false
    return a.every((x, i) => eq(x, b[i]))
  }
  if (a && b && typeof a === "object" && typeof b === "object") {
    const ka = Object.keys(a as object)
    const kb = Object.keys(b as object)
    if (ka.length !== kb.length) return false
    return ka.every(k => eq((a as Record<string, unknown>)[k], (b as Record<string, unknown>)[k]))
  }
  return false
}

let failures = 0
function check(name: string, cond: boolean): void {
  if (!cond) {
    failures++
    console.error("FAIL: " + name)
  } else {
    console.log("ok: " + name)
  }
}

const cases: [string, TestSchema, number[]][] = [
  ["map", C.map_schema, C.map_bytes],
  ["maxlong", C.maxlong_schema, C.maxlong_bytes],
  ["minlong", C.minlong_schema, C.minlong_bytes],
  ["max_bigint", C.max_bigint_schema, C.max_bigint_bytes],
  ["min_bigint", C.min_bigint_schema, C.min_bigint_bytes],
  ["maxint", C.maxint_schema, C.maxint_bytes],
  ["minint", C.minint_schema, C.minint_bytes],
]

for (const [name, schema, bytes] of cases) {
  const enc = encodeTestSchema(schema)
  check(name + " encode (byte-identical to JVM)", eq(enc, new Uint8Array(bytes)))
  const dec = decodeTestSchema(new Uint8Array(bytes))
  check(name + " decode (structural)", eq(dec, schema))
  check(name + " decode (class identity)", (dec as object).constructor === (schema as object).constructor)
}

if (failures > 0) {
  throw new Error(failures + " failure(s)")
} else {
  console.log("ALL PASSED")
}
