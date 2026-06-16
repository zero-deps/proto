import { Encode } from "../proto"
import { TestSchema, ClassWithMap, ClassWithLong, ClassWithInt } from "./schemaCommon"

export function encodeTestSchema(x: TestSchema): Uint8Array {
  if (x instanceof ClassWithMap) return Encode.concatAll([Encode.unsignedVarint32(10), encodeClassWithMap(x)])
  if (x instanceof ClassWithLong) return Encode.concatAll([Encode.unsignedVarint32(18), encodeClassWithLong(x)])
  if (x instanceof ClassWithInt) return Encode.concatAll([Encode.unsignedVarint32(26), encodeClassWithInt(x)])
  throw new Error("encodeTestSchema: unknown variant")
}

export function encodeClassWithMap(msg: ClassWithMap): Uint8Array {
  const xs = Encode.concatAll([
    Encode.concatAll(msg.m.flatMap(x => [Encode.unsignedVarint32(10), encodeStringString(x)])),
  ])
  return Encode.concatAll([Encode.unsignedVarint32(xs.length), xs])
}

export function encodeStringString(x: [string, string]): Uint8Array {
  const msg = { _1: x[0], _2: x[1] }
  const xs = Encode.concatAll([
    Encode.unsignedVarint32(10),
    Encode.string(msg._1),
    Encode.unsignedVarint32(18),
    Encode.string(msg._2),
  ])
  return Encode.concatAll([Encode.unsignedVarint32(xs.length), xs])
}

export function encodeClassWithLong(msg: ClassWithLong): Uint8Array {
  const xs = Encode.concatAll([
    Encode.unsignedVarint32(8),
    Encode.bigInt(msg.x),
  ])
  return Encode.concatAll([Encode.unsignedVarint32(xs.length), xs])
}

export function encodeClassWithInt(msg: ClassWithInt): Uint8Array {
  const xs = Encode.concatAll([
    Encode.unsignedVarint32(8),
    Encode.signedVarint32(msg.x),
  ])
  return Encode.concatAll([Encode.unsignedVarint32(xs.length), xs])
}