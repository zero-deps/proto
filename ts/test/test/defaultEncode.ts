import { Encode } from "../proto"
import { SimpleT1, SimpleT2 } from "./defaultCommon"

export type DPull = SimpleT1 | SimpleT2

export function encodeDPull(x: DPull): Uint8Array {
  if (x instanceof SimpleT1) return Encode.concatAll([Encode.unsignedVarint32(10), encodeSimpleT1(x)])
  if (x instanceof SimpleT2) return Encode.concatAll([Encode.unsignedVarint32(18), encodeSimpleT2(x)])
  throw new Error("encodeDPull: unknown variant")
}

export function encodeSimpleT1(msg: SimpleT1): Uint8Array {
  const xs = Encode.concatAll([
    (msg.m1 === undefined ? new Uint8Array(0) : Encode.concatAll([Encode.unsignedVarint32(8), Encode.boolean(msg.m1)])),
    Encode.unsignedVarint32(16),
    Encode.boolean(msg.b1),
    Encode.unsignedVarint32(26),
    Encode.string(msg.b2),
  ])
  return Encode.concatAll([Encode.unsignedVarint32(xs.length), xs])
}

export function encodeSimpleT2(msg: SimpleT2): Uint8Array {
  const xs = Encode.concatAll([
    Encode.unsignedVarint32(8),
    Encode.boolean(msg.b0),
    Encode.unsignedVarint32(16),
    Encode.boolean(msg.b1),
    Encode.unsignedVarint32(26),
    Encode.string(msg.b2),
  ])
  return Encode.concatAll([Encode.unsignedVarint32(xs.length), xs])
}