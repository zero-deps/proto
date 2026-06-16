import { Decode } from "../proto"
import { SimpleT1, SimpleT2 } from "./defaultCommon"

export type DPush = SimpleT1 | SimpleT2

export function decodeDPush(__xs: Uint8Array): DPush {
  const __t = Decode.unsignedVarint32(__xs, 0)
  const __pos1 = __t.pos
  switch (__t.val >>> 3) {
    case 1: { const __r = decodeSimpleT1(__xs, __pos1); return __r.val }
    case 2: { const __r = decodeSimpleT2(__xs, __pos1); return __r.val }
    default: throw new Decode.BadType(__t.val >>> 3)
  }
}

export function decodeSimpleT1(__xs: Uint8Array, __pos0: number): { pos: number, val: SimpleT1 } {
  const __m = Decode.unsignedVarint32(__xs, __pos0)
  let __pos = __m.pos
  const __end = __pos + __m.val
  let m1: boolean | undefined = undefined
  let b1: boolean = false
  let b2: string = ""
  while (__pos < __end) {
    const __t = Decode.unsignedVarint32(__xs, __pos); __pos = __t.pos
    switch (__t.val >>> 3) {
      case 1: { const __r = Decode.boolean(__xs, __pos); m1 = __r.val; __pos = __r.pos; break }
      case 2: { const __r = Decode.boolean(__xs, __pos); b1 = __r.val; __pos = __r.pos; break }
      case 3: { const __r = Decode.string(__xs, __pos); b2 = __r.val; __pos = __r.pos; break }
      default: { __pos = Decode.skipType(__xs, __pos, __t.val & 7); break }
    }
  }
  return { pos: __pos, val: new SimpleT1(m1, b1, b2) }
}

export function decodeSimpleT2(__xs: Uint8Array, __pos0: number): { pos: number, val: SimpleT2 } {
  const __m = Decode.unsignedVarint32(__xs, __pos0)
  let __pos = __m.pos
  const __end = __pos + __m.val
  let b0: boolean | undefined = undefined
  let b1: boolean = false
  let b2: string = ""
  while (__pos < __end) {
    const __t = Decode.unsignedVarint32(__xs, __pos); __pos = __t.pos
    switch (__t.val >>> 3) {
      case 1: { const __r = Decode.boolean(__xs, __pos); b0 = __r.val; __pos = __r.pos; break }
      case 2: { const __r = Decode.boolean(__xs, __pos); b1 = __r.val; __pos = __r.pos; break }
      case 3: { const __r = Decode.string(__xs, __pos); b2 = __r.val; __pos = __r.pos; break }
      default: { __pos = Decode.skipType(__xs, __pos, __t.val & 7); break }
    }
  }
  if (b0 === undefined) throw new Decode.MissingFields("SimpleT2")
  return { pos: __pos, val: new SimpleT2(b0, b1, b2) }
}