import { Decode } from "../proto"
import { TestSchema, ClassWithMap, ClassWithLong, ClassWithInt } from "./schemaCommon"

export function decodeTestSchema(__xs: Uint8Array): TestSchema {
  const __t = Decode.unsignedVarint32(__xs, 0)
  const __pos1 = __t.pos
  switch (__t.val >>> 3) {
    case 1: { const __r = decodeClassWithMap(__xs, __pos1); return __r.val }
    case 2: { const __r = decodeClassWithLong(__xs, __pos1); return __r.val }
    case 3: { const __r = decodeClassWithInt(__xs, __pos1); return __r.val }
    default: throw new Decode.BadType(__t.val >>> 3)
  }
}

export function decodeClassWithMap(__xs: Uint8Array, __pos0: number): { pos: number, val: ClassWithMap } {
  const __m = Decode.unsignedVarint32(__xs, __pos0)
  let __pos = __m.pos
  const __end = __pos + __m.val
  let m: Array<[string, string]> = []
  while (__pos < __end) {
    const __t = Decode.unsignedVarint32(__xs, __pos); __pos = __t.pos
    switch (__t.val >>> 3) {
      case 1: { const __r = decodeStringString(__xs, __pos); m.push(__r.val); __pos = __r.pos; break }
      default: { __pos = Decode.skipType(__xs, __pos, __t.val & 7); break }
    }
  }
  return { pos: __pos, val: new ClassWithMap(m) }
}

export function decodeStringString(__xs: Uint8Array, __pos0: number): { pos: number, val: [string, string] } {
  const __m = Decode.unsignedVarint32(__xs, __pos0)
  let __pos = __m.pos
  const __end = __pos + __m.val
  let _1: string | undefined = undefined
  let _2: string | undefined = undefined
  while (__pos < __end) {
    const __t = Decode.unsignedVarint32(__xs, __pos); __pos = __t.pos
    switch (__t.val >>> 3) {
      case 1: { const __r = Decode.string(__xs, __pos); _1 = __r.val; __pos = __r.pos; break }
      case 2: { const __r = Decode.string(__xs, __pos); _2 = __r.val; __pos = __r.pos; break }
      default: { __pos = Decode.skipType(__xs, __pos, __t.val & 7); break }
    }
  }
  if (_1 === undefined || _2 === undefined) throw new Decode.MissingFields("StringString")
  return { pos: __pos, val: [_1, _2] }
}

export function decodeClassWithLong(__xs: Uint8Array, __pos0: number): { pos: number, val: ClassWithLong } {
  const __m = Decode.unsignedVarint32(__xs, __pos0)
  let __pos = __m.pos
  const __end = __pos + __m.val
  let x: bigint | undefined = undefined
  while (__pos < __end) {
    const __t = Decode.unsignedVarint32(__xs, __pos); __pos = __t.pos
    switch (__t.val >>> 3) {
      case 1: { const __r = Decode.bigInt(__xs, __pos); x = __r.val; __pos = __r.pos; break }
      default: { __pos = Decode.skipType(__xs, __pos, __t.val & 7); break }
    }
  }
  if (x === undefined) throw new Decode.MissingFields("ClassWithLong")
  return { pos: __pos, val: new ClassWithLong(x) }
}

export function decodeClassWithInt(__xs: Uint8Array, __pos0: number): { pos: number, val: ClassWithInt } {
  const __m = Decode.unsignedVarint32(__xs, __pos0)
  let __pos = __m.pos
  const __end = __pos + __m.val
  let x: number | undefined = undefined
  while (__pos < __end) {
    const __t = Decode.unsignedVarint32(__xs, __pos); __pos = __t.pos
    switch (__t.val >>> 3) {
      case 1: { const __r = Decode.signedVarint32(__xs, __pos); x = __r.val; __pos = __r.pos; break }
      default: { __pos = Decode.skipType(__xs, __pos, __t.val & 7); break }
    }
  }
  if (x === undefined) throw new Decode.MissingFields("ClassWithInt")
  return { pos: __pos, val: new ClassWithInt(x) }
}