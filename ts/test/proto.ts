// Minimal protobuf wire-format runtime for generated TypeScript codecs.
// Byte-compatible with the JVM `proto` library (Google CodedInputStream/OutputStream):
// non-zigzag signed varints, little-endian fixed32/fixed64, length-delimited strings/bytes.

export type Result<A> = { pos: number; val: A }

// ---------- Encode ----------

function concatAll(parts: Uint8Array[]): Uint8Array {
  let total = 0
  for (const p of parts) total += p.length
  const out = new Uint8Array(total)
  let off = 0
  for (const p of parts) {
    out.set(p, off)
    off += p.length
  }
  return out
}

function unsignedVarint32(x: number): Uint8Array {
  const out: number[] = []
  let v = x >>> 0
  while (v >= 0x80) {
    out.push((v & 0x7f) | 0x80)
    v = v >>> 7
  }
  out.push(v)
  return new Uint8Array(out)
}

function varintBig(u: bigint): Uint8Array {
  const out: number[] = []
  let v = u
  while (v >= 0x80n) {
    out.push(Number((v & 0x7fn) | 0x80n))
    v >>= 7n
  }
  out.push(Number(v))
  return new Uint8Array(out)
}

// protobuf int32: negatives are sign-extended to 64 bits (10-byte varint)
function signedVarint32(x: number): Uint8Array {
  return varintBig(BigInt.asUintN(64, BigInt(Math.trunc(x))))
}

function bigInt(x: bigint): Uint8Array {
  return varintBig(BigInt.asUintN(64, x))
}

function boolean(x: boolean): Uint8Array {
  return new Uint8Array([x ? 1 : 0])
}

function double(x: number): Uint8Array {
  const b = new Uint8Array(8)
  new DataView(b.buffer).setFloat64(0, x, true)
  return b
}

function float(x: number): Uint8Array {
  const b = new Uint8Array(4)
  new DataView(b.buffer).setFloat32(0, x, true)
  return b
}

function string(x: string): Uint8Array {
  const b = new TextEncoder().encode(x)
  return concatAll([unsignedVarint32(b.length), b])
}

function bytes(x: Uint8Array): Uint8Array {
  return concatAll([unsignedVarint32(x.length), x])
}

export const Encode = {
  concatAll,
  unsignedVarint32,
  signedVarint32,
  bigInt,
  boolean,
  double,
  float,
  string,
  bytes,
}

// ---------- Decode ----------

export class MissingFields extends Error {
  constructor(name: string) {
    super(`missing required fields for ${name}`)
    this.name = "MissingFields"
  }
}

export class BadType extends Error {
  constructor(n: number) {
    super(`unexpected field/type ${n}`)
    this.name = "BadType"
  }
}

function readVarintBig(xs: Uint8Array, pos: number): Result<bigint> {
  let result = 0n
  let shift = 0n
  let p = pos
  for (;;) {
    const b = xs[p]
    p += 1
    result |= BigInt(b & 0x7f) << shift
    if ((b & 0x80) === 0) break
    shift += 7n
  }
  return { pos: p, val: result }
}

function d_unsignedVarint32(xs: Uint8Array, pos: number): Result<number> {
  const r = readVarintBig(xs, pos)
  return { pos: r.pos, val: Number(BigInt.asUintN(32, r.val)) }
}

function d_signedVarint32(xs: Uint8Array, pos: number): Result<number> {
  const r = readVarintBig(xs, pos)
  return { pos: r.pos, val: Number(BigInt.asIntN(32, r.val)) }
}

function d_bigInt(xs: Uint8Array, pos: number): Result<bigint> {
  const r = readVarintBig(xs, pos)
  return { pos: r.pos, val: BigInt.asIntN(64, r.val) }
}

function d_boolean(xs: Uint8Array, pos: number): Result<boolean> {
  const r = readVarintBig(xs, pos)
  return { pos: r.pos, val: r.val !== 0n }
}

function d_double(xs: Uint8Array, pos: number): Result<number> {
  const v = new DataView(xs.buffer, xs.byteOffset + pos, 8).getFloat64(0, true)
  return { pos: pos + 8, val: v }
}

function d_float(xs: Uint8Array, pos: number): Result<number> {
  const v = new DataView(xs.buffer, xs.byteOffset + pos, 4).getFloat32(0, true)
  return { pos: pos + 4, val: v }
}

function d_string(xs: Uint8Array, pos: number): Result<string> {
  const len = d_unsignedVarint32(xs, pos)
  const start = len.pos
  const end = start + len.val
  return { pos: end, val: new TextDecoder().decode(xs.subarray(start, end)) }
}

function d_bytes(xs: Uint8Array, pos: number): Result<Uint8Array> {
  const len = d_unsignedVarint32(xs, pos)
  const start = len.pos
  const end = start + len.val
  return { pos: end, val: xs.slice(start, end) }
}

function skipType(xs: Uint8Array, pos: number, wireType: number): number {
  switch (wireType) {
    case 0:
      return readVarintBig(xs, pos).pos
    case 1:
      return pos + 8
    case 2: {
      const len = d_unsignedVarint32(xs, pos)
      return len.pos + len.val
    }
    case 5:
      return pos + 4
    default:
      throw new BadType(wireType)
  }
}

export const Decode = {
  unsignedVarint32: d_unsignedVarint32,
  signedVarint32: d_signedVarint32,
  bigInt: d_bigInt,
  boolean: d_boolean,
  double: d_double,
  float: d_float,
  string: d_string,
  bytes: d_bytes,
  skipType,
  MissingFields,
  BadType,
}
