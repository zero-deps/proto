import { Encode } from "../proto"
import { PageType, PageWidgets, PageUrl, PageSeo, FieldNode } from "./common"

export type Pull = GetSites | UploadChunk | SavePage | SaveComponentTemplate | ComponentsSavePrefs

export class GetSites {}

export class UploadChunk {
  constructor(
    readonly id: string,
    readonly chunk: Uint8Array,
    readonly path: Array<string> = [],
  ) {}
}

export class SavePage {
  constructor(
    readonly tpe: PageType,
    readonly guest: boolean,
    readonly seo: PageSeo,
    readonly mobileSeo?: PageSeo,
    readonly name: Array<[string, string]> = [],
  ) {}
}

export class SaveComponentTemplate {
  constructor(
    readonly fieldNode: FieldNode,
  ) {}
}

export class ComponentsSavePrefs {
  constructor(
    readonly id: string,
    readonly pageid: string,
    readonly siteid: string,
    readonly tree: FieldNode,
    readonly extTree?: FieldNode,
  ) {}
}

export function encodePull(x: Pull): Uint8Array {
  if (x instanceof GetSites) return Encode.concatAll([Encode.unsignedVarint32(8002), encodeGetSites(x)])
  if (x instanceof UploadChunk) return Encode.concatAll([Encode.unsignedVarint32(8010), encodeUploadChunk(x)])
  if (x instanceof SavePage) return Encode.concatAll([Encode.unsignedVarint32(8018), encodeSavePage(x)])
  if (x instanceof SaveComponentTemplate) return Encode.concatAll([Encode.unsignedVarint32(11202), encodeSaveComponentTemplate(x)])
  if (x instanceof ComponentsSavePrefs) return Encode.concatAll([Encode.unsignedVarint32(15362), encodeComponentsSavePrefs(x)])
  throw new Error("encodePull: unknown variant")
}

export function encodeGetSites(_msg: GetSites): Uint8Array {
  return Encode.unsignedVarint32(0)
}

export function encodeUploadChunk(msg: UploadChunk): Uint8Array {
  const xs = Encode.concatAll([
    Encode.concatAll(msg.path.flatMap(x => [Encode.unsignedVarint32(10), Encode.string(x)])),
    Encode.unsignedVarint32(18),
    Encode.string(msg.id),
    Encode.unsignedVarint32(26),
    Encode.bytes(msg.chunk),
  ])
  return Encode.concatAll([Encode.unsignedVarint32(xs.length), xs])
}

export function encodeSavePage(msg: SavePage): Uint8Array {
  const xs = Encode.concatAll([
    Encode.unsignedVarint32(10),
    encodePageType(msg.tpe),
    Encode.unsignedVarint32(16),
    Encode.boolean(msg.guest),
    Encode.unsignedVarint32(26),
    encodePageSeo(msg.seo),
    (msg.mobileSeo === undefined ? new Uint8Array(0) : Encode.concatAll([Encode.unsignedVarint32(34), encodePageSeo(msg.mobileSeo)])),
    Encode.concatAll(msg.name.flatMap(x => [Encode.unsignedVarint32(42), encodeStringString(x)])),
  ])
  return Encode.concatAll([Encode.unsignedVarint32(xs.length), xs])
}

export function encodePageType(x: PageType): Uint8Array {
  if (x instanceof PageWidgets) { const xs = Encode.concatAll([Encode.unsignedVarint32(10), encodePageWidgets(x)]); return Encode.concatAll([Encode.unsignedVarint32(xs.length), xs]) }
  if (x instanceof PageUrl) { const xs = Encode.concatAll([Encode.unsignedVarint32(18), encodePageUrl(x)]); return Encode.concatAll([Encode.unsignedVarint32(xs.length), xs]) }
  throw new Error("encodePageType: unknown variant")
}

export function encodePageWidgets(_msg: PageWidgets): Uint8Array {
  return Encode.unsignedVarint32(0)
}

export function encodePageUrl(msg: PageUrl): Uint8Array {
  const xs = Encode.concatAll([
    Encode.unsignedVarint32(10),
    Encode.string(msg.addr),
  ])
  return Encode.concatAll([Encode.unsignedVarint32(xs.length), xs])
}

export function encodePageSeo(msg: PageSeo): Uint8Array {
  const xs = Encode.concatAll([
    Encode.unsignedVarint32(10),
    Encode.string(msg.descr),
    Encode.unsignedVarint32(17),
    Encode.double(msg.order),
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

export function encodeSaveComponentTemplate(msg: SaveComponentTemplate): Uint8Array {
  const xs = Encode.concatAll([
    Encode.unsignedVarint32(10),
    encodeFieldNode(msg.fieldNode),
  ])
  return Encode.concatAll([Encode.unsignedVarint32(xs.length), xs])
}

export function encodeFieldNode(msg: FieldNode): Uint8Array {
  const xs = Encode.concatAll([
    Encode.unsignedVarint32(10),
    Encode.string(msg.root),
    Encode.concatAll(msg.forest.flatMap(x => [Encode.unsignedVarint32(18), encodeFieldNode(x)])),
  ])
  return Encode.concatAll([Encode.unsignedVarint32(xs.length), xs])
}

export function encodeComponentsSavePrefs(msg: ComponentsSavePrefs): Uint8Array {
  const xs = Encode.concatAll([
    Encode.unsignedVarint32(10),
    Encode.string(msg.id),
    Encode.unsignedVarint32(18),
    Encode.string(msg.pageid),
    Encode.unsignedVarint32(26),
    Encode.string(msg.siteid),
    Encode.unsignedVarint32(34),
    encodeFieldNode(msg.tree),
    (msg.extTree === undefined ? new Uint8Array(0) : Encode.concatAll([Encode.unsignedVarint32(42), encodeFieldNode(msg.extTree)])),
  ])
  return Encode.concatAll([Encode.unsignedVarint32(xs.length), xs])
}