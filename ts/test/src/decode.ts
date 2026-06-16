import { Decode } from "../proto"
import { PageType, PageWidgets, PageUrl, PageSeo, FieldNode } from "./common"

export type Push = SiteOpts | Permissions | Page | PageTreeItem | Ping | ComponentTemplateOk

export class SiteOpts {
  constructor(
    readonly xs: Array<SiteOpt> = [],
  ) {}
}

export interface SiteOpt {
  id: string
  label?: string
}

export class Permissions {
  constructor(
    readonly xs: Array<string> = [],
  ) {}
}

export class Page {
  constructor(
    readonly tpe: PageType,
    readonly guest: boolean,
    readonly seo: PageSeo,
    readonly mobileSeo?: PageSeo,
    readonly name: Array<[string, string]> = [],
  ) {}
}

export class PageTreeItem {
  constructor(
    readonly priority: number,
  ) {}
}

export class Ping {}

export class ComponentTemplateOk {
  constructor(
    readonly fieldNode: FieldNode,
    readonly fieldNode1: FieldNode1,
  ) {}
}

export interface FieldNode1 {
  root?: string
  forest: Array<FieldNode1>
}

export function decodePush(__xs: Uint8Array): Push {
  const __t = Decode.unsignedVarint32(__xs, 0)
  const __pos1 = __t.pos
  switch (__t.val >>> 3) {
    case 1: { const __r = decodeSiteOpts(__xs, __pos1); return __r.val }
    case 2: { const __r = decodePermissions(__xs, __pos1); return __r.val }
    case 3: { const __r = decodePage(__xs, __pos1); return __r.val }
    case 4: { const __r = decodePageTreeItem(__xs, __pos1); return __r.val }
    case 5: { const __r = decodePing(__xs, __pos1); return __r.val }
    case 1300: { const __r = decodeComponentTemplateOk(__xs, __pos1); return __r.val }
    default: throw new Decode.BadType(__t.val >>> 3)
  }
}

export function decodeSiteOpts(__xs: Uint8Array, __pos0: number): { pos: number, val: SiteOpts } {
  const __m = Decode.unsignedVarint32(__xs, __pos0)
  let __pos = __m.pos
  const __end = __pos + __m.val
  let xs: Array<SiteOpt> = []
  while (__pos < __end) {
    const __t = Decode.unsignedVarint32(__xs, __pos); __pos = __t.pos
    switch (__t.val >>> 3) {
      case 1: { const __r = decodeSiteOpt(__xs, __pos); xs.push(__r.val); __pos = __r.pos; break }
      default: { __pos = Decode.skipType(__xs, __pos, __t.val & 7); break }
    }
  }
  return { pos: __pos, val: new SiteOpts(xs) }
}

export function decodeSiteOpt(__xs: Uint8Array, __pos0: number): { pos: number, val: SiteOpt } {
  const __m = Decode.unsignedVarint32(__xs, __pos0)
  let __pos = __m.pos
  const __end = __pos + __m.val
  let id: string | undefined = undefined
  let label: string | undefined = undefined
  while (__pos < __end) {
    const __t = Decode.unsignedVarint32(__xs, __pos); __pos = __t.pos
    switch (__t.val >>> 3) {
      case 1: { const __r = Decode.string(__xs, __pos); id = __r.val; __pos = __r.pos; break }
      case 2: { const __r = Decode.string(__xs, __pos); label = __r.val; __pos = __r.pos; break }
      default: { __pos = Decode.skipType(__xs, __pos, __t.val & 7); break }
    }
  }
  if (id === undefined) throw new Decode.MissingFields("SiteOpt")
  return { pos: __pos, val: { id, label } }
}

export function decodePermissions(__xs: Uint8Array, __pos0: number): { pos: number, val: Permissions } {
  const __m = Decode.unsignedVarint32(__xs, __pos0)
  let __pos = __m.pos
  const __end = __pos + __m.val
  let xs: Array<string> = []
  while (__pos < __end) {
    const __t = Decode.unsignedVarint32(__xs, __pos); __pos = __t.pos
    switch (__t.val >>> 3) {
      case 1: { const __r = Decode.string(__xs, __pos); xs.push(__r.val); __pos = __r.pos; break }
      default: { __pos = Decode.skipType(__xs, __pos, __t.val & 7); break }
    }
  }
  return { pos: __pos, val: new Permissions(xs) }
}

export function decodePage(__xs: Uint8Array, __pos0: number): { pos: number, val: Page } {
  const __m = Decode.unsignedVarint32(__xs, __pos0)
  let __pos = __m.pos
  const __end = __pos + __m.val
  let tpe: PageType | undefined = undefined
  let guest: boolean | undefined = undefined
  let seo: PageSeo | undefined = undefined
  let mobileSeo: PageSeo | undefined = undefined
  let name: Array<[string, string]> = []
  while (__pos < __end) {
    const __t = Decode.unsignedVarint32(__xs, __pos); __pos = __t.pos
    switch (__t.val >>> 3) {
      case 1: { const __r = decodePageType(__xs, __pos); tpe = __r.val; __pos = __r.pos; break }
      case 2: { const __r = Decode.boolean(__xs, __pos); guest = __r.val; __pos = __r.pos; break }
      case 3: { const __r = decodePageSeo(__xs, __pos); seo = __r.val; __pos = __r.pos; break }
      case 4: { const __r = decodePageSeo(__xs, __pos); mobileSeo = __r.val; __pos = __r.pos; break }
      case 5: { const __r = decodeStringString(__xs, __pos); name.push(__r.val); __pos = __r.pos; break }
      default: { __pos = Decode.skipType(__xs, __pos, __t.val & 7); break }
    }
  }
  if (tpe === undefined || guest === undefined || seo === undefined) throw new Decode.MissingFields("Page")
  return { pos: __pos, val: new Page(tpe, guest, seo, mobileSeo, name) }
}

export function decodePageType(__xs: Uint8Array, __pos0: number): { pos: number, val: PageType } {
  const __m = Decode.unsignedVarint32(__xs, __pos0)
  let __pos = __m.pos
  const __end = __pos + __m.val
  let __acc: PageType | undefined = undefined
  while (__pos < __end) {
    const __t = Decode.unsignedVarint32(__xs, __pos); __pos = __t.pos
    switch (__t.val >>> 3) {
      case 1: { const __r = decodePageWidgets(__xs, __pos); __acc = __r.val; __pos = __r.pos; break }
      case 2: { const __r = decodePageUrl(__xs, __pos); __acc = __r.val; __pos = __r.pos; break }
      default: { __pos = Decode.skipType(__xs, __pos, __t.val & 7); break }
    }
  }
  if (__acc === undefined) throw new Decode.MissingFields("PageType")
  return { pos: __pos, val: __acc }
}

export function decodePageWidgets(__xs: Uint8Array, __pos0: number): { pos: number, val: PageWidgets } {
  const __m = Decode.unsignedVarint32(__xs, __pos0)
  return { pos: __m.pos + __m.val, val: new PageWidgets() }
}

export function decodePageUrl(__xs: Uint8Array, __pos0: number): { pos: number, val: PageUrl } {
  const __m = Decode.unsignedVarint32(__xs, __pos0)
  let __pos = __m.pos
  const __end = __pos + __m.val
  let addr: string | undefined = undefined
  while (__pos < __end) {
    const __t = Decode.unsignedVarint32(__xs, __pos); __pos = __t.pos
    switch (__t.val >>> 3) {
      case 1: { const __r = Decode.string(__xs, __pos); addr = __r.val; __pos = __r.pos; break }
      default: { __pos = Decode.skipType(__xs, __pos, __t.val & 7); break }
    }
  }
  if (addr === undefined) throw new Decode.MissingFields("PageUrl")
  return { pos: __pos, val: new PageUrl(addr) }
}

export function decodePageSeo(__xs: Uint8Array, __pos0: number): { pos: number, val: PageSeo } {
  const __m = Decode.unsignedVarint32(__xs, __pos0)
  let __pos = __m.pos
  const __end = __pos + __m.val
  let descr: string | undefined = undefined
  let order: number | undefined = undefined
  while (__pos < __end) {
    const __t = Decode.unsignedVarint32(__xs, __pos); __pos = __t.pos
    switch (__t.val >>> 3) {
      case 1: { const __r = Decode.string(__xs, __pos); descr = __r.val; __pos = __r.pos; break }
      case 2: { const __r = Decode.double(__xs, __pos); order = __r.val; __pos = __r.pos; break }
      default: { __pos = Decode.skipType(__xs, __pos, __t.val & 7); break }
    }
  }
  if (descr === undefined || order === undefined) throw new Decode.MissingFields("PageSeo")
  return { pos: __pos, val: { descr, order } }
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

export function decodePageTreeItem(__xs: Uint8Array, __pos0: number): { pos: number, val: PageTreeItem } {
  const __m = Decode.unsignedVarint32(__xs, __pos0)
  let __pos = __m.pos
  const __end = __pos + __m.val
  let priority: number | undefined = undefined
  while (__pos < __end) {
    const __t = Decode.unsignedVarint32(__xs, __pos); __pos = __t.pos
    switch (__t.val >>> 3) {
      case 1: { const __r = Decode.signedVarint32(__xs, __pos); priority = __r.val; __pos = __r.pos; break }
      default: { __pos = Decode.skipType(__xs, __pos, __t.val & 7); break }
    }
  }
  if (priority === undefined) throw new Decode.MissingFields("PageTreeItem")
  return { pos: __pos, val: new PageTreeItem(priority) }
}

export function decodePing(__xs: Uint8Array, __pos0: number): { pos: number, val: Ping } {
  const __m = Decode.unsignedVarint32(__xs, __pos0)
  return { pos: __m.pos + __m.val, val: new Ping() }
}

export function decodeComponentTemplateOk(__xs: Uint8Array, __pos0: number): { pos: number, val: ComponentTemplateOk } {
  const __m = Decode.unsignedVarint32(__xs, __pos0)
  let __pos = __m.pos
  const __end = __pos + __m.val
  let fieldNode: FieldNode | undefined = undefined
  let fieldNode1: FieldNode1 | undefined = undefined
  while (__pos < __end) {
    const __t = Decode.unsignedVarint32(__xs, __pos); __pos = __t.pos
    switch (__t.val >>> 3) {
      case 1: { const __r = decodeFieldNode(__xs, __pos); fieldNode = __r.val; __pos = __r.pos; break }
      case 2: { const __r = decodeFieldNode1(__xs, __pos); fieldNode1 = __r.val; __pos = __r.pos; break }
      default: { __pos = Decode.skipType(__xs, __pos, __t.val & 7); break }
    }
  }
  if (fieldNode === undefined || fieldNode1 === undefined) throw new Decode.MissingFields("ComponentTemplateOk")
  return { pos: __pos, val: new ComponentTemplateOk(fieldNode, fieldNode1) }
}

export function decodeFieldNode(__xs: Uint8Array, __pos0: number): { pos: number, val: FieldNode } {
  const __m = Decode.unsignedVarint32(__xs, __pos0)
  let __pos = __m.pos
  const __end = __pos + __m.val
  let root: string | undefined = undefined
  let forest: Array<FieldNode> = []
  while (__pos < __end) {
    const __t = Decode.unsignedVarint32(__xs, __pos); __pos = __t.pos
    switch (__t.val >>> 3) {
      case 1: { const __r = Decode.string(__xs, __pos); root = __r.val; __pos = __r.pos; break }
      case 2: { const __r = decodeFieldNode(__xs, __pos); forest.push(__r.val); __pos = __r.pos; break }
      default: { __pos = Decode.skipType(__xs, __pos, __t.val & 7); break }
    }
  }
  if (root === undefined) throw new Decode.MissingFields("FieldNode")
  return { pos: __pos, val: { root, forest } }
}

export function decodeFieldNode1(__xs: Uint8Array, __pos0: number): { pos: number, val: FieldNode1 } {
  const __m = Decode.unsignedVarint32(__xs, __pos0)
  let __pos = __m.pos
  const __end = __pos + __m.val
  let root: string | undefined = undefined
  let forest: Array<FieldNode1> = []
  while (__pos < __end) {
    const __t = Decode.unsignedVarint32(__xs, __pos); __pos = __t.pos
    switch (__t.val >>> 3) {
      case 1: { const __r = Decode.string(__xs, __pos); root = __r.val; __pos = __r.pos; break }
      case 2: { const __r = decodeFieldNode1(__xs, __pos); forest.push(__r.val); __pos = __r.pos; break }
      default: { __pos = Decode.skipType(__xs, __pos, __t.val & 7); break }
    }
  }
  return { pos: __pos, val: { root, forest } }
}