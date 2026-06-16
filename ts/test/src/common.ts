export type PageType = PageWidgets | PageUrl

export class PageWidgets {}

export class PageUrl {
  constructor(
    readonly addr: string,
  ) {}
}

export interface PageSeo {
  descr: string
  order: number
}

export interface FieldNode {
  root: string
  forest: Array<FieldNode>
}