export type TestSchema = ClassWithMap | ClassWithLong | ClassWithInt

export class ClassWithMap {
  constructor(
    readonly m: Array<[string, string]> = [],
  ) {}
}

export class ClassWithLong {
  constructor(
    readonly x: bigint,
  ) {}
}

export class ClassWithInt {
  constructor(
    readonly x: number,
  ) {}
}