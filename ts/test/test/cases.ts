import { TestSchema, ClassWithMap, ClassWithLong, ClassWithInt } from "./schemaCommon"

export const map_schema: TestSchema = new ClassWithMap([ ["en_GB", "Hello"], ["it_IT", "Ciao"] ])
export const map_bytes: number[] = [10,31,10,14,10,5,101,110,95,71,66,18,5,72,101,108,108,111,10,13,10,5,105,116,95,73,84,18,4,67,105,97,111]

export const maxlong_schema: TestSchema = new ClassWithLong(9007199254740991n)
export const maxlong_bytes: number[] = [18,9,8,255,255,255,255,255,255,255,15]

export const minlong_schema: TestSchema = new ClassWithLong(-9007199254740991n)
export const minlong_bytes: number[] = [18,11,8,129,128,128,128,128,128,128,240,255,1]

export const max_bigint_schema: TestSchema = new ClassWithLong(9223372036854775807n)
export const max_bigint_bytes: number[] = [18,10,8,255,255,255,255,255,255,255,255,127]

export const min_bigint_schema: TestSchema = new ClassWithLong(-9223372036854775808n)
export const min_bigint_bytes: number[] = [18,11,8,128,128,128,128,128,128,128,128,128,1]

export const maxint_schema: TestSchema = new ClassWithInt(2147483647)
export const maxint_bytes: number[] = [26,6,8,255,255,255,255,7]

export const minint_schema: TestSchema = new ClassWithInt(-2147483648)
export const minint_bytes: number[] = [26,11,8,128,128,128,128,248,255,255,255,255,1]
