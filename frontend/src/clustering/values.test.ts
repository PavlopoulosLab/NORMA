import { expect, test } from 'vitest'
import { numericShare, parseNumber, parseNumericValues } from './mcl'

test('parseNumber: numbers, decimal commas, missing markers, junk', () => {
  expect(parseNumber('1.5')).toBe(1.5)
  expect(parseNumber('-2,5')).toBe(-2.5)
  expect(parseNumber(' 3e2 ')).toBe(300)
  expect(parseNumber('NA')).toBeNull()
  expect(parseNumber('')).toBeNull()
  expect(parseNumber('abc')).toBeUndefined()
})

test('numericShare counts rows whose value columns are all numbers', () => {
  expect(numericShare(['A\t1', 'B\t2', 'C\tred'])).toBeCloseTo(2 / 3)
  expect(numericShare(['A'])).toBe(0)
})

test('parseNumericValues reads a header, names columns and reports bad values', () => {
  const r = parseNumericValues('node\tlog2fc\tpval\nA\t1.5\t0.01\nB\tx\t0.2\nB\t-1\t0.3\n')
  expect(r.values.get('A')).toEqual({ log2fc: 1.5, pval: 0.01 })
  expect(r.values.get('B')).toEqual({ log2fc: -1, pval: 0.3 })
  expect(r.notes.join(' ')).toMatch(/column names: log2fc, pval/)
  expect(r.notes.join(' ')).toMatch(/isn't a number/)
  expect(r.notes.join(' ')).toMatch(/repeated node/)
})

test('parseNumericValues without a header names the single column "value"', () => {
  const r = parseNumericValues('A\t1\nB\t2\n')
  expect(r.values.get('B')).toEqual({ value: 2 })
})

test('parseNumericValues throws when nothing is numeric', () => {
  expect(() => parseNumericValues('A\tred\n')).toThrow(/No numeric values/)
})
