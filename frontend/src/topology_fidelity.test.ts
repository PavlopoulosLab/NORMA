import { expect, test } from 'vitest'
import { topologyFidelity } from './topology_fidelity'

// a path a-b-c-d laid out on a line keeps every neighbourhood and every distance
test('a path drawn as a straight line scores perfectly', () => {
  const ids = ['a', 'b', 'c', 'd']
  const edges = [
    { source: 'a', target: 'b' },
    { source: 'b', target: 'c' },
    { source: 'c', target: 'd' },
  ]
  const positions = {
    a: { x: 0, y: 0 },
    b: { x: 10, y: 0 },
    c: { x: 20, y: 0 },
    d: { x: 30, y: 0 },
  }
  const f = topologyFidelity(positions, ids, edges)
  expect(f.neighbourhood).toBeCloseTo(1, 6)
  expect(f.stress).toBeCloseTo(0, 6)
  expect(f.nodes).toBe(4)
})

// the same path with the ends swapped on screen: neighbours are no longer nearest
test('scrambled positions lower neighbourhood preservation and raise stress', () => {
  const ids = ['a', 'b', 'c', 'd']
  const edges = [
    { source: 'a', target: 'b' },
    { source: 'b', target: 'c' },
    { source: 'c', target: 'd' },
  ]
  const positions = {
    a: { x: 20, y: 0 },
    b: { x: 0, y: 0 },
    c: { x: 30, y: 0 },
    d: { x: 10, y: 0 },
  }
  const f = topologyFidelity(positions, ids, edges)
  expect(f.neighbourhood).toBeLessThan(0.6)
  expect(f.stress).toBeGreaterThan(0.1)
})

test('scale does not matter and disconnected pairs are ignored', () => {
  const ids = ['a', 'b', 'c', 'x', 'y']
  const edges = [
    { source: 'a', target: 'b' },
    { source: 'b', target: 'c' },
    { source: 'x', target: 'y' },
  ]
  const small = {
    a: { x: 0, y: 0 },
    b: { x: 1, y: 0 },
    c: { x: 2, y: 0 },
    x: { x: 0, y: 5 },
    y: { x: 1, y: 5 },
  }
  const big = Object.fromEntries(
    Object.entries(small).map(([k, p]) => [k, { x: p.x * 100, y: p.y * 100 }])
  )
  const f1 = topologyFidelity(small, ids, edges)
  const f2 = topologyFidelity(big, ids, edges)
  expect(f1.stress).toBeCloseTo(f2.stress, 9)
  expect(f1.neighbourhood).toBeCloseTo(1, 6)
  expect(f1.stress).toBeCloseTo(0, 6)
})

test('too few nodes gives no score', () => {
  const f = topologyFidelity({ a: { x: 0, y: 0 } }, ['a'], [])
  expect(Number.isNaN(f.neighbourhood)).toBe(true)
  expect(f.nodes).toBe(0)
})
