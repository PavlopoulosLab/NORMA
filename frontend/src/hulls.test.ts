import { expect, test } from 'vitest'
import { convexHull, hexToRgba, inflateHull } from './hulls'

type Pt = { x: number; y: number }

test('convexHull drops interior points and keeps counter-clockwise corners', () => {
  const pts = [
    { x: 0, y: 0 },
    { x: 2, y: 0 },
    { x: 2, y: 2 },
    { x: 0, y: 2 },
    { x: 1, y: 1 },
    { x: 1, y: 0.5 },
  ]
  const hull = convexHull(pts)
  expect(hull).toHaveLength(4)
  expect(hull.map((p: Pt) => `${p.x},${p.y}`)).toEqual(['0,0', '2,0', '2,2', '0,2'])
})

test('convexHull leaves fewer than three points alone', () => {
  expect(convexHull([{ x: 1, y: 1 }])).toEqual([{ x: 1, y: 1 }])
  expect(convexHull([])).toEqual([])
})

test('inflateHull pushes every vertex away from the centroid by the padding', () => {
  const square = [
    { x: -1, y: -1 },
    { x: 1, y: -1 },
    { x: 1, y: 1 },
    { x: -1, y: 1 },
  ]
  const big = inflateHull(square, 2)
  big.forEach((p: Pt, i: number) => {
    const before = Math.hypot(square[i].x, square[i].y)
    expect(Math.hypot(p.x, p.y)).toBeCloseTo(before + 2)
  })
})

test('hexToRgba', () => {
  expect(hexToRgba('#ff8000', 0.5)).toBe('rgba(255,128,0,0.5)')
  expect(hexToRgba('#zz', 1)).toBe('rgba(0,0,0,1)')
})
