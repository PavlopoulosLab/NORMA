import { expect, test } from 'vitest'
import { blobPath, capsulePath } from './shading'
import { convexHull, inflateHull } from '../hulls'

// Parses the "M x,y L x,y ... Z" path blobPath emits back into points.
function pointsOf(d: string) {
  return d
    .replace(/Z$/, '')
    .split(/(?=[ML])/)
    .map((seg) => {
      const [x, y] = seg.slice(1).split(',').map(Number)
      return { x, y }
    })
}

function pointInPolygon(pt: { x: number; y: number }, poly: { x: number; y: number }[]) {
  let inside = false
  for (let i = 0, j = poly.length - 1; i < poly.length; j = i++) {
    const a = poly[i],
      b = poly[j]
    const crosses = a.y > pt.y !== b.y > pt.y
    if (!crosses) continue
    const xIntersect = ((b.x - a.x) * (pt.y - a.y)) / (b.y - a.y) + a.x
    if (pt.x < xIntersect) inside = !inside
  }
  return inside
}

test('blobPath traces the exact hull points with straight lines, not rounded midpoints', () => {
  const hull = [
    { x: 0, y: 0 },
    { x: 100, y: 0 },
    { x: 100, y: 100 },
    { x: 0, y: 100 },
  ]
  const d = blobPath(hull)
  expect(pointsOf(d)).toEqual(hull)
})

test('the inflated hull fully covers an outlier node, reproducing the glyA/lgt bug on the trp-operon network', () => {
  // a tight cluster (like trpA-E/trpB/trpC/trpD/trpE) plus two nodes dragged
  // well away from it (like glyA and lgt): the outliers sit at the hull's
  // extreme corners, so a rounded-corner path used to fall short of them.
  const r = 40 // node radius
  const pad = 30 // hull margin
  const nodes = [
    { x: 500, y: 400, r },
    { x: 540, y: 410, r },
    { x: 520, y: 440, r },
    { x: 560, y: 430, r },
    { x: 900, y: 380, r }, // lgt: far to the right
    { x: 520, y: 700, r }, // glyA: far below
    { x: 950, y: 600, r },
  ]
  const maxR = Math.max(...nodes.map((p) => p.r))
  const hull = inflateHull(convexHull(nodes), pad + maxR)
  const d = blobPath(hull)
  const polygon = pointsOf(d)
  nodes.forEach((n) => {
    expect(pointInPolygon(n, polygon)).toBe(true)
  })
})

test('capsulePath (the 2-node case) is unaffected and still a valid closed path', () => {
  const d = capsulePath({ x: 0, y: 0 }, { x: 100, y: 0 }, 20)
  expect(d.startsWith('M')).toBe(true)
  expect(d.endsWith('Z')).toBe(true)
})
