import { describe, expect, test } from 'vitest'
import { runCommunityAlgorithm } from './mcl'

// Two 4-cliques joined by a single edge: every algorithm must find the two cliques.
function twoCliques() {
  const edges: [number, number][] = []
  for (const base of [0, 4])
    for (let i = 0; i < 4; i++) for (let j = i + 1; j < 4; j++) edges.push([base + i, base + j])
  edges.push([3, 4])
  const adj: number[][] = Array.from({ length: 8 }, () => [])
  for (const [a, b] of edges) {
    adj[a].push(b)
    adj[b].push(a)
  }
  return { n: 8, m: edges.length, adj }
}

describe.each(['louvain', 'leiden', 'lpa', 'walktrap', 'mcl'])('%s', (algo) => {
  test('separates two cliques', () => {
    const r = runCommunityAlgorithm(twoCliques(), algo)
    expect(r.count).toBe(2)
    const m = Array.from(r.membership as Int32Array)
    expect(new Set(m.slice(0, 4)).size).toBe(1)
    expect(new Set(m.slice(4)).size).toBe(1)
    expect(m[0]).not.toBe(m[7])
    expect(r.sizes).toEqual([4, 4])
    // 13 edges; each clique: 12/26 - (13/26)^2
    expect(r.modularity).toBeCloseTo(2 * (12 / 26 - (13 / 26) ** 2), 6)
  })

  test('is deterministic', () => {
    const a = runCommunityAlgorithm(twoCliques(), algo)
    const b = runCommunityAlgorithm(twoCliques(), algo)
    expect(Array.from(a.membership as Int32Array)).toEqual(Array.from(b.membership as Int32Array))
  })
})

test('a graph without edges is one community per node', () => {
  const r = runCommunityAlgorithm({ n: 3, m: 0, adj: [[], [], []] }, 'leiden')
  expect(r.count).toBe(3)
  expect(Number.isNaN(r.modularity)).toBe(true)
})

test('unknown algorithm throws', () => {
  expect(() => runCommunityAlgorithm(twoCliques(), 'nope')).toThrow(/Unknown community algorithm/)
})
