import { expect, test } from 'vitest'
import {
  benjaminiHochberg,
  convertArena3dNetwork,
  hypergeomUpper,
  isArena3dNetworkText,
} from './arena3d'

const ARENA = [
  'SourceNode\tSourceLayer\tTargetNode\tTargetLayer\tWeight\tChannel',
  'A\tL1\tB\tL1\t2\tppi',
  'B\tL1\tC\tL2\t\tgenetic',
  'C\tL2\tD\tL2\t1\tppi',
  '',
].join('\n')

test('recognises Arena3D network files', () => {
  expect(isArena3dNetworkText(ARENA)).toBe(true)
  expect(isArena3dNetworkText('Source\tTarget\nA\tB\n')).toBe(false)
  expect(isArena3dNetworkText('')).toBe(false)
})

test('converts layers to groups and keeps weights and channels', () => {
  const r = convertArena3dNetwork(ARENA)
  expect(r.layers).toBe(2)
  expect(r.network.split('\n').filter(Boolean)).toEqual([
    'Source\tTarget\tWeight\tType',
    'A\tB\t2\tppi',
    'B\tC\t1\tgenetic',
    'C\tD\t1\tppi',
  ])
  expect(r.annotation).toBe('L1\tA,B\nL2\tC,D\n')
})

test('hypergeometric upper tail', () => {
  // urn: 10 balls, 4 white, draw 3: P(X >= 3) = C(4,3)/C(10,3) = 4/120
  expect(hypergeomUpper(3, 10, 4, 3)).toBeCloseTo(4 / 120, 10)
  expect(hypergeomUpper(0, 10, 4, 3)).toBeCloseTo(1, 10)
  expect(hypergeomUpper(5, 10, 4, 3)).toBe(0)
})

test('Benjamini-Hochberg keeps order and is monotone', () => {
  // ranks: 0.01, 0.03, 0.04, 0.5 -> q = min over higher ranks of p * n / rank
  const q = benjaminiHochberg([0.01, 0.04, 0.03, 0.5])
  expect(q.map((x) => +x.toFixed(4))).toEqual([0.04, 0.0533, 0.0533, 0.5])
  expect(benjaminiHochberg([])).toEqual([])
})
