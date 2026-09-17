// @ts-nocheck
// ponytail: split verbatim from the former single-file script; typed one file at a time (remove @ts-nocheck as it lands)
import { labelPropagation } from './label_propagation'
import { leidenCommunities } from './leiden'
import { listSample, normaCols, normaLines, plural } from '../layouts/controls'
import { louvain } from '../wiring'
import { partitionStats, splitDisconnected } from '../welcome'
import { walktrap } from './walktrap'

/* ---------- Markov clustering ---------- */
function markovClustering(g, opts = {}) {
  const inflation = opts.inflation ?? 2
  const n = g.n
  const keep = opts.keep ?? 60 // largest entries kept per column
  const eps = 1e-5
  // column-stochastic matrix with self-loops; cols[j] = Map(i -> value)
  let cols = new Array(n)
  for (let j = 0; j < n; j++) {
    const a = g.adj[j]
    const w = 1 / (a.length + 1)
    const m = new Map([[j, w]])
    for (let k = 0; k < a.length; k++) m.set(a[k], w)
    cols[j] = m
  }
  const prune = (m) => {
    let entries = [...m].filter(([, v]) => v > eps)
    if (entries.length > keep) {
      entries.sort((x, y) => y[1] - x[1])
      entries = entries.slice(0, keep)
    }
    let s = 0
    entries.forEach((e) => {
      s += e[1]
    })
    return new Map(entries.map(([i, v]) => [i, v / s]))
  }
  for (let it = 0; it < 100; it++) {
    // expansion: M * M, column by column
    const next = new Array(n)
    let delta = 0
    for (let j = 0; j < n; j++) {
      const out = new Map()
      cols[j].forEach((vkj, k) => {
        cols[k].forEach((vik, i) => {
          out.set(i, (out.get(i) || 0) + vik * vkj)
        })
      })
      // inflation
      out.forEach((v, i) => out.set(i, Math.pow(v, inflation)))
      const pruned = prune(out)
      pruned.forEach((v, i) => {
        delta = Math.max(delta, Math.abs(v - (cols[j].get(i) || 0)))
      })
      next[j] = pruned
    }
    cols = next
    if (delta < 1e-6) break
  }
  // each node joins its attractor (strongest row in its column); attractors
  // that point at each other form one cluster
  const parent = Int32Array.from({ length: n }, (_, i) => i)
  const find = (x) => {
    while (parent[x] !== x) {
      parent[x] = parent[parent[x]]
      x = parent[x]
    }
    return x
  }
  const union = (a, b) => {
    const ra = find(a),
      rb = find(b)
    if (ra !== rb) parent[ra] = rb
  }
  for (let j = 0; j < n; j++) {
    let best = j,
      bestV = -1
    cols[j].forEach((v, i) => {
      if (v > bestV + 1e-12) {
        bestV = v
        best = i
      }
    })
    union(j, best)
  }
  const raw = new Int32Array(n)
  for (let i = 0; i < n; i++) raw[i] = find(i)
  return partitionStats(g, splitDisconnected(g, raw))
}

export function runCommunityAlgorithm(g, algo, params = {}) {
  if (algo === 'louvain') return louvain(g, params.resolution ?? 1)
  if (algo === 'leiden') return leidenCommunities(g, { resolution: params.resolution ?? 1 })
  if (algo === 'lpa') return labelPropagation(g)
  if (algo === 'walktrap') return walktrap(g, { steps: params.steps ?? 4 })
  if (algo === 'mcl') return markovClustering(g, { inflation: params.inflation ?? 2 })
  throw new Error('Unknown community algorithm: ' + algo)
}

/* ============================================================
   NUMERIC NODE VALUES
   Expression files may hold numbers (log2 fold changes, p-values, ...)
   in one or more named columns instead of colors. Each node then carries
   data('values') = { column: number | null }, and the chosen column is
   mapped to a diverging or sequential color scale and, optionally, to
   node size.
   ============================================================ */
export const VALUE_SCALES = {
  'div-rdbu': {
    label: 'Blue – white – red',
    diverging: true,
    stops: ['#2166ac', '#67a9cf', '#f7f7f7', '#ef8a62', '#b2182b'],
  },
  'div-puor': {
    label: 'Purple – white – orange',
    diverging: true,
    stops: ['#5e3c99', '#b2abd2', '#f7f7f7', '#fdb863', '#e66101'],
  },
  'div-prgn': {
    label: 'Purple – white – green',
    diverging: true,
    stops: ['#762a83', '#af8dc3', '#f7f7f7', '#7fbf7b', '#1b7837'],
  },
  'div-brbg': {
    label: 'Brown – white – teal',
    diverging: true,
    stops: ['#8c510a', '#d8b365', '#f5f5f5', '#5ab4ac', '#01665e'],
  },
  'seq-viridis': {
    label: 'Viridis',
    diverging: false,
    stops: ['#440154', '#3b528b', '#21918c', '#5ec962', '#fde725'],
  },
  'seq-magma': {
    label: 'Magma',
    diverging: false,
    stops: ['#000004', '#51127c', '#b73779', '#fc8961', '#fcfdbf'],
  },
  'seq-blues': {
    label: 'Light to dark blue',
    diverging: false,
    stops: ['#f7fbff', '#c6dbef', '#6baed6', '#2171b5', '#08306b'],
  },
  'seq-reds': {
    label: 'Light to dark red',
    diverging: false,
    stops: ['#fff5f0', '#fcbba1', '#fb6a4a', '#cb181d', '#67000d'],
  },
}

export const VALUE_TRANSFORMS = {
  none: { label: 'as they are', fn: (v) => v, title: (c) => c },
  neglog10: {
    label: '−log10 (for p-values)',
    fn: (v) => (v > 0 ? -Math.log10(v) : null),
    title: (c) => `−log10(${c})`,
  },
  abs: { label: 'absolute values', fn: (v) => Math.abs(v), title: (c) => `|${c}|` },
}

const NUMERIC_MISSING = new Set([
  '',
  'na',
  'n/a',
  'nan',
  'null',
  'none',
  '-',
  '.',
  '?',
  'inf',
  '-inf',
])

export function parseNumber(s) {
  const t = String(s ?? '').trim()
  if (NUMERIC_MISSING.has(t.toLowerCase())) return null
  // allow decimal commas when there is no dot
  const x = Number(/^[-+]?\d+,\d+(e[-+]?\d+)?$/i.test(t) ? t.replace(',', '.') : t)
  return Number.isFinite(x) ? x : undefined // undefined: not a number at all
}

// Does a table look numeric (node name, then numbers)? Used by detection and parsing.
export function numericShare(lines) {
  let rows = 0,
    numeric = 0
  lines.forEach((line) => {
    const cols = normaCols(line)
    if (cols.length < 2) return
    rows++
    if (
      cols.slice(1).every((c) => parseNumber(c) !== undefined) &&
      cols.slice(1).some((c) => parseNumber(c) !== null)
    )
      numeric++
  })
  return rows ? numeric / rows : 0
}

export function parseNumericValues(text) {
  const lines = normaLines(text).filter((l) => l.trim())
  let header = null
  const first = normaCols(lines[0] || '')
  if (first.length >= 2 && first.slice(1).some((c) => parseNumber(c) === undefined)) header = first
  const body = header ? lines.slice(1) : lines
  const width = Math.max(...body.map((l) => normaCols(l).length), header ? header.length : 2)
  const names = []
  for (let c = 1; c < width; c++) {
    const raw = header && header[c] ? header[c].trim() : width === 2 ? 'value' : `value ${c}`
    let name = raw || `value ${c}`
    while (names.includes(name)) name += '′'
    names.push(name)
  }
  const values = new Map()
  const bad = [],
    repeated = []
  body.forEach((line, i) => {
    const cols = normaCols(line)
    const node = (cols[0] || '').trim()
    if (!node) return
    const row = {}
    let any = false
    names.forEach((name, k) => {
      const v = parseNumber(cols[k + 1])
      if (v === undefined) {
        bad.push(i + 1 + (header ? 1 : 0))
        row[name] = null
      } else {
        row[name] = v
        if (v !== null) any = true
      }
    })
    if (values.has(node)) repeated.push(node)
    if (any || !values.has(node)) values.set(node, row)
  })
  if (!values.size)
    throw new Error(
      'No numeric values were found. Each line should be a node name, a tab, then one or more numbers.'
    )
  const notes = []
  if (header) notes.push(`Read the first line as column names: ${names.join(', ')}.`)
  if (bad.length)
    notes.push(
      `Read ${plural(bad.length, 'value')} that ${bad.length === 1 ? "isn't a number" : "aren't numbers"} as missing: line ${listSample([...new Set(bad)])}.`
    )
  if (repeated.length)
    notes.push(
      `Kept the last line for ${plural(repeated.length, 'repeated node')}: ${listSample(repeated)}.`
    )
  return {
    colors: new Map(),
    numeric: true,
    columns: names,
    values,
    notes,
    summary: `${plural(values.size, 'node')}, ${names.length === 1 ? `values (${names[0]})` : `${names.length} value columns`}`,
  }
}
