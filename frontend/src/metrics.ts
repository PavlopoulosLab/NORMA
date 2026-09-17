// @ts-nocheck
// ponytail: split verbatim from the former single-file script; typed one file at a time (remove @ts-nocheck as it lands)
import cytoscape from 'cytoscape'
import { S } from './state'
import { UNGROUPED, getUsedGroups } from './network_state'
import {
  anyEdgeDirected,
  directionMode,
  directionOf,
  edgeIsDirected,
  fr3dLayout,
} from './export/dialog'
import { applyBundleResult, applyLabelSizes } from './profiler'
import { cy } from './cy'
import { mulberry32 } from './sample_data'
import { scheduleLegend, scheduleSeparation, valueForSize } from './clustering/mapping'
import { updateContextInfo } from './recording'

/* ---------- centrality metrics ---------- */

// Computes a raw value per node for the given metric, over either the whole
// graph or just the currently active (checked) edge channels.
// directed = true: out-neighbours only for directed edges (undirected
// edges still count both ways)
function buildAdjacency(activeEdges, nodes, directed = false) {
  const adjSet = {}
  nodes.forEach((n) => {
    adjSet[n.id()] = new Set()
  })
  activeEdges.forEach((e) => {
    const s = e.data('source'),
      t = e.data('target')
    if (s === t || !adjSet[s] || !adjSet[t]) return
    adjSet[s].add(t)
    if (!directed || !edgeIsDirected(e)) adjSet[t].add(s)
  })
  const adj = {}
  Object.keys(adjSet).forEach((k) => {
    adj[k] = [...adjSet[k]]
  })
  return adj
}

// Runs a single unweighted BFS-based pass (Brandes' algorithm) to get both
// betweenness and closeness together, since they share the same shortest-path work.
function bfsCentralities(nodeIds, adj, directed = false) {
  const betweenness = {}
  const closenessSum = {}
  const reached = {}
  nodeIds.forEach((id) => {
    betweenness[id] = 0
    closenessSum[id] = 0
    reached[id] = 0
  })

  nodeIds.forEach((s) => {
    const P = {},
      sigma = {},
      d = {}
    nodeIds.forEach((v) => {
      P[v] = []
      sigma[v] = 0
      d[v] = -1
    })
    sigma[s] = 1
    d[s] = 0
    const S = []
    const queue = [s]
    while (queue.length) {
      const v = queue.shift()
      S.push(v)
      ;(adj[v] || []).forEach((w) => {
        if (d[w] < 0) {
          d[w] = d[v] + 1
          queue.push(w)
        }
        if (d[w] === d[v] + 1) {
          sigma[w] += sigma[v]
          P[w].push(v)
        }
      })
    }
    nodeIds.forEach((v) => {
      if (d[v] > 0) {
        closenessSum[s] += d[v]
        reached[s]++
      }
    })
    const delta = {}
    nodeIds.forEach((v) => (delta[v] = 0))
    while (S.length) {
      const w = S.pop()
      P[w].forEach((v) => {
        delta[v] += (sigma[v] / sigma[w]) * (1 + delta[w])
      })
      if (w !== s) betweenness[w] += delta[w]
    }
  })
  // each shortest path is counted from both endpoints in an undirected graph
  if (!directed)
    nodeIds.forEach((v) => {
      betweenness[v] /= 2
    })

  const closeness = {}
  nodeIds.forEach((v) => {
    closeness[v] = closenessSum[v] > 0 ? reached[v] / closenessSum[v] : 0
  })

  return { betweenness, closeness }
}

// Self-contained (no reliance on cytoscape's own centrality helpers, which
// resolve neighborhoods against the full core graph rather than a given
// subset) so "compute on selected channels only" is honored exactly.
/* ============================================================
   THE SHOWN PART OF A VIEW
   NORMA works on what is ticked: nodes in at least one ticked group,
   and edges of ticked channels whose two ends are shown. Layouts, node
   sizing, node statistics, the Profiler, Compare, search, bundling,
   group shading and (by default) exports all use this part only.
   ============================================================ */
export function shownNodes() {
  return cy.nodes().filter((n) => !n.hasClass('hidden-group'))
}

export function shownEdges(channelOnly = true) {
  return cy
    .edges()
    .filter(
      (e) =>
        (!channelOnly || S.activeTypes.has(e.data('type'))) &&
        !e.source().hasClass('hidden-group') &&
        !e.target().hasClass('hidden-group')
    )
}

export function exportShownOnly() {
  const el = document.getElementById('exportShownOnly')
  return !el || el.checked
}

export function exportNodes() {
  return exportShownOnly() ? shownNodes() : cy.nodes()
}

export function exportEdges() {
  return exportShownOnly() ? shownEdges(true) : cy.edges()
}

export function computeMetricValues(metric, channelOnly) {
  const activeEdges = shownEdges(channelOnly)
  const nodes = shownNodes()
  const nodeIds = nodes.map((n) => n.id())
  const values = {}

  if (metric === 'value') {
    nodes.forEach((n) => {
      values[n.id()] = valueForSize(n)
    })
    return values
  }
  if (metric === 'degree' || metric === 'indegree' || metric === 'outdegree') {
    nodeIds.forEach((id) => {
      values[id] = 0
    })
    const add = (id) => {
      if (id in values) values[id]++
    }
    activeEdges.forEach((e) => {
      const s = e.data('source'),
        t = e.data('target')
      if (metric === 'degree' || !edgeIsDirected(e)) {
        add(s)
        add(t)
        return
      }
      if (metric === 'outdegree') add(s)
      else add(t)
    })
    return values
  }

  // shortest paths follow edge direction when the shown part has any
  // directed edge; clustering always ignores direction
  const directed = metric !== 'clustering' && anyEdgeDirected(activeEdges)
  const adj = buildAdjacency(activeEdges, nodes, directed)

  if (metric === 'clustering') {
    nodeIds.forEach((id) => {
      const neigh = adj[id]
      const k = neigh.length
      if (k < 2) {
        values[id] = 0
        return
      }
      let links = 0
      for (let i = 0; i < neigh.length; i++) {
        for (let j = i + 1; j < neigh.length; j++) {
          if (adj[neigh[i]].includes(neigh[j])) links++
        }
      }
      values[id] = links / ((k * (k - 1)) / 2)
    })
    return values
  }

  if (metric === 'betweenness' || metric === 'closeness') {
    const { betweenness, closeness } = bfsCentralities(nodeIds, adj, directed)
    return metric === 'betweenness' ? betweenness : closeness
  }

  return values
}

// Cache of all four metrics computed on the full graph, used by the node info
// popup. Computed lazily (on first node click after a load) rather than
// eagerly on every data load: betweenness/closeness are O(V*(V+E)), and with
// networks up to several thousand nodes that eager pass would stall the UI
// even when the person never opens the info popup.
let fullMetricsCache = null

export function invalidateFullMetrics() {
  fullMetricsCache = null
}

export function ensureFullMetrics() {
  if (!fullMetricsCache) {
    fullMetricsCache = {
      degree: computeMetricValues('degree', true),
      indegree: computeMetricValues('indegree', true),
      outdegree: computeMetricValues('outdegree', true),
      betweenness: computeMetricValues('betweenness', true),
      closeness: computeMetricValues('closeness', true),
      clustering: computeMetricValues('clustering', true),
    }
  }
  return fullMetricsCache
}

export function applyNodeSizing() {
  const metric = document.getElementById('sizeMetric').value
  const scale = parseFloat(document.getElementById('nodeScale').value) || 1
  document.getElementById('nodeScaleValue').textContent = scale.toFixed(2).replace(/0$/, '') + '×'

  if (metric === 'fixed') {
    cy.batch(() => cy.nodes().forEach((n) => n.data('size', n.data('baseSize') * scale)))
    applyLabelSizes()
    if (typeof applyBundleResult === 'function') applyBundleResult()
    return
  }

  const channelOnly = document.getElementById('sizeChannelOnly').checked
  const minPx = parseFloat(document.getElementById('sizeMin').value) || 22
  const maxPx = parseFloat(document.getElementById('sizeMax').value) || 74
  const values = computeMetricValues(metric, channelOnly)
  const nums = Object.values(values)
  if (!nums.length) return
  const lo = Math.min(...nums),
    hi = Math.max(...nums)

  cy.nodes().forEach((n) => {
    const v = values[n.id()] || 0
    const size = hi === lo ? (minPx + maxPx) / 2 : minPx + ((v - lo) / (hi - lo)) * (maxPx - minPx)
    n.data('size', size * scale)
  })
  applyLabelSizes()
  if (typeof applyBundleResult === 'function') applyBundleResult()
}

export function updateStats() {
  document.getElementById('statNodes').textContent = cy.nodes().length.toLocaleString()
  document.getElementById('statEdges').textContent = cy.edges().length.toLocaleString()
  const types = new Set(cy.edges().map((e) => e.data('type')))
  document.getElementById('statTypes').textContent = types.size
  const groups = getUsedGroups().filter((g) => g !== UNGROUPED)
  const active = groups.filter((g) => S.activeGroups.has(g)).length
  document.getElementById('statGroups').textContent =
    active === groups.length ? groups.length : `${active}/${groups.length}`
  const word = (id, n, one, many) => {
    document.getElementById(id).textContent = n === 1 ? one : many
  }
  word('statNodesWord', cy.nodes().length, 'node', 'nodes')
  word('statEdgesWord', cy.edges().length, 'edge', 'edges')
  word('statTypesWord', types.size, 'channel', 'channels')
  word('statGroupsWord', groups.length, 'group', 'groups')
  const dirEl = document.getElementById('statDirection')
  dirEl.classList.remove('stat-dir-off')
  if (typeof directionOf === 'function' && cy.edges().length) {
    const dir = directionOf(cy.edges())
    const dataDirected = cy.edges().filter((e) => e.data('directed')).length
    if (dir === 'undirected' && dataDirected && directionMode() === 'undirected') {
      // the data has direction but the view ignores it (the default)
      dirEl.hidden = false
      dirEl.textContent = 'direction ignored'
      dirEl.classList.add('stat-dir-off')
      dirEl.title = `${dataDirected.toLocaleString()} edges are marked as directed in the data. Click to show them as directed (Edges → Direction: As in the data).`
    } else {
      dirEl.hidden = dir === 'undirected'
      dirEl.textContent = dir === 'mixed' ? 'mixed direction' : 'directed'
      dirEl.title =
        dir === 'mixed'
          ? 'Some edges are directed, some are not'
          : 'Edges point from source to target'
    }
  } else dirEl.hidden = true
  if (typeof updateContextInfo === 'function') updateContextInfo()
  if (typeof scheduleLegend === 'function') {
    scheduleLegend()
    scheduleSeparation()
  }
}

// Per-layout-name option tweaks shared between the normal single-graph
// layout and the headless sub-layouts used by the group strategies.
export function layoutOptsFor(name) {
  const opts = { name }
  if (name === 'concentric') {
    opts.concentric = (n) => n.degree()
    opts.levelWidth = () => 1
  }
  if (name === 'breadthfirst') {
    opts.directed = typeof anyEdgeDirected === 'function' && anyEdgeDirected(shownEdges(true))
    opts.spacingFactor = 1.1
  }
  if (name === 'cose') {
    opts.idealEdgeLength = 90
    opts.nodeRepulsion = 6000
    opts.gravity = 0.4
  }
  return opts
}

// Above this size a network loads with a grid layout instead of force-directed.
export const LARGE_NETWORK_NODES = 700

/* ============================================================
   FORCE-DIRECTED EDGE BUNDLING
   Holten D, van Wijk JJ (2009) Force-directed edge bundling for graph
   visualization. Computer Graphics Forum 28(3):983-990.
   Each edge becomes a polyline whose inner points are pulled toward the
   matching points of "compatible" edges (similar direction, length,
   position and mutual visibility) and held in shape by springs. The
   number of points doubles each cycle while the step size halves.
   Input: [{sx, sy, tx, ty}] in model coordinates. Output: for each edge,
   a flat array [x1, y1, x2, y2, ...] of its inner points, or null.
   ============================================================ */
// Progress of long computations (layouts, edge bundling), reported from
// inside their loops as a fraction of the part in workRange. In the worker,
// workSink posts it to the page; on the page itself it is not used.
var workSink = null,
  workLast = 0

export function workStep(f) {
  if (!workSink) return
  const now = Date.now()
  if (now - workLast < 80) return
  workLast = now
  workSink(S.workRange[0] + (S.workRange[1] - S.workRange[0]) * Math.max(0, Math.min(1, f)))
}

function fdebBundle(segs, opts) {
  const E = segs.length
  const threshold = opts.threshold
  const K = 0.1,
    EPS = 1e-6
  // 4 cycles give 8 inner points per edge; Cytoscape stops drawing
  // unbundled-bezier edges with more than 8 control points (tested)
  const cycles = 4
  let iterations = opts.iterations
  let S = 0.1
  const COMPAT_CAP = 150

  const len = new Float64Array(E),
    mx = new Float64Array(E),
    my = new Float64Array(E)
  const dx = new Float64Array(E),
    dy = new Float64Array(E)
  for (let i = 0; i < E; i++) {
    const s = segs[i]
    dx[i] = s.tx - s.sx
    dy[i] = s.ty - s.sy
    len[i] = Math.hypot(dx[i], dy[i])
    mx[i] = (s.sx + s.tx) / 2
    my[i] = (s.sy + s.ty) / 2
  }
  // how much of segment q is "visible" from segment p (projection overlap)
  function visibility(p, q) {
    const s = segs[p],
      t = segs[q]
    const L2 = len[p] * len[p]
    const u0 = ((t.sx - s.sx) * dx[p] + (t.sy - s.sy) * dy[p]) / L2
    const u1 = ((t.tx - s.sx) * dx[p] + (t.ty - s.sy) * dy[p]) / L2
    const i0x = s.sx + u0 * dx[p],
      i0y = s.sy + u0 * dy[p]
    const i1x = s.sx + u1 * dx[p],
      i1y = s.sy + u1 * dy[p]
    const il = Math.hypot(i1x - i0x, i1y - i0y)
    if (il < EPS) return 0
    const imx = (i0x + i1x) / 2,
      imy = (i0y + i1y) / 2
    return Math.max(0, 1 - (2 * Math.hypot(mx[p] - imx, my[p] - imy)) / il)
  }

  // compatible partners per edge; `flip` marks partners running the other way
  const partners = Array.from({ length: E }, () => [])
  for (let p = 0; p < E; p++) {
    if ((p & 31) === 0) workStep(0.2 * (1 - ((E - p) * (E - p)) / (E * E)))
    if (len[p] < EPS) continue
    for (let q = p + 1; q < E; q++) {
      if (len[q] < EPS) continue
      const dot = dx[p] * dx[q] + dy[p] * dy[q]
      const angle = Math.abs(dot) / (len[p] * len[q])
      if (angle < threshold) continue
      const lavg = (len[p] + len[q]) / 2
      const scale = 2 / (lavg / Math.min(len[p], len[q]) + Math.max(len[p], len[q]) / lavg)
      let c = angle * scale
      if (c < threshold) continue
      c *= lavg / (lavg + Math.hypot(mx[p] - mx[q], my[p] - my[q]))
      if (c < threshold) continue
      c *= Math.min(visibility(p, q), visibility(q, p))
      if (c < threshold) continue
      const flip = dot < 0
      partners[p].push({ e: q, c, flip })
      partners[q].push({ e: p, c, flip })
      if (partners[p].length > COMPAT_CAP * 4) {
        partners[p].sort((a, b) => b.c - a.c)
        partners[p].length = COMPAT_CAP
      }
      if (partners[q].length > COMPAT_CAP * 4) {
        partners[q].sort((a, b) => b.c - a.c)
        partners[q].length = COMPAT_CAP
      }
    }
  }
  partners.forEach((list) => {
    if (list.length > COMPAT_CAP) {
      list.sort((a, b) => b.c - a.c)
      list.length = COMPAT_CAP
    }
  })

  // polylines, stored as [x0, y0, ..., xn, yn] including both endpoints
  let P = 1
  let pts = segs.map((s) =>
    Float64Array.from([s.sx, s.sy, (s.sx + s.tx) / 2, (s.sy + s.ty) / 2, s.tx, s.ty])
  )

  function resample(line, segments) {
    const n = line.length / 2
    let total = 0
    for (let i = 1; i < n; i++)
      total += Math.hypot(line[2 * i] - line[2 * i - 2], line[2 * i + 1] - line[2 * i - 1])
    const out = new Float64Array((segments + 1) * 2)
    out[0] = line[0]
    out[1] = line[1]
    out[segments * 2] = line[(n - 1) * 2]
    out[segments * 2 + 1] = line[(n - 1) * 2 + 1]
    if (total < EPS) {
      for (let k = 1; k < segments; k++) {
        out[2 * k] = line[0]
        out[2 * k + 1] = line[1]
      }
      return out
    }
    const step = total / segments
    let seg = 1,
      acc = 0
    for (let k = 1; k < segments; k++) {
      const target = step * k
      while (seg < n) {
        const sl = Math.hypot(
          line[2 * seg] - line[2 * seg - 2],
          line[2 * seg + 1] - line[2 * seg - 1]
        )
        if (acc + sl >= target || seg === n - 1) {
          const f = sl > EPS ? (target - acc) / sl : 0
          out[2 * k] = line[2 * seg - 2] + (line[2 * seg] - line[2 * seg - 2]) * f
          out[2 * k + 1] = line[2 * seg - 1] + (line[2 * seg + 1] - line[2 * seg - 1]) * f
          break
        }
        acc += sl
        seg++
      }
    }
    return out
  }

  // work per iteration grows with the points per edge (2, 4, 8, 16)
  let planned = 0,
    spent = 0
  for (let c = 0, its = opts.iterations; c < cycles; c++) {
    if (c > 0) its = Math.max(3, Math.round((its * 2) / 3))
    planned += its * 2 ** c
  }
  for (let cycle = 0; cycle < cycles; cycle++) {
    if (cycle > 0) {
      P *= 2
      S /= 2
      iterations = Math.max(3, Math.round((iterations * 2) / 3))
      pts = pts.map((line) => resample(line, P + 1))
    }
    const nPts = P + 2 // including endpoints
    for (let it = 0; it < iterations; it++) {
      workStep(0.2 + (0.8 * spent) / planned)
      spent += 2 ** cycle
      const next = pts.map((line) => Float64Array.from(line))
      for (let e = 0; e < E; e++) {
        const line = pts[e]
        if (len[e] < EPS) continue
        const kP = K / (len[e] * (P + 1))
        const plist = partners[e]
        for (let i = 1; i < nPts - 1; i++) {
          const x = line[2 * i],
            y = line[2 * i + 1]
          let fx = kP * (line[2 * i - 2] - x + line[2 * i + 2] - x)
          let fy = kP * (line[2 * i - 1] - y + line[2 * i + 3] - y)
          for (let j = 0; j < plist.length; j++) {
            const other = pts[plist[j].e]
            const oi = plist[j].flip ? nPts - 1 - i : i
            const ex = other[2 * oi] - x,
              ey = other[2 * oi + 1] - y
            const d = Math.hypot(ex, ey)
            if (d > EPS) {
              fx += ex / d
              fy += ey / d
            }
          }
          next[e][2 * i] = x + S * fx
          next[e][2 * i + 1] = y + S * fy
        }
      }
      pts = next
    }
  }
  return pts.map((line, e) => (len[e] < EPS ? null : Array.from(line.slice(2, line.length - 2))))
}

/* ============================================================
   WEIGHTED FRUCHTERMAN-REINGOLD
   NORMA computes its layouts with igraph, where layout_with_fr
   multiplies the spring attraction along an edge by that edge's weight.
   The group strategies depend on this: they work by adding heavy or
   light edges. Cytoscape has no equivalent, so this is a direct
   implementation with igraph's defaults: 500 iterations, a start
   temperature of sqrt(n)/10 cooling linearly to zero, repulsion k²/d and
   attraction w·d²/k with k = 1. It is seeded (NORMA uses set.seed(123)),
   so the same input always gives the same picture.
   Each connected component is laid out on its own and the components
   are then packed side by side; otherwise small components drift
   arbitrarily far from the main one.
   ============================================================ */
export const FR_SEED = 123

function connectedComponents(n, edges) {
  const parent = Int32Array.from({ length: n }, (_, i) => i)
  const find = (i) => {
    while (parent[i] !== i) {
      parent[i] = parent[parent[i]]
      i = parent[i]
    }
    return i
  }
  edges.forEach((e) => {
    const a = find(e.s),
      b = find(e.t)
    if (a !== b) parent[a] = b
  })
  const byRoot = new Map()
  for (let i = 0; i < n; i++) {
    const r = find(i)
    if (!byRoot.has(r)) byRoot.set(r, [])
    byRoot.get(r).push(i)
  }
  return [...byRoot.values()]
}

// n nodes (0..n-1), edges [{s, t, w}] -> { x: Float64Array, y: Float64Array }
function frComponent(n, edges, rand) {
  const X = new Float64Array(n),
    Y = new Float64Array(n)
  if (n === 1) return { x: X, y: Y }
  const side = Math.sqrt(n)
  for (let i = 0; i < n; i++) {
    X[i] = (rand() - 0.5) * side
    Y[i] = (rand() - 0.5) * side
  }
  const iterations = n > 3000 ? 150 : n > 1000 ? 300 : 500
  const temp0 = Math.sqrt(n) / 10
  const DX = new Float64Array(n),
    DY = new Float64Array(n)
  const useGrid = n > 1500
  const CELL = 2 // FR's grid variant: repulsion only within 2k

  for (let it = 0; it < iterations; it++) {
    if ((it & 3) === 0) workStep(it / iterations)
    DX.fill(0)
    DY.fill(0)
    if (!useGrid) {
      for (let i = 0; i < n; i++) {
        for (let j = i + 1; j < n; j++) {
          let dx = X[i] - X[j],
            dy = Y[i] - Y[j]
          let d2 = dx * dx + dy * dy
          if (d2 < 1e-9) {
            dx = (rand() - 0.5) * 1e-3
            dy = (rand() - 0.5) * 1e-3
            d2 = dx * dx + dy * dy
          }
          const f = 1 / d2 // (k²/d) along the unit vector
          DX[i] += dx * f
          DY[i] += dy * f
          DX[j] -= dx * f
          DY[j] -= dy * f
        }
      }
    } else {
      const grid = new Map()
      for (let i = 0; i < n; i++) {
        const key = Math.floor(X[i] / CELL) + ',' + Math.floor(Y[i] / CELL)
        let cell = grid.get(key)
        if (!cell) grid.set(key, (cell = []))
        cell.push(i)
      }
      for (let i = 0; i < n; i++) {
        const cx = Math.floor(X[i] / CELL),
          cyy = Math.floor(Y[i] / CELL)
        for (let gx = cx - 1; gx <= cx + 1; gx++) {
          for (let gy = cyy - 1; gy <= cyy + 1; gy++) {
            const cell = grid.get(gx + ',' + gy)
            if (!cell) continue
            for (const j of cell) {
              if (j <= i) continue
              let dx = X[i] - X[j],
                dy = Y[i] - Y[j]
              let d2 = dx * dx + dy * dy
              if (d2 > CELL * CELL * 4) continue
              if (d2 < 1e-9) {
                dx = (rand() - 0.5) * 1e-3
                dy = (rand() - 0.5) * 1e-3
                d2 = dx * dx + dy * dy
              }
              const f = 1 / d2
              DX[i] += dx * f
              DY[i] += dy * f
              DX[j] -= dx * f
              DY[j] -= dy * f
            }
          }
        }
      }
    }
    for (const e of edges) {
      const dx = X[e.s] - X[e.t],
        dy = Y[e.s] - Y[e.t]
      const d = Math.sqrt(dx * dx + dy * dy)
      const f = e.w * d // (w·d²/k) along the unit vector
      DX[e.s] -= dx * f
      DY[e.s] -= dy * f
      DX[e.t] += dx * f
      DY[e.t] += dy * f
    }
    const temp = temp0 * (1 - it / iterations)
    for (let i = 0; i < n; i++) {
      const len = Math.sqrt(DX[i] * DX[i] + DY[i] * DY[i])
      if (len > temp) {
        DX[i] *= temp / len
        DY[i] *= temp / len
      }
      X[i] += DX[i]
      Y[i] += DY[i]
    }
  }
  return { x: X, y: Y }
}

// Shelf-packs laid-out components, largest first, into a roughly square block.
function packComponents(parts) {
  const PAD = 1.5
  parts.forEach((p) => {
    let x1 = Infinity,
      y1 = Infinity,
      x2 = -Infinity,
      y2 = -Infinity
    for (let i = 0; i < p.x.length; i++) {
      x1 = Math.min(x1, p.x[i])
      x2 = Math.max(x2, p.x[i])
      y1 = Math.min(y1, p.y[i])
      y2 = Math.max(y2, p.y[i])
    }
    p.x1 = x1
    p.y1 = y1
    p.w = x2 - x1 + PAD
    p.h = y2 - y1 + PAD
  })
  parts.sort((a, b) => b.w * b.h - a.w * a.h)
  const area = parts.reduce((s, p) => s + p.w * p.h, 0)
  const rowWidth = Math.max(parts[0].w, Math.sqrt(area) * 1.2)
  let cx = 0,
    cyy = 0,
    rowH = 0
  parts.forEach((p) => {
    if (cx > 0 && cx + p.w > rowWidth) {
      cx = 0
      cyy += rowH
      rowH = 0
    }
    p.ox = cx - p.x1
    p.oy = cyy - p.y1
    cx += p.w
    rowH = Math.max(rowH, p.h)
  })
}

// ids: string[], edges: [{source, target, weight}] -> { id: {x, y} } in FR units
function frLayout(ids, edges) {
  const index = new Map(ids.map((id, i) => [id, i]))
  const all = []
  edges.forEach((e) => {
    const s = index.get(e.source),
      t = index.get(e.target)
    if (s === undefined || t === undefined || s === t) return
    const w = Number.isFinite(e.weight) && e.weight > 0 ? e.weight : 1
    all.push({ s, t, w })
  })
  const rand = mulberry32(FR_SEED)
  const comps = connectedComponents(ids.length, all)
  const compOf = new Int32Array(ids.length)
  const localIdx = new Int32Array(ids.length)
  comps.forEach((members, c) =>
    members.forEach((g, i) => {
      compOf[g] = c
      localIdx[g] = i
    })
  )
  const compEdges = comps.map(() => [])
  all.forEach((e) => compEdges[compOf[e.s]].push({ s: localIdx[e.s], t: localIdx[e.t], w: e.w }))
  let laidOut = 0
  const parts = comps.map((members, c) => {
    S.workRange = [laidOut / ids.length, (laidOut + members.length) / ids.length]
    laidOut += members.length
    return { members, ...frComponent(members.length, compEdges[c], rand) }
  })
  S.workRange = [0, 1]
  packComponents(parts)
  const out = {}
  parts.forEach((p) =>
    p.members.forEach((g, i) => {
      out[ids[g]] = { x: p.x[i] + p.ox, y: p.y[i] + p.oy }
    })
  )
  return out
}

/* ============================================================
   DISTANCE-BASED LAYOUTS
   kkLayout      Kamada & Kawai (1989): springs between all pairs, with
                 lengths equal to graph distances (hops), relaxed node by
                 node with Newton-Raphson steps.
   stressLayout  Stress majorization (Gansner, Koren & North 2004):
                 pivot MDS start, then weighted stress minimisation
                 (weights 1/d^2).
   Both lay out each connected component separately and pack them, like
   the weighted Fruchterman-Reingold layout, and run in the worker.
   Edge weights are not used: distances are shortest paths in hops.
   ============================================================ */
const DIST_LAYOUT_LIMIT = 2500
// nodes per component

// all-pairs hop distances of one component (local indices)
function componentDistances(n, adj) {
  const D = new Float64Array(n * n)
  const q = new Int32Array(n)
  for (let s = 0; s < n; s++) {
    const row = s * n
    for (let i = 0; i < n; i++) D[row + i] = -1
    D[row + s] = 0
    let head = 0,
      tail = 0
    q[tail++] = s
    while (head < tail) {
      const v = q[head++]
      const dv = D[row + v]
      const a = adj[v]
      for (let k = 0; k < a.length; k++) {
        const w = a[k]
        if (D[row + w] < 0) {
          D[row + w] = dv + 1
          q[tail++] = w
        }
      }
    }
  }
  return D
}

// Pivot MDS (Brandes & Pich 2006): a quick, good starting layout.
function pivotMDS(n, D) {
  const X = new Float64Array(n),
    Y = new Float64Array(n)
  if (n < 3) {
    for (let i = 0; i < n; i++) {
      X[i] = i
    }
    return { X, Y }
  }
  const k = Math.min(50, n)
  const pivots = [0]
  const minD = new Float64Array(n).fill(Infinity)
  for (let p = 1; p < k; p++) {
    const last = pivots[p - 1]
    let best = 0,
      bestD = -1
    for (let i = 0; i < n; i++) {
      minD[i] = Math.min(minD[i], D[last * n + i])
      if (minD[i] > bestD) {
        bestD = minD[i]
        best = i
      }
    }
    pivots.push(best)
  }
  // C: n x k squared distances, double centered
  const C = new Float64Array(n * k)
  const colMean = new Float64Array(k),
    rowMean = new Float64Array(n)
  let total = 0
  for (let i = 0; i < n; i++)
    for (let j = 0; j < k; j++) {
      const v = Math.pow(D[pivots[j] * n + i], 2)
      C[i * k + j] = v
      colMean[j] += v
      rowMean[i] += v
      total += v
    }
  for (let j = 0; j < k; j++) colMean[j] /= n
  for (let i = 0; i < n; i++) rowMean[i] /= k
  total /= n * k
  for (let i = 0; i < n; i++)
    for (let j = 0; j < k; j++) {
      C[i * k + j] = -0.5 * (C[i * k + j] - rowMean[i] - colMean[j] + total)
    }
  // top two eigenvectors of C^T C by power iteration
  const CtC = new Float64Array(k * k)
  for (let a = 0; a < k; a++)
    for (let b = a; b < k; b++) {
      let s = 0
      for (let i = 0; i < n; i++) s += C[i * k + a] * C[i * k + b]
      CtC[a * k + b] = s
      CtC[b * k + a] = s
    }
  const vecs = []
  for (let e = 0; e < 2; e++) {
    let v = Float64Array.from({ length: k }, (_, i) => Math.sin(i * (e + 1) + 1))
    for (let it = 0; it < 100; it++) {
      const w = new Float64Array(k)
      for (let a = 0; a < k; a++) {
        let s = 0
        for (let b = 0; b < k; b++) s += CtC[a * k + b] * v[b]
        w[a] = s
      }
      vecs.forEach((u) => {
        let d = 0
        for (let a = 0; a < k; a++) d += w[a] * u[a]
        for (let a = 0; a < k; a++) w[a] -= d * u[a]
      })
      let norm = 0
      for (let a = 0; a < k; a++) norm += w[a] * w[a]
      norm = Math.sqrt(norm) || 1
      for (let a = 0; a < k; a++) w[a] /= norm
      v = w
    }
    vecs.push(v)
  }
  for (let i = 0; i < n; i++) {
    let x = 0,
      y = 0
    for (let j = 0; j < k; j++) {
      x += C[i * k + j] * vecs[0][j]
      y += C[i * k + j] * vecs[1][j]
    }
    X[i] = x
    Y[i] = y
  }
  // scale to hop units
  let spread = 0,
    count = 0
  for (let i = 0; i < n; i++)
    for (let j = i + 1; j < Math.min(n, i + 20); j++) {
      const d = Math.hypot(X[i] - X[j], Y[i] - Y[j])
      if (d > 0) {
        spread += D[i * n + j] / d
        count++
      }
    }
  const s = count ? spread / count : 1
  for (let i = 0; i < n; i++) {
    X[i] *= s
    Y[i] *= s
  }
  // break exact ties
  for (let i = 0; i < n; i++) {
    X[i] += 1e-3 * Math.sin(i * 12.9898)
    Y[i] += 1e-3 * Math.cos(i * 78.233)
  }
  return { X, Y }
}

function kkComponent(n, adj) {
  const X = new Float64Array(n),
    Y = new Float64Array(n)
  if (n === 1) return { x: X, y: Y }
  const D = componentDistances(n, adj)
  // start on a circle, as igraph does
  let maxD = 0
  for (let i = 0; i < D.length; i++) if (D[i] > maxD) maxD = D[i]
  for (let i = 0; i < n; i++) {
    X[i] = (Math.cos((2 * Math.PI * i) / n) * maxD) / 2
    Y[i] = (Math.sin((2 * Math.PI * i) / n) * maxD) / 2
  }
  const gx = new Float64Array(n),
    gy = new Float64Array(n)
  const term = (m, i) => {
    // gradient contribution of pair (m, i) to m
    const d = D[m * n + i]
    if (d <= 0) return [0, 0]
    const dx = X[m] - X[i],
      dy = Y[m] - Y[i]
    const dist = Math.sqrt(dx * dx + dy * dy) || 1e-9
    const k = 1 / (d * d)
    return [k * (dx - (d * dx) / dist), k * (dy - (d * dy) / dist)]
  }
  for (let m = 0; m < n; m++)
    for (let i = 0; i < n; i++) {
      if (i === m) continue
      const [a, b] = term(m, i)
      gx[m] += a
      gy[m] += b
    }
  const maxIter = Math.min(50 * n, 40000)
  const eps = 1e-4
  for (let it = 0; it < maxIter; it++) {
    if ((it & 63) === 0) workStep(it / maxIter)
    let m = -1,
      best = eps
    for (let i = 0; i < n; i++) {
      const g = gx[i] * gx[i] + gy[i] * gy[i]
      if (g > best) {
        best = g
        m = i
      }
    }
    if (m < 0) break
    // remove m's old contributions from everybody else
    for (let i = 0; i < n; i++) {
      if (i === m) continue
      const [a, b] = term(i, m)
      gx[i] -= a
      gy[i] -= b
    }
    // Newton-Raphson on node m
    for (let step = 0; step < 20; step++) {
      let dxx = 0,
        dyy = 0,
        dxy = 0,
        ex = 0,
        ey = 0
      for (let i = 0; i < n; i++) {
        if (i === m) continue
        const d = D[m * n + i]
        if (d <= 0) continue
        const k = 1 / (d * d)
        const dx = X[m] - X[i],
          dy = Y[m] - Y[i]
        const dist2 = dx * dx + dy * dy || 1e-12
        const dist = Math.sqrt(dist2)
        const dist3 = dist2 * dist
        ex += k * (dx - (d * dx) / dist)
        ey += k * (dy - (d * dy) / dist)
        dxx += k * (1 - (d * dy * dy) / dist3)
        dyy += k * (1 - (d * dx * dx) / dist3)
        dxy += k * ((d * dx * dy) / dist3)
      }
      const det = dxx * dyy - dxy * dxy
      if (Math.abs(det) < 1e-12) break
      const sx = (-ex * dyy + ey * dxy) / det
      const sy = (-ey * dxx + ex * dxy) / det
      X[m] += sx
      Y[m] += sy
      gx[m] = ex
      gy[m] = ey
      if (sx * sx + sy * sy < 1e-8) break
    }
    // recompute m's gradient and add its new contributions to everybody
    gx[m] = 0
    gy[m] = 0
    for (let i = 0; i < n; i++) {
      if (i === m) continue
      const [a, b] = term(m, i)
      gx[m] += a
      gy[m] += b
      const [c, e] = term(i, m)
      gx[i] += c
      gy[i] += e
    }
  }
  return { x: X, y: Y }
}

function stressComponent(n, adj) {
  if (n === 1) return { x: new Float64Array(1), y: new Float64Array(1) }
  const D = componentDistances(n, adj)
  const { X, Y } = pivotMDS(n, D)
  const iters = Math.max(20, Math.min(300, Math.floor(4e7 / (n * n))))
  let prev = Infinity
  for (let it = 0; it < iters; it++) {
    workStep(it / iters)
    let stress = 0
    for (let i = 0; i < n; i++) {
      let sx = 0,
        sy = 0,
        sw = 0
      for (let j = 0; j < n; j++) {
        if (j === i) continue
        const d = D[i * n + j]
        if (d <= 0) continue
        const w = 1 / (d * d)
        const dx = X[i] - X[j],
          dy = Y[i] - Y[j]
        const dist = Math.sqrt(dx * dx + dy * dy) || 1e-9
        sx += w * (X[j] + (d * dx) / dist)
        sy += w * (Y[j] + (d * dy) / dist)
        sw += w
        stress += w * (dist - d) * (dist - d)
      }
      if (sw) {
        X[i] = sx / sw
        Y[i] = sy / sw
      }
    }
    if (Math.abs(prev - stress) / (stress || 1) < 1e-5) break
    prev = stress
  }
  return { x: X, y: Y }
}

function distanceLayout(ids, edges, kind) {
  const index = new Map(ids.map((id, i) => [id, i]))
  const all = []
  edges.forEach((e) => {
    const s = index.get(e.source),
      t = index.get(e.target)
    if (s === undefined || t === undefined || s === t) return
    all.push({ s, t, w: 1 })
  })
  const comps = connectedComponents(ids.length, all)
  const compOf = new Int32Array(ids.length),
    localIdx = new Int32Array(ids.length)
  comps.forEach((members, c) =>
    members.forEach((g, i) => {
      compOf[g] = c
      localIdx[g] = i
    })
  )
  const adjs = comps.map((m) => m.map(() => []))
  all.forEach((e) => {
    const c = compOf[e.s]
    adjs[c][localIdx[e.s]].push(localIdx[e.t])
    adjs[c][localIdx[e.t]].push(localIdx[e.s])
  })
  let laidOut = 0
  const parts = comps.map((members, c) => {
    S.workRange = [laidOut / ids.length, (laidOut + members.length) / ids.length]
    laidOut += members.length
    if (members.length > DIST_LAYOUT_LIMIT)
      throw new Error(
        `${kind === 'kk' ? 'Kamada–Kawai' : 'Stress majorization'} handles connected parts of up to ${DIST_LAYOUT_LIMIT.toLocaleString('en-US')} nodes; this network has one of ${members.length.toLocaleString('en-US')}. Use a force-directed layout instead.`
      )
    const lay =
      kind === 'kk'
        ? kkComponent(members.length, adjs[c])
        : stressComponent(members.length, adjs[c])
    // hop units -> FR-like units so packing gaps look alike
    return { members, x: lay.x, y: lay.y }
  })
  S.workRange = [0, 1]
  packComponents(parts)
  const out = {}
  parts.forEach((p) =>
    p.members.forEach((g, i) => {
      out[ids[g]] = { x: p.x[i] + p.ox, y: p.y[i] + p.oy }
    })
  )
  return out
}

function distanceLayoutAsync(ids, edges, kind, onProgress) {
  const worker = ids.length > 60 ? getFrWorker() : null
  if (!worker) return Promise.resolve(distanceLayout(ids, edges, kind))
  const id = ++S.frRequestSeq
  return new Promise((resolve, reject) => {
    frPending.set(id, {
      resolve,
      reject,
      onProgress,
      local: () => distanceLayout(ids, edges, kind),
    })
    worker.postMessage({ id, kind, ids, edges })
  })
}

// The weighted layout runs in a background worker when the browser allows
// it, so large networks don't freeze the page. The worker is built from
// the same functions as above; if it can't be created, or fails, the
// layout runs on the page instead.
let frWorker = null

let frWorkerFailed = false

export const frPending = new Map()

export function getFrWorker() {
  if (frWorker || frWorkerFailed) return frWorker
  try {
    const src =
      [
        mulberry32,
        connectedComponents,
        frComponent,
        packComponents,
        frLayout,
        fdebBundle,
        fr3dLayout,
        componentDistances,
        pivotMDS,
        kkComponent,
        stressComponent,
        distanceLayout,
        workStep,
      ]
        .map((f) => f.toString())
        .join('\n') +
      `\nconst FR_SEED = ${FR_SEED};\nconst DIST_LAYOUT_LIMIT = ${DIST_LAYOUT_LIMIT};\nvar workRange = [0, 1], workSink = null, workLast = 0;\n` +
      'onmessage = e => { const m = e.data; let result; workRange = [0, 1]; workLast = 0; workSink = f => postMessage({ id: m.id, progress: f }); try{ result = m.kind === "fdeb" ? fdebBundle(m.segs, m.opts) : m.kind === "fr3d" ? fr3dLayout(m.ids, m.edges, m.opts) : (m.kind === "kk" || m.kind === "stress") ? distanceLayout(m.ids, m.edges, m.kind) : frLayout(m.ids, m.edges); } catch(err){ postMessage({ id: m.id, error: String(err && err.message || err) }); return; } postMessage({ id: m.id, result }); };'
    const url = URL.createObjectURL(new Blob([src], { type: 'text/javascript' }))
    frWorker = new Worker(url)
    frWorker.onmessage = (e) => {
      const job = frPending.get(e.data.id)
      if (!job) return
      if (e.data.progress !== undefined) {
        if (job.onProgress) job.onProgress(e.data.progress)
        return
      }
      frPending.delete(e.data.id)
      if (e.data.error) {
        if (job.reject) job.reject(new Error(e.data.error))
        else job.resolve(null)
      } else job.resolve(e.data.result)
    }
    frWorker.onerror = (e) => {
      e.preventDefault()
      frWorkerFailed = true
      frWorker = null
      // finish anything still waiting on the page itself
      frPending.forEach((job) => job.resolve(job.local()))
      frPending.clear()
    }
  } catch (err) {
    frWorkerFailed = true
    frWorker = null
  }
  return frWorker
}

// Stops any weighted layout still computing in the worker; its callers see
// null and, being superseded, discard it.
export function cancelFrJobs() {
  if (!frWorker || !frPending.size) return
  frWorker.terminate()
  frWorker = null
  frPending.forEach((job) => job.resolve(null))
  frPending.clear()
}

export function frLayoutAsync(ids, edges, onProgress) {
  const worker = ids.length > 60 ? getFrWorker() : null // small graphs: not worth the round trip
  if (!worker) return Promise.resolve(frLayout(ids, edges))
  const id = ++S.frRequestSeq
  return new Promise((resolve) => {
    frPending.set(id, { resolve, onProgress, local: () => frLayout(ids, edges) })
    worker.postMessage({ id, kind: 'fr', ids, edges })
  })
}

export function bundleAsync(segs, opts, onProgress) {
  const worker = segs.length > 150 ? getFrWorker() : null
  if (!worker) return Promise.resolve(fdebBundle(segs, opts))
  const id = ++S.frRequestSeq
  return new Promise((resolve) => {
    frPending.set(id, { resolve, onProgress, local: () => fdebBundle(segs, opts) })
    worker.postMessage({ id, kind: 'fdeb', segs, opts })
  })
}

// Layouts that place nodes without looking at edges; strategies can skip
// adding layout-only links for these, since they change nothing.
export const EDGE_BLIND_LAYOUTS = new Set(['circle', 'grid', 'random'])

// Runs a layout on a detached, invisible headless Cytoscape instance (or the
// weighted FR above) and returns a { nodeId: {x,y} } position map in
// arbitrary units; callers rescale. Edge weights are honored by 'fr'
// (attraction x weight) and by 'cose' (shorter, stiffer springs).
export async function computeSubLayoutAsync(nodeIds, edgeDefs, layoutName) {
  const onProgress = S.layoutProgress ? (f) => S.layoutProgress && S.layoutProgress.report(f) : null
  if (layoutName === 'fr' && nodeIds.length > 1) return frLayoutAsync(nodeIds, edgeDefs, onProgress)
  if ((layoutName === 'kk' || layoutName === 'stress') && nodeIds.length > 1)
    return distanceLayoutAsync(nodeIds, edgeDefs, layoutName, onProgress)
  return computeSubLayout(nodeIds, edgeDefs, layoutName)
}

export function makeLayoutProgress(show) {
  return {
    total: 1,
    done: 0,
    current: 1,
    begin(total) {
      this.total = Math.max(1e-9, total)
      this.done = 0
      this.current = 0
    },
    part(weight) {
      this.done += this.current
      this.current = weight
      this.report(0)
    },
    report(f) {
      show(Math.min(1, (this.done + this.current * Math.max(0, Math.min(1, f))) / this.total))
    },
  }
}

export function computeSubLayout(nodeIds, edgeDefs, layoutName) {
  if (nodeIds.length === 1) {
    return { [nodeIds[0]]: { x: 0, y: 0 } }
  }
  if (layoutName === 'fr') return frLayout(nodeIds, edgeDefs)
  if (layoutName === 'kk' || layoutName === 'stress')
    return distanceLayout(nodeIds, edgeDefs, layoutName)
  const size = Math.max(300, Math.sqrt(nodeIds.length) * 90)
  const sub = cytoscape({
    headless: true,
    styleEnabled: false,
    elements: {
      nodes: nodeIds.map((id) => ({ data: { id } })),
      edges: edgeDefs.map((e, i) => ({
        data: {
          id: 'sub' + i,
          source: e.source,
          target: e.target,
          weight: Math.min(100, Math.max(0.02, Number.isFinite(e.weight) ? e.weight : 1)),
        },
      })),
    },
  })
  const opts = {
    ...layoutOptsFor(layoutName),
    animate: false,
    fit: false,
    boundingBox: { x1: 0, y1: 0, w: size, h: size },
  }
  if (layoutName === 'cose') {
    opts.idealEdgeLength = (e) => 90 / Math.sqrt(e.data('weight'))
    opts.edgeElasticity = (e) => 32 / e.data('weight')
    opts.randomize = true
  }
  sub.layout(opts).run()
  const positions = {}
  sub.nodes().forEach((n) => {
    positions[n.id()] = { x: n.position('x'), y: n.position('y') }
  })
  sub.destroy()
  return positions
}

// Typical spacing between neighboring nodes on screen, from current node sizes.
export function targetNodeSpacing() {
  const nodes = cy.nodes(':visible')
  if (!nodes.length) return 64
  const mean = nodes.reduce((s, n) => s + (n.data('size') || 42), 0) / nodes.length
  return Math.max(24, mean * 1.6)
}

// Rescales a position map so the median nearest-neighbor distance equals
// `spacing` pixels. Positions from different algorithms come in very
// different units; this puts them all on the same screen scale.
export function normalizeSpacing(positions, spacing) {
  const ids = Object.keys(positions)
  if (ids.length < 2) return positions
  const sample =
    ids.length > 1500 ? ids.filter((_, i) => i % Math.ceil(ids.length / 1500) === 0) : ids
  const pts = sample.map((id) => positions[id])
  const nn = []
  for (let i = 0; i < pts.length; i++) {
    let best = Infinity
    for (let j = 0; j < pts.length; j++) {
      if (i === j) continue
      const d = Math.hypot(pts[i].x - pts[j].x, pts[i].y - pts[j].y)
      if (d > 1e-9 && d < best) best = d
    }
    if (Number.isFinite(best)) nn.push(best)
  }
  if (!nn.length) return positions
  nn.sort((a, b) => a - b)
  const scale = spacing / nn[Math.floor(nn.length / 2)]
  const out = {}
  ids.forEach((id) => {
    out[id] = { x: positions[id].x * scale, y: positions[id].y * scale }
  })
  return out
}

export function applyPositions(positions) {
  cy.layout({
    name: 'preset',
    positions: (node) => positions[node.id()] || node.position(),
    animate: cy.nodes().length <= 1500,
    animationDuration: 500,
    fit: true,
    padding: 50,
  }).run()
}
