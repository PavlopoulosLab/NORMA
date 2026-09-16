// @ts-nocheck
// ponytail: split verbatim from the former single-file script; typed one file at a time (remove @ts-nocheck as it lands)
import {
  EDGE_BLIND_LAYOUTS,
  FR_SEED,
  computeSubLayoutAsync,
  normalizeSpacing,
  targetNodeSpacing,
} from '../metrics'
import { S } from '../state'
import { UNGROUPED, effectiveGroupsFor } from '../network_state'
import { cy } from '../cy'
import { mulberry32 } from '../sample_data'
import { nextPaint } from './controls'

/* ---------- layout input: what the strategies see ---------- */
// Visible nodes, their active groups, and one weighted edge per node pair
// (parallel channels collapse, keeping the highest weight, as NORMA's
// simplify(edge.attr.comb = "max") does). Missing weights count as 1.
export function collectLayoutGraph() {
  const channelOnly = document.getElementById('layoutOnActiveOnly').checked
  const nodes = cy.nodes().filter((n) => !n.hasClass('hidden-group'))
  const nodeIds = nodes.map((n) => n.id())
  const visible = new Set(nodeIds)
  const groupsOf = {}
  const groupMembers = new Map()
  nodes.forEach((n) => {
    const gs = effectiveGroupsFor(n).filter((g) => g !== UNGROUPED)
    groupsOf[n.id()] = gs
    gs.forEach((g) => {
      if (!groupMembers.has(g)) groupMembers.set(g, [])
      groupMembers.get(g).push(n.id())
    })
  })
  const pairs = new Map()
  cy.edges().forEach((e) => {
    if (channelOnly && !S.activeTypes.has(e.data('type'))) return
    const s = e.data('source'),
      t = e.data('target')
    if (s === t || !visible.has(s) || !visible.has(t)) return
    const key = s < t ? s + '\t' + t : t + '\t' + s
    const w = typeof e.data('weight') === 'number' && e.data('weight') > 0 ? e.data('weight') : 1
    const p = pairs.get(key)
    if (p) p.weight = Math.max(p.weight, w)
    else pairs.set(key, { source: s, target: t, weight: w })
  })
  return { nodeIds, groupsOf, groupMembers, edges: [...pairs.values()] }
}

function sharesGroup(a, b) {
  return a.length > 0 && b.length > 0 && a.some((g) => b.includes(g))
}

// All-vs-all pairs inside a group. Very large groups get a sparse random
// subset per member instead of a full clique, which would otherwise add
// hundreds of thousands of layout-only edges.
const CLIQUE_FULL_LIMIT = 400

const CLIQUE_SPARSE_DEGREE = 60

function groupPairs(members, rand) {
  const out = []
  if (members.length <= CLIQUE_FULL_LIMIT) {
    for (let i = 0; i < members.length; i++) {
      for (let j = i + 1; j < members.length; j++) out.push([members[i], members[j]])
    }
  } else {
    members.forEach((a) => {
      for (let k = 0; k < CLIQUE_SPARSE_DEGREE; k++) {
        const b = members[Math.floor(rand() * members.length)]
        if (a !== b) out.push([a, b])
      }
    })
  }
  return out
}

/* ============================================================
   NORMA-2.0 GROUP LAYOUT STRATEGIES
   Karatzas et al. (2022) Bioinformatics Advances 2(1):vbac036,
   Figure 1. Weights and steps follow NORMA's own implementation
   (strategy1_virtualNodes, strategy2_gravity, strategy3_superNodes in
   github.com/PavlopoulosLab/NORMA, functions/annotations.R).
   ============================================================ */

// Strategy 1 -- virtual nodes (Fig. 1A). One hidden hub per group, tied to
// every member with a heavy edge (50); real edges get weight 5 and group
// members get light all-pairs edges (0.1). The chosen layout runs on this
// enlarged graph and the hubs are then dropped.
export async function strategyVirtualNodes(graph, layoutName) {
  const rand = mulberry32(FR_SEED)
  const ids = [...graph.nodeIds]
  const edges = graph.edges.map((e) => ({ source: e.source, target: e.target, weight: 5 }))
  graph.groupMembers.forEach((members, g) => {
    const hub = '\u0000virtual:' + g
    ids.push(hub)
    if (members.length >= 2 && !EDGE_BLIND_LAYOUTS.has(layoutName)) {
      groupPairs(members, rand).forEach(([a, b]) =>
        edges.push({ source: a, target: b, weight: 0.1 })
      )
    }
    members.forEach((m) => edges.push({ source: hub, target: m, weight: 50 }))
  })
  const pos = await computeSubLayoutAsync(ids, edges, layoutName)
  if (!pos) return null
  const real = {}
  graph.nodeIds.forEach((id) => {
    real[id] = pos[id]
  })
  return normalizeSpacing(real, targetNodeSpacing())
}

// Strategy 2 -- group gravity (Fig. 1B). Groups become cliques with
// layout-only edges at the network's highest weight; real edges inside a
// group are multiplied by the force and edges that leave a group (or touch
// an ungrouped node) are divided by it. Parallel pairs keep the max.
export async function strategyGravity(graph, layoutName, force) {
  const rand = mulberry32(FR_SEED)
  const maxW = graph.edges.reduce((m, e) => Math.max(m, e.weight), 1)
  const pairs = new Map()
  const put = (a, b, w) => {
    const key = a < b ? a + '\t' + b : b + '\t' + a
    const p = pairs.get(key)
    if (p) p.weight = Math.max(p.weight, w)
    else pairs.set(key, { source: a, target: b, weight: w })
  }
  graph.edges.forEach((e) => {
    const inside = sharesGroup(graph.groupsOf[e.source], graph.groupsOf[e.target])
    put(e.source, e.target, inside ? e.weight * force : e.weight / force)
  })
  if (!EDGE_BLIND_LAYOUTS.has(layoutName)) {
    graph.groupMembers.forEach((members) => {
      groupPairs(members, rand).forEach(([a, b]) => put(a, b, maxW))
    })
  }
  const pos = await computeSubLayoutAsync(graph.nodeIds, [...pairs.values()], layoutName)
  return pos ? normalizeSpacing(pos, targetNodeSpacing()) : null
}

/* ---------- shared group placement ----------
   Radius a group's local layout gets, in pixels. A ring must fit every
   member at the usual node spacing, so circles of big groups get a larger
   radius than the area-based estimate used for the other local layouts. */
function localGroupRadius(k, localName, spacing, clusterScale) {
  const area = 0.95 * spacing * Math.sqrt(Math.max(1, k))
  const ring = localName === 'circle' ? (k * spacing) / (2 * Math.PI) : 0
  return Math.max(spacing / 2, area, ring) * clusterScale
}

// Lays each group's members out with `localName`, scaled to `radii[g]` and
// centred on `centers[g]`. Returns { nodeId: [candidate positions] } so that
// a node in several groups can be averaged afterwards.
async function placeGroupsLocally(graph, groups, centers, radii, localName, force) {
  const rand = mulberry32(FR_SEED)
  const candidates = {}
  const lp0 = S.layoutProgress
  if (lp0) lp0.begin([...groups].reduce((a, [, m]) => a + m.length, 0))
  let lastPaint = performance.now()
  for (const [g, members] of groups) {
    if (lp0) lp0.part(members.length)
    if (performance.now() - lastPaint > 60) {
      await nextPaint()
      lastPaint = performance.now()
    }
    const center = centers[g]
    const radius = radii[g]
    if (members.length === 1) {
      ;(candidates[members[0]] = candidates[members[0]] || []).push({ x: center.x, y: center.y })
      continue
    }
    const memberSet = new Set(members)
    const local = graph.edges
      .filter((e) => memberSet.has(e.source) && memberSet.has(e.target))
      .map((e) => ({ ...e }))
    if (!EDGE_BLIND_LAYOUTS.has(localName)) {
      const minW = local.length ? local.reduce((m, e) => Math.min(m, e.weight), Infinity) : 1
      groupPairs(members, rand).forEach(([a, b]) =>
        local.push({ source: a, target: b, weight: minW / Math.max(1, force) })
      )
    }
    const lp = await computeSubLayoutAsync(members, local, localName)
    if (!lp) return null
    let x1 = Infinity,
      x2 = -Infinity,
      y1 = Infinity,
      y2 = -Infinity
    members.forEach((id) => {
      x1 = Math.min(x1, lp[id].x)
      x2 = Math.max(x2, lp[id].x)
      y1 = Math.min(y1, lp[id].y)
      y2 = Math.max(y2, lp[id].y)
    })
    const mx = (x1 + x2) / 2,
      my = (y1 + y2) / 2
    // fit the farthest member onto the disc's edge, so every shape
    // (square grids included) stays inside the group's disc
    const reach = members.reduce(
      (m, id) => Math.max(m, Math.hypot(lp[id].x - mx, lp[id].y - my)),
      0
    )
    const scale = reach > 1e-9 ? radius / reach : 0
    members.forEach((id) => {
      ;(candidates[id] = candidates[id] || []).push({
        x: center.x + (lp[id].x - mx) * scale,
        y: center.y + (lp[id].y - my) * scale,
      })
    })
  }
  return candidates
}

function averageCandidates(candidates, fallback) {
  const out = { ...(fallback || {}) }
  Object.entries(candidates).forEach(([id, list]) => {
    out[id] = {
      x: list.reduce((a, p) => a + p.x, 0) / list.length,
      y: list.reduce((a, p) => a + p.y, 0) / list.length,
    }
  })
  return out
}

// Pushes discs apart until none overlap (keeping `gap` between them).
// Smaller discs give way more than big ones. With `compact`, discs are
// first drawn toward the middle while being kept apart, which packs
// scattered groups (for example unconnected ones) into a tidy cluster.
function resolveOverlaps(keys, centers, radii, gap, { compact = false } = {}) {
  const n = keys.length
  if (n < 2) return
  const pullPasses = compact ? 250 : 0
  for (let pass = 0; pass < pullPasses + 400; pass++) {
    if (pass < pullPasses) {
      let mx = 0,
        my = 0
      keys.forEach((k) => {
        mx += centers[k].x
        my += centers[k].y
      })
      mx /= n
      my /= n
      const pull = 0.04 * (1 - pass / pullPasses)
      keys.forEach((k) => {
        centers[k].x += (mx - centers[k].x) * pull
        centers[k].y += (my - centers[k].y) * pull
      })
    }
    let worst = 0
    for (let i = 0; i < n; i++) {
      const a = centers[keys[i]],
        ra = radii[keys[i]]
      for (let j = i + 1; j < n; j++) {
        const b = centers[keys[j]],
          rb = radii[keys[j]]
        let dx = b.x - a.x,
          dy = b.y - a.y
        let d = Math.hypot(dx, dy)
        const overlap = ra + rb + gap - d
        if (overlap <= 0) continue
        if (d < 1e-6) {
          dx = Math.cos(i * 2.399 + j)
          dy = Math.sin(i * 2.399 + j)
          d = 1
        }
        const shareA = rb / (ra + rb),
          shareB = ra / (ra + rb)
        a.x -= (dx / d) * overlap * shareA
        a.y -= (dy / d) * overlap * shareA
        b.x += (dx / d) * overlap * shareB
        b.y += (dy / d) * overlap * shareB
        worst = Math.max(worst, overlap)
      }
    }
    if (pass >= pullPasses && worst < 0.5) break
  }
}

// Places discs around a ring so neighbours are exactly far enough apart
// (straight-line distance, not arc length). Returns the ring radius.
function ringRadiusFor(radii, gap, minR) {
  const needAngle = (R) =>
    radii.reduce((acc, r) => acc + 2 * Math.asin(Math.min(1, (r + gap / 2) / R)), 0)
  let lo = Math.max(minR, Math.max(...radii) + gap / 2),
    hi = lo
  while (needAngle(hi) > 2 * Math.PI) hi *= 1.5
  if (needAngle(lo) <= 2 * Math.PI) return lo
  for (let i = 0; i < 40; i++) {
    const mid = (lo + hi) / 2
    if (needAngle(mid) > 2 * Math.PI) lo = mid
    else hi = mid
  }
  return hi
}

function placeOnRing(keys, radii, gap, R, centers, startAngle) {
  const halves = keys.map((g) => Math.asin(Math.min(1, (radii[g] + gap / 2) / R)))
  const used = halves.reduce((a, h) => a + 2 * h, 0)
  const slack = keys.length ? Math.max(0, 2 * Math.PI - used) / keys.length : 0
  let angle = startAngle
  keys.forEach((g, i) => {
    angle += halves[i] + slack / 2
    centers[g] = { x: R * Math.cos(angle), y: R * Math.sin(angle) }
    angle += halves[i] + slack / 2
  })
}

// The collapsed "group network": one node per group, linked when real
// edges cross between groups, weighted by how many do.
function groupSuperGraph(graph, groups) {
  const memberOf = {}
  groups.forEach(([g, members]) =>
    members.forEach((id) => (memberOf[id] = memberOf[id] || []).push(g))
  )
  const pairs = new Map()
  graph.edges.forEach((e) => {
    ;(memberOf[e.source] || []).forEach((a) =>
      (memberOf[e.target] || []).forEach((b) => {
        if (a === b) return
        const key = a < b ? a + '\t' + b : b + '\t' + a
        const p = pairs.get(key)
        if (p) p.count += 1
        else pairs.set(key, { source: a, target: b, count: 1 })
      })
    )
  })
  // log scale: a pair of groups with hundreds of links shouldn't collapse
  return [...pairs.values()].map((p) => ({
    source: p.source,
    target: p.target,
    weight: 1 + Math.log(p.count),
  }))
}

/* ---------- group blocks ----------
   Each group (plus one block for nodes in no group) is a disc sized to its
   local layout; the discs are arranged with the chosen scheme and never
   overlap. `spacing` sets the gap between discs. */
const UNGROUPED_BLOCK = '\u0000ungrouped'

export async function blockGroupLayout(graph, arrangement, localName, force, clusterScale) {
  const spacing = targetNodeSpacing()
  const groups = [...graph.groupMembers]
  const ungrouped = graph.nodeIds.filter((id) => !graph.groupsOf[id].length)
  if (ungrouped.length) groups.push([UNGROUPED_BLOCK, ungrouped])
  const keys = groups.map(([g]) => g)
  const radii = {}
  groups.forEach(([g, m]) => {
    radii[g] = localGroupRadius(m.length, localName, spacing, clusterScale)
  })
  // room for the group shading (it extends beyond the outermost nodes)
  const gap = spacing * 2.5 * (force / 10)
  const centers = {}

  if (arrangement === 'circle') {
    if (keys.length === 1) {
      centers[keys[0]] = { x: 0, y: 0 }
    } else {
      const R = ringRadiusFor(
        keys.map((g) => radii[g]),
        gap,
        0
      )
      placeOnRing(keys, radii, gap, R, centers, -Math.PI / 2)
    }
  } else if (arrangement === 'grid') {
    const cells = keys.map((g) => 2 * radii[g] + gap)
    const area = cells.reduce((a, c) => a + c * c, 0)
    const rowWidth = Math.max(Math.max(...cells), Math.sqrt(area) * 1.15)
    let x = 0,
      y = 0,
      rowH = 0
    keys.forEach((g, i) => {
      if (x > 0 && x + cells[i] > rowWidth) {
        x = 0
        y += rowH
        rowH = 0
      }
      centers[g] = { x: x + cells[i] / 2, y: y + cells[i] / 2 }
      x += cells[i]
      rowH = Math.max(rowH, cells[i])
    })
  } else if (arrangement === 'concentric') {
    // biggest group in the middle, then rings of the next biggest
    const order = keys.slice().sort((x, y) => radii[y] - radii[x])
    centers[order[0]] = { x: 0, y: 0 }
    let inner = radii[order[0]]
    let i = 1,
      ringIndex = 0
    while (i < order.length) {
      const ringR = inner + gap + radii[order[i]]
      const ring = []
      let usedAngle = 0
      while (i < order.length) {
        const need = 2 * Math.asin(Math.min(1, (radii[order[i]] + gap / 2) / ringR))
        if (ring.length && usedAngle + need > 2 * Math.PI) break
        ring.push(order[i])
        usedAngle += need
        i++
      }
      placeOnRing(ring, radii, gap, ringR, centers, ringIndex * 0.7)
      inner = ringR + Math.max(...ring.map((g) => radii[g]))
      ringIndex++
    }
  } else if (arrangement === 'breadthfirst') {
    // levels by distance in the group network from its best-connected group
    const edges = groupSuperGraph(graph, groups)
    const adj = {}
    keys.forEach((g) => {
      adj[g] = new Set()
    })
    edges.forEach((e) => {
      adj[e.source].add(e.target)
      adj[e.target].add(e.source)
    })
    const level = {}
    const byDegree = keys.slice().sort((a, b) => adj[b].size - adj[a].size || radii[b] - radii[a])
    let depthBase = 0
    byDegree.forEach((root) => {
      if (level[root] !== undefined) return
      level[root] = depthBase
      const queue = [root]
      let maxDepth = depthBase
      while (queue.length) {
        const g = queue.shift()
        adj[g].forEach((h) => {
          if (level[h] === undefined) {
            level[h] = level[g] + 1
            maxDepth = Math.max(maxDepth, level[h])
            queue.push(h)
          }
        })
      }
      depthBase = maxDepth + 1
    })
    const rows = []
    keys.forEach((g) => {
      ;(rows[level[g]] = rows[level[g]] || []).push(g)
    })
    let y = 0
    rows.filter(Boolean).forEach((row) => {
      const h = Math.max(...row.map((g) => 2 * radii[g])) + gap
      const width = row.reduce((a, g) => a + 2 * radii[g] + gap, 0)
      let x = -width / 2
      row.forEach((g) => {
        centers[g] = { x: x + radii[g] + gap / 2, y: y + h / 2 }
        x += 2 * radii[g] + gap
      })
      y += h
    })
  } else {
    // force-directed arrangement of the group network, then de-overlap
    const algorithm = arrangement === 'cose' ? 'cose' : 'fr'
    const pos =
      keys.length > 1
        ? await computeSubLayoutAsync(keys, groupSuperGraph(graph, groups), algorithm)
        : { [keys[0]]: { x: 0, y: 0 } }
    if (!pos) return null
    const mx = keys.reduce((a, g) => a + pos[g].x, 0) / keys.length
    const my = keys.reduce((a, g) => a + pos[g].y, 0) / keys.length
    const spread =
      keys.reduce((a, g) => a + Math.hypot(pos[g].x - mx, pos[g].y - my), 0) / keys.length || 1
    const target = keys.reduce((a, g) => a + radii[g] + gap / 2, 0) / Math.PI
    keys.forEach((g) => {
      centers[g] = {
        x: ((pos[g].x - mx) / spread) * target,
        y: ((pos[g].y - my) / spread) * target,
      }
    })
    resolveOverlaps(keys, centers, radii, gap, { compact: true })
  }

  const candidates = await placeGroupsLocally(graph, groups, centers, radii, localName, force)
  return candidates ? averageCandidates(candidates) : null
}

// Strategies 1 and 2 place every node themselves. With a local layout
// chosen, each group is then rearranged around where the strategy put it.
export async function applyLocalLayoutsAfter(graph, positions, localName, force, clusterScale) {
  if (!positions || localName === 'keep') return positions
  const spacing = targetNodeSpacing()
  const groups = [...graph.groupMembers]
  const keys = groups.map(([g]) => g)
  const centers = {},
    radii = {}
  groups.forEach(([g, members]) => {
    centers[g] = {
      x: members.reduce((a, id) => a + positions[id].x, 0) / members.length,
      y: members.reduce((a, id) => a + positions[id].y, 0) / members.length,
    }
    radii[g] = localGroupRadius(members.length, localName, spacing, clusterScale)
  })
  resolveOverlaps(keys, centers, radii, spacing * 1.5)
  const candidates = await placeGroupsLocally(graph, groups, centers, radii, localName, force)
  return candidates ? averageCandidates(candidates, positions) : null
}

// Strategy 3 -- super nodes (Fig. 1C). Each group collapses into one
// super-node; ungrouped nodes stay as themselves. The chosen algorithm runs
// on that small super-network, the result is pushed outward by the force,
// and each group's members are then laid out locally (with light all-pairs
// edges, min weight / force) around their super-node. A node in several
// groups is placed at the average of its positions.
export async function strategySuperNodes(graph, globalName, localName, force, clusterScale) {
  const spacing = targetNodeSpacing()
  const superOf = (id) =>
    graph.groupsOf[id].length ? graph.groupsOf[id].map((g) => 'g\u0000' + g) : ['n\u0000' + id]

  const superIds = new Set()
  graph.nodeIds.forEach((id) => superOf(id).forEach((s) => superIds.add(s)))
  const superPairs = new Map()
  graph.edges.forEach((e) => {
    superOf(e.source).forEach((a) =>
      superOf(e.target).forEach((b) => {
        if (a === b) return
        const key = a < b ? a + '\t' + b : b + '\t' + a
        const p = superPairs.get(key)
        if (p) p.weight = Math.max(p.weight, e.weight)
        else superPairs.set(key, { source: a, target: b, weight: e.weight })
      })
    )
  })
  const superList = [...superIds]
  const superPos = await computeSubLayoutAsync(superList, [...superPairs.values()], globalName)
  if (!superPos) return null

  const radiusOf = (s) =>
    s.startsWith('n\u0000')
      ? spacing / 2
      : localGroupRadius(
          graph.groupMembers.get(s.slice(2)).length,
          localName,
          spacing,
          clusterScale
        )
  const cx = superList.reduce((a, s) => a + superPos[s].x, 0) / superList.length
  const cyc = superList.reduce((a, s) => a + superPos[s].y, 0) / superList.length
  let normalize = 1
  if (superList.length > 1) {
    const radii = superList.map(radiusOf)
    const ring = radii.reduce((a, r) => a + r, 0) / Math.PI
    const disk = (2 / 3) * Math.sqrt(2 * radii.reduce((a, r) => a + r * r, 0))
    const targetSpread = Math.min(ring, disk)
    const meanSpread =
      superList.reduce((a, s) => a + Math.hypot(superPos[s].x - cx, superPos[s].y - cyc), 0) /
      superList.length
    if (meanSpread > 1e-9) {
      const spreadFactor = targetSpread / meanSpread
      const groupSupers = superList.filter((s) => s.startsWith('g\u0000'))
      let pairFactor = 0
      for (let i = 0; i < groupSupers.length; i++) {
        for (let j = i + 1; j < groupSupers.length; j++) {
          const a = superPos[groupSupers[i]],
            b = superPos[groupSupers[j]]
          const d = Math.hypot(a.x - b.x, a.y - b.y)
          if (d > 1e-9)
            pairFactor = Math.max(
              pairFactor,
              (radiusOf(groupSupers[i]) + radiusOf(groupSupers[j])) / d
            )
        }
      }
      normalize = Math.min(Math.max(spreadFactor, pairFactor), spreadFactor * 2.5)
    } else {
      superList.forEach((s, i) => {
        const angle = (2 * Math.PI * i) / superList.length
        superPos[s] = { x: cx + Math.cos(angle), y: cyc + Math.sin(angle) }
      })
      normalize = targetSpread
    }
  }
  // NORMA multiplies the super-node coordinates by the force (default 10);
  // here force 10 gives 1.6x the "just touching" arrangement.
  const repel = normalize * force * 0.16
  const placed = {}
  superList.forEach((s) => {
    placed[s] = { x: (superPos[s].x - cx) * repel, y: (superPos[s].y - cyc) * repel }
  })

  const groups = [...graph.groupMembers]
  const centers = {},
    radii = {}
  groups.forEach(([g]) => {
    centers[g] = placed['g\u0000' + g]
    radii[g] = radiusOf('g\u0000' + g)
  })
  const candidates = await placeGroupsLocally(graph, groups, centers, radii, localName, force)
  if (!candidates) return null
  const ungrouped = {}
  graph.nodeIds.forEach((id) => {
    if (!graph.groupsOf[id].length) ungrouped[id] = placed['n\u0000' + id]
  })
  return averageCandidates(candidates, ungrouped)
}
