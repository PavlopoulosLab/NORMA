// @ts-nocheck
// ponytail: split verbatim from the former single-file script; typed one file at a time (remove @ts-nocheck as it lands)
import { S } from '../state'
import { UNGROUPED, effectiveGroupsFor, getUsedGroups } from '../network_state'
import { VIEW_PRESETS_3D, fit3d, moveCamera3d, shownIds3d } from './camera'
import {
  anyEdgeDirected,
  edgeIsDirected,
  fibonacciBall,
  fibonacciSphere,
  fr3dAsync,
  fr3dLayout,
} from '../export/dialog'
import { computeMetricValues, shownEdges, shownNodes, targetNodeSpacing } from '../metrics'
import { cy } from '../cy'
import { el3, net3d, requestRender3d } from './state'
import { mulberry32 } from '../sample_data'
import { pos3dOf } from './cache'
import { render3d } from './draw'
import { setStatus } from '../layouts/controls'

/* ---------- layouts ---------- */
function setLayoutStatus3d(items) {
  setStatus('layout3dStatus', items)
}

function positions2dOf(ids) {
  const out = {}
  ids.forEach((id) => {
    const p = cy.getElementById(id).position()
    out[id] = [p.x, p.y]
  })
  return out
}

function centerPositions(map) {
  const ids = Object.keys(map)
  if (!ids.length) return map
  const c = [0, 0, 0]
  ids.forEach((id) => {
    c[0] += map[id][0]
    c[1] += map[id][1]
    c[2] += map[id][2]
  })
  c.forEach((v, i) => {
    c[i] = v / ids.length
  })
  ids.forEach((id) => {
    map[id] = [map[id][0] - c[0], map[id][1] - c[1], map[id][2] - c[2]]
  })
  return map
}

// Layer index (0, 1, ...) for each node: its ticked groups in legend order;
// nodes in several groups sit between their layers.
function groupLayerOf(nodes) {
  const groups = getUsedGroups().filter((g) => S.activeGroups.has(g))
  const rank = new Map(groups.map((g, i) => [g, i]))
  const out = {}
  nodes.forEach((n) => {
    const gs = effectiveGroupsFor(n).filter((g) => rank.has(g))
    out[n.id()] = gs.length ? gs.reduce((s, g) => s + rank.get(g), 0) / gs.length : groups.length
  })
  return out
}

function hierarchyLevels(nodes, edges) {
  const ids = nodes.map((n) => n.id())
  const out = new Map(ids.map((id) => [id, []]))
  const indeg = new Map(ids.map((id) => [id, 0]))
  edges.forEach((e) => {
    const s = e.data('source'),
      t = e.data('target')
    if (!out.has(s) || !out.has(t) || s === t) return
    out.get(s).push(t)
    indeg.set(t, indeg.get(t) + 1)
    if (!edgeIsDirected(e)) {
      out.get(t).push(s)
      indeg.set(s, indeg.get(s) + 1)
    }
  })
  const level = new Map()
  const directed = anyEdgeDirected(edges)
  // roots: nodes nothing points to (directed) or the best-connected node of each component
  const byDegree = [...ids].sort((a, b) => out.get(b).length - out.get(a).length)
  const roots = directed ? ids.filter((id) => indeg.get(id) === 0 && out.get(id).length) : []
  const bfs = (starts) => {
    const queue = []
    starts.forEach((s) => {
      if (!level.has(s)) {
        level.set(s, 0)
        queue.push(s)
      }
    })
    for (let q = 0; q < queue.length; q++) {
      const v = queue[q]
      out.get(v).forEach((w) => {
        if (!level.has(w)) {
          level.set(w, level.get(v) + 1)
          queue.push(w)
        }
      })
    }
  }
  bfs(roots)
  byDegree.forEach((id) => {
    if (!level.has(id)) bfs([id])
  })
  return level
}

async function computeLayout3d(algo) {
  const nodes = shownNodes()
  const channelOnly = el3('layout3dChannels').checked
  const edges = shownEdges(channelOnly)
  const ids = nodes.map((n) => n.id())
  const spacing = targetNodeSpacing()
  const layerGap = parseFloat(el3('layer3dSpacing').value) || 150
  const pos2 = positions2dOf(ids)
  const result = {}
  if (!ids.length) return result

  if (algo === 'fr3d') {
    const init = {}
    const r = await fr3dAsync(
      ids,
      edges.map((e) => ({ s: e.data('source'), t: e.data('target'), w: e.data('weight') })),
      { spacing, seed: 123 },
      net3d.onLayoutProgress
    )
    return r
  }
  if (algo === 'flat') {
    ids.forEach((id) => {
      result[id] = [pos2[id][0], pos2[id][1], 0]
    })
    return centerPositions(result)
  }
  if (algo === 'layers' || algo === 'hierarchy' || algo === 'degree') {
    // the 2D layout lies flat; height (y) comes from groups, levels or degree
    let height
    if (algo === 'layers') {
      const layer = groupLayerOf(nodes)
      height = (id) => layer[id] * layerGap
    } else if (algo === 'hierarchy') {
      const level = hierarchyLevels(nodes, edges)
      height = (id) => (level.get(id) || 0) * layerGap
    } else {
      const deg = computeMetricValues('degree', channelOnly)
      const max = Math.max(1, ...Object.values(deg))
      height = (id) => (-(deg[id] || 0) / max) * layerGap * 3
    }
    ids.forEach((id) => {
      result[id] = [pos2[id][0], height(id), pos2[id][1]]
    })
    return centerPositions(result)
  }
  if (algo === 'sphere') {
    const R = spacing * Math.sqrt(ids.length) * 0.5 + spacing
    const order = [...nodes].sort((a, b) => b.degree() - a.degree())
    fibonacciSphere(order.length).forEach((p, i) => {
      result[order[i].id()] = [p[0] * R, p[1] * R, p[2] * R]
    })
    return result
  }
  if (algo === 'cube') {
    const side = Math.ceil(Math.cbrt(ids.length))
    const order = [...nodes].sort((a, b) => b.degree() - a.degree())
    order.forEach((n, i) => {
      const x = i % side,
        y = Math.floor(i / side) % side,
        z = Math.floor(i / (side * side))
      const step = spacing * 1.8
      result[n.id()] = [
        (x - (side - 1) / 2) * step,
        (y - (side - 1) / 2) * step,
        (z - (side - 1) / 2) * step,
      ]
    })
    return result
  }
  if (algo === 'random') {
    const rand = mulberry32(7)
    const R = spacing * Math.cbrt(ids.length) * 0.9
    ids.forEach((id) => {
      result[id] = [(rand() * 2 - 1) * R, (rand() * 2 - 1) * R, (rand() * 2 - 1) * R]
    })
    return result
  }
  if (algo === 'groups3d') {
    // each ticked group becomes a ball of nodes (hubs in the middle); the
    // balls are placed by a 3D force layout of the group network and then
    // pushed apart until they no longer overlap
    const groups = getUsedGroups().filter((g) => S.activeGroups.has(g))
    const members = new Map(groups.map((g) => [g, []]))
    const memberOf = {}
    nodes.forEach((n) => {
      let gs = effectiveGroupsFor(n).filter((g) => members.has(g))
      if (!gs.length) {
        if (!members.has(UNGROUPED)) members.set(UNGROUPED, [])
        gs = [UNGROUPED]
      }
      memberOf[n.id()] = gs
      gs.forEach((g) => members.get(g).push(n))
    })
    const keys = [...members.keys()].filter((g) => members.get(g).length)
    const radius = new Map(
      keys.map((g) => [g, spacing * 0.55 * Math.cbrt(members.get(g).length) + spacing * 0.3])
    )
    const weight = new Map()
    edges.forEach((e) => {
      const ga = memberOf[e.data('source')],
        gb = memberOf[e.data('target')]
      if (!ga || !gb) return
      ga.forEach((a) =>
        gb.forEach((b) => {
          if (a === b) return
          const key = a < b ? a + '\u0000' + b : b + '\u0000' + a
          weight.set(key, (weight.get(key) || 0) + 1)
        })
      )
    })
    const metaEdges = [...weight].map(([key, w]) => {
      const [s, t] = key.split('\u0000')
      return { s, t, w }
    })
    const maxR = Math.max(...radius.values())
    const centers = fr3dLayout(keys, metaEdges, { spacing: maxR * 2.4, seed: 11, iterations: 300 })
    const gap = spacing * 0.6
    const needOf = (a, b) => radius.get(a) + radius.get(b) + gap
    if (keys.length > 1) {
      // scale so that each group's nearest neighbour is about touching,
      // then pull far-away groups (often unconnected ones) back in
      const ratios = keys
        .map((a) => {
          let best = Infinity,
            need = 1
          keys.forEach((b) => {
            if (a === b) return
            const d =
              Math.hypot(
                centers[a][0] - centers[b][0],
                centers[a][1] - centers[b][1],
                centers[a][2] - centers[b][2]
              ) || 0.01
            if (d / needOf(a, b) < best) {
              best = d / needOf(a, b)
              need = needOf(a, b)
            }
          })
          return best
        })
        .sort((x, y) => x - y)
      const factor = 1.15 / ratios[Math.floor(ratios.length / 2)]
      keys.forEach((g) => {
        centers[g] = centers[g].map((v) => v * factor)
      })
      keys.forEach((a) => {
        let nearest = null,
          best = Infinity
        keys.forEach((b) => {
          if (a === b) return
          const d = Math.hypot(
            centers[a][0] - centers[b][0],
            centers[a][1] - centers[b][1],
            centers[a][2] - centers[b][2]
          )
          if (d < best) {
            best = d
            nearest = b
          }
        })
        const need = needOf(a, nearest)
        if (best > 2.5 * need) {
          const t = 1 - (1.6 * need) / best
          centers[a] = centers[a].map((v, d) => v + (centers[nearest][d] - v) * t)
        }
      })
    }
    // separate overlapping balls
    for (let it = 0; it < 80; it++) {
      let moved = false
      for (let i = 0; i < keys.length; i++)
        for (let j = i + 1; j < keys.length; j++) {
          const a = centers[keys[i]],
            b = centers[keys[j]]
          const dx = b[0] - a[0],
            dy = b[1] - a[1],
            dz = b[2] - a[2]
          const d = Math.hypot(dx, dy, dz) || 0.01
          const need = radius.get(keys[i]) + radius.get(keys[j]) + gap
          if (d < need) {
            const push = (need - d) / 2 / d
            a[0] -= dx * push
            a[1] -= dy * push
            a[2] -= dz * push
            b[0] += dx * push
            b[1] += dy * push
            b[2] += dz * push
            moved = true
          }
        }
      if (!moved) break
    }
    const acc = {}
    keys.forEach((g) => {
      const list = [...members.get(g)].sort((a, b) => b.degree() - a.degree())
      const ball = fibonacciBall(list.length, radius.get(g))
      const c = centers[g]
      list.forEach((n, i) => {
        const id = n.id()
        ;(acc[id] = acc[id] || []).push([c[0] + ball[i][0], c[1] + ball[i][1], c[2] + ball[i][2]])
      })
    })
    Object.entries(acc).forEach(([id, list]) => {
      result[id] = [0, 1, 2].map((d) => list.reduce((s, p) => s + p[d], 0) / list.length)
    })
    return centerPositions(result)
  }
  return result
}

export async function runLayout3d(algo) {
  algo = algo || el3('layout3d').value
  const seq = ++net3d.layoutSeq
  el3('btnRun3d').disabled = true
  const big3d = shownNodes().length > 300
  setLayoutStatus3d(
    big3d ? [{ level: 'busy', text: 'Computing the 3D layout…', progress: null }] : []
  )
  net3d.onLayoutProgress = (f) => {
    if (big3d && seq === net3d.layoutSeq)
      setLayoutStatus3d([{ level: 'busy', text: 'Computing the 3D layout…', progress: f }])
  }
  await new Promise((r) => setTimeout(r, 20))
  try {
    const result = await computeLayout3d(algo)
    if (seq !== net3d.layoutSeq || !result) return
    net3d.spreadApplied = 0
    el3('spread3d').value = 0
    updateSpread3dReadout()
    animatePositions3d(result)
    net3d.hasLayout = true
    if (algo === 'layers' || algo === 'hierarchy' || algo === 'degree') {
      moveCamera3d({ ...VIEW_PRESETS_3D.tilted, pitch: 0.55 }, false)
    }
    setTimeout(() => fit3d(null, true), 520)
    setLayoutStatus3d([])
  } catch (err) {
    setLayoutStatus3d([
      { level: 'error', text: `The 3D layout couldn't be computed: ${err.message}` },
    ])
  } finally {
    if (seq === net3d.layoutSeq) el3('btnRun3d').disabled = false
  }
}

function animatePositions3d(target) {
  const ids = Object.keys(target)
  const from = new Map(ids.map((id) => [id, pos3dOf(id).slice()]))
  const start = performance.now()
  const step = (now) => {
    const t = Math.min(1, (now - start) / 500)
    const e = 1 - Math.pow(1 - t, 3)
    ids.forEach((id) => {
      const a = from.get(id),
        b = target[id]
      net3d.pos.set(id, [
        a[0] + (b[0] - a[0]) * e,
        a[1] + (b[1] - a[1]) * e,
        a[2] + (b[2] - a[2]) * e,
      ])
    })
    render3d()
    if (t < 1) requestAnimationFrame(step)
  }
  requestAnimationFrame(step)
}

export function updateSpread3dReadout() {
  const v = parseFloat(el3('spread3d').value) || 0
  el3('spread3dValue').textContent = Math.pow(2, v).toFixed(2) + '×'
}

export function applySpread3d() {
  const v = parseFloat(el3('spread3d').value) || 0
  const ratio = Math.pow(2, v - net3d.spreadApplied)
  net3d.spreadApplied = v
  updateSpread3dReadout()
  const ids = shownIds3d()
  if (!ids.length) return
  const c = [0, 0, 0]
  ids.forEach((id) => {
    const p = pos3dOf(id)
    c[0] += p[0]
    c[1] += p[1]
    c[2] += p[2]
  })
  c.forEach((x, i) => {
    c[i] = x / ids.length
  })
  ids.forEach((id) => {
    const p = pos3dOf(id)
    net3d.pos.set(id, [
      c[0] + (p[0] - c[0]) * ratio,
      c[1] + (p[1] - c[1]) * ratio,
      c[2] + (p[2] - c[2]) * ratio,
    ])
  })
  requestRender3d()
}
