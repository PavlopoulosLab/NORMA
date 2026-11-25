// @ts-nocheck
/* ---------- "Separate nodes": push overlapping nodes slightly apart ----------
   Nodes closer than the sum of their radii (plus a small gap) are moved
   away from each other along the line between their centres, in a few
   relaxation passes over a spatial grid. Nothing else moves, so the
   arrangement a layout produced is kept; nodes exactly on top of each
   other are pushed in a fixed direction so the result is reproducible.
   Works on the shown nodes of the 2D view and of the 3D view. */
import { cy } from './cy'
import { setStatus } from './layouts/controls'
import { shownNodes } from './metrics'
import { animatePositions3d, setLayoutStatus3d } from './view3d/layouts'
import { el3, net3d } from './view3d/state'
import { pos3dOf } from './view3d/cache'
import { shownIds3d } from './view3d/camera'

const GAP = 6 // pixels kept free between two nodes
const PASSES = 40

// positions: Float64Array of dims * n; radii: per node. Returns the number of
// nodes that moved.
function separate(positions, radii, dims) {
  const n = radii.length
  if (n < 2) return 0
  const maxR = radii.reduce((a, r) => Math.max(a, r), 0)
  const cell = Math.max(2 * maxR + GAP, 1)
  const moved = new Uint8Array(n)
  const key = (i) => {
    let k = ''
    for (let d = 0; d < dims; d++) k += Math.floor(positions[i * dims + d] / cell) + ','
    return k
  }
  const neighbours = (i) => {
    const out = []
    const base = []
    for (let d = 0; d < dims; d++) base.push(Math.floor(positions[i * dims + d] / cell))
    const offsets = dims === 2 ? [-1, 0, 1] : [-1, 0, 1]
    const walk = (d, acc) => {
      if (d === dims) {
        out.push(acc.join(',') + ',')
        return
      }
      for (const o of offsets) walk(d + 1, [...acc, base[d] + o])
    }
    walk(0, [])
    return out
  }
  for (let pass = 0; pass < PASSES; pass++) {
    const grid = new Map()
    for (let i = 0; i < n; i++) {
      const k = key(i)
      let list = grid.get(k)
      if (!list) grid.set(k, (list = []))
      list.push(i)
    }
    let any = false
    for (let i = 0; i < n; i++) {
      for (const k of neighbours(i)) {
        const list = grid.get(k)
        if (!list) continue
        for (const j of list) {
          if (j <= i) continue
          const want = radii[i] + radii[j] + GAP
          const delta = []
          let d2 = 0
          for (let d = 0; d < dims; d++) {
            const v = positions[i * dims + d] - positions[j * dims + d]
            delta.push(v)
            d2 += v * v
          }
          let dist = Math.sqrt(d2)
          if (dist >= want - 1e-6) continue
          if (dist < 1e-9) {
            // coincident: separate along a fixed axis, alternating sides
            delta.fill(0)
            delta[0] = (i + j) % 2 ? 1 : -1
            dist = 1
          }
          const push = (want - dist) / 2 / dist
          for (let d = 0; d < dims; d++) {
            positions[i * dims + d] += delta[d] * push
            positions[j * dims + d] -= delta[d] * push
          }
          moved[i] = moved[j] = 1
          any = true
        }
      }
    }
    if (!any) break
  }
  let count = 0
  for (let i = 0; i < n; i++) count += moved[i]
  return count
}

export function separateNodes2d() {
  const nodes = shownNodes()
  if (nodes.length < 2) return
  const pos = new Float64Array(nodes.length * 2)
  const radii = nodes.map((n, i) => {
    const p = n.position()
    pos[2 * i] = p.x
    pos[2 * i + 1] = p.y
    return n.outerWidth() / 2
  })
  const count = separate(pos, radii, 2)
  if (!count) {
    setStatus('layoutStatus', [{ level: 'ok', text: 'No nodes overlap.' }])
    return
  }
  const positions = {}
  nodes.forEach((n, i) => {
    positions[n.id()] = { x: pos[2 * i], y: pos[2 * i + 1] }
  })
  nodes
    .layout({
      name: 'preset',
      positions: (node) => positions[node.id()] || node.position(),
      animate: nodes.length <= 1500,
      animationDuration: 400,
      fit: false,
    })
    .run()
  setStatus('layoutStatus', [{ level: 'ok', text: `Moved ${count} overlapping nodes apart.` }])
}

export function separateNodes3d() {
  const ids = shownIds3d()
  if (ids.length < 2) return
  const pos = new Float64Array(ids.length * 3)
  const radii = ids.map((id, i) => {
    const p = pos3dOf(id)
    pos[3 * i] = p[0]
    pos[3 * i + 1] = p[1]
    pos[3 * i + 2] = p[2]
    const n = cy.getElementById(id)
    return (n.length ? n.width() : 40) / 2
  })
  const count = separate(pos, radii, 3)
  if (!count) {
    setLayoutStatus3d([{ level: 'ok', text: 'No nodes overlap.' }])
    return
  }
  const target = {}
  ids.forEach((id, i) => {
    target[id] = [pos[3 * i], pos[3 * i + 1], pos[3 * i + 2]]
  })
  animatePositions3d(target)
  net3d.hasLayout = true
  setLayoutStatus3d([{ level: 'ok', text: `Moved ${count} overlapping nodes apart.` }])
}

export function init() {
  document.getElementById('btnDeoverlap').addEventListener('click', separateNodes2d)
  el3('btnDeoverlap3d').addEventListener('click', separateNodes3d)
}
