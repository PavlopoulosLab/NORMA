// @ts-nocheck
// ponytail: split verbatim from the former single-file script; typed one file at a time (remove @ts-nocheck as it lands)
import { VALUE_SCALES } from './mcl'
import { applyValueColors, scheduleLegend, scheduleSeparation } from './mapping'
import { benchmarkTsv, runLayoutBenchmark } from '../benchmark'
import { buildGroupLegend } from '../hulls'
import { cy, onCommitStyle } from '../cy'

/* ---------- wiring: values, legend, separation, benchmark ---------- */
export function updateValueScalePreview() {
  const sc = VALUE_SCALES[document.getElementById('valueScale').value] || VALUE_SCALES['div-rdbu']
  document.getElementById('valueScalePreview').style.background =
    `linear-gradient(90deg, ${sc.stops.join(', ')})`
}

/* ============================================================
   BUBBLE SETS
   Group contours that hug their members (after Collins, Penn and
   Carpendale, "Bubble Sets", IEEE TVCG 2009). For each group an energy
   field is built on a grid: positive around member nodes and along
   "virtual edges" that tie the members together (a minimum spanning
   tree, so the contour stays one piece), negative around nodes that are
   not members, so the contour bends away from them. The outline is the
   iso-line of that field (marching squares), smoothed.
   Works in any coordinate system: pass positions and radii in the units
   the result should be drawn in.
   ============================================================ */
const BUBBLE_THRESHOLD = 0.45

// members / others: [{ x, y, r }]; opts: { margin, edgeRadius, avoid, cell }
export function bubbleContour(members, others, opts) {
  if (!members.length) return []
  const margin = opts.margin,
    edgeR = opts.edgeRadius,
    avoid = opts.avoid
  let x1 = Infinity,
    y1 = Infinity,
    x2 = -Infinity,
    y2 = -Infinity,
    maxR = 0
  members.forEach((p) => {
    x1 = Math.min(x1, p.x - p.r)
    y1 = Math.min(y1, p.y - p.r)
    x2 = Math.max(x2, p.x + p.r)
    y2 = Math.max(y2, p.y + p.r)
    maxR = Math.max(maxR, p.r)
  })
  const pad = margin * 1.6 + 2
  x1 -= pad
  y1 -= pad
  x2 += pad
  y2 += pad
  // grid resolution: fine enough for the margin, capped for speed
  let cell = opts.cell || Math.max(margin / 3, 0.5)
  const maxCells = opts.maxCells || 250000
  while (((x2 - x1) / cell) * ((y2 - y1) / cell) > maxCells) cell *= 1.25
  const nx = Math.ceil((x2 - x1) / cell) + 1,
    ny = Math.ceil((y2 - y1) / cell) + 1
  const F = new Float32Array(nx * ny)

  // positive energy around members: 1 inside the node, fading to 0 at `margin`
  const splatDisc = (p, reach, weight) => {
    const R = p.r + reach
    const i0 = Math.max(0, Math.floor((p.x - R - x1) / cell)),
      i1 = Math.min(nx - 1, Math.ceil((p.x + R - x1) / cell))
    const j0 = Math.max(0, Math.floor((p.y - R - y1) / cell)),
      j1 = Math.min(ny - 1, Math.ceil((p.y + R - y1) / cell))
    for (let j = j0; j <= j1; j++) {
      const gy = y1 + j * cell,
        dy = gy - p.y
      for (let i = i0; i <= i1; i++) {
        const gx = x1 + i * cell,
          dx = gx - p.x
        const d = Math.sqrt(dx * dx + dy * dy) - p.r
        if (d >= reach) continue
        const t = d <= 0 ? 1 : 1 - d / reach
        F[j * nx + i] += weight * t * t
      }
    }
  }
  members.forEach((p) => splatDisc(p, margin, 1))

  // virtual edges: a minimum spanning tree over the members (Prim)
  const m = members.length
  const segs = []
  if (m > 1 && m <= 2500) {
    const inTree = new Uint8Array(m),
      best = new Float64Array(m).fill(Infinity),
      from = new Int32Array(m).fill(-1)
    best[0] = 0
    for (let k = 0; k < m; k++) {
      let u = -1,
        bu = Infinity
      for (let v = 0; v < m; v++)
        if (!inTree[v] && best[v] < bu) {
          bu = best[v]
          u = v
        }
      if (u < 0) break
      inTree[u] = 1
      if (from[u] >= 0) segs.push([members[from[u]], members[u]])
      const pu = members[u]
      for (let v = 0; v < m; v++) {
        if (inTree[v]) continue
        const d = Math.hypot(members[v].x - pu.x, members[v].y - pu.y)
        if (d < best[v]) {
          best[v] = d
          from[v] = u
        }
      }
    }
  } else if (opts.links) {
    opts.links.forEach(([a, b]) => segs.push([a, b]))
  }
  segs.forEach(([a, b]) => {
    const R = edgeR
    const i0 = Math.max(0, Math.floor((Math.min(a.x, b.x) - R - x1) / cell)),
      i1 = Math.min(nx - 1, Math.ceil((Math.max(a.x, b.x) + R - x1) / cell))
    const j0 = Math.max(0, Math.floor((Math.min(a.y, b.y) - R - y1) / cell)),
      j1 = Math.min(ny - 1, Math.ceil((Math.max(a.y, b.y) + R - y1) / cell))
    const vx = b.x - a.x,
      vy = b.y - a.y,
      L2 = vx * vx + vy * vy || 1
    for (let j = j0; j <= j1; j++) {
      const gy = y1 + j * cell
      for (let i = i0; i <= i1; i++) {
        const gx = x1 + i * cell
        const t = Math.max(0, Math.min(1, ((gx - a.x) * vx + (gy - a.y) * vy) / L2))
        const d = Math.hypot(gx - (a.x + t * vx), gy - (a.y + t * vy))
        if (d >= R) continue
        const s = 1 - d / R
        // take the stronger of node and edge energy so tree links don't bloat the nodes
        const idx = j * nx + i
        F[idx] = Math.max(F[idx], s * s)
      }
    }
  })

  // negative energy around non-members, strongest at their rim
  others.forEach((p) => {
    if (
      p.x + p.r + avoid < x1 ||
      p.x - p.r - avoid > x2 ||
      p.y + p.r + avoid < y1 ||
      p.y - p.r - avoid > y2
    )
      return
    const R = p.r + avoid
    const i0 = Math.max(0, Math.floor((p.x - R - x1) / cell)),
      i1 = Math.min(nx - 1, Math.ceil((p.x + R - x1) / cell))
    const j0 = Math.max(0, Math.floor((p.y - R - y1) / cell)),
      j1 = Math.min(ny - 1, Math.ceil((p.y + R - y1) / cell))
    for (let j = j0; j <= j1; j++) {
      const gy = y1 + j * cell,
        dy = gy - p.y
      for (let i = i0; i <= i1; i++) {
        const gx = x1 + i * cell,
          dx = gx - p.x
        const d = Math.sqrt(dx * dx + dy * dy) - p.r
        if (d >= avoid) continue
        const t = d <= 0 ? 1 : 1 - d / avoid
        F[j * nx + i] -= 0.9 * t * t
      }
    }
  })
  // members always stay inside their own contour
  members.forEach((p) => {
    const i = Math.round((p.x - x1) / cell),
      j = Math.round((p.y - y1) / cell)
    for (let b = -1; b <= 1; b++)
      for (let a = -1; a <= 1; a++) {
        const ii = i + a,
          jj = j + b
        if (ii >= 0 && jj >= 0 && ii < nx && jj < ny) F[jj * nx + ii] = Math.max(F[jj * nx + ii], 1)
      }
  })

  return marchingSquares(F, nx, ny, x1, y1, cell, BUBBLE_THRESHOLD)
}

// Iso-lines of a grid as closed loops of points.
function marchingSquares(F, nx, ny, x0, y0, cell, T) {
  // edge ids: horizontal edge (i, j)->(i+1, j) = 2 * (j * nx + i); vertical = +1
  const point = new Map()
  const edgePoint = (i, j, horizontal) => {
    const key = 2 * (j * nx + i) + (horizontal ? 0 : 1)
    let p = point.get(key)
    if (p) return key
    const a = F[j * nx + i]
    const b = horizontal ? F[j * nx + i + 1] : F[(j + 1) * nx + i]
    const t = Math.abs(b - a) < 1e-9 ? 0.5 : (T - a) / (b - a)
    p = horizontal
      ? { x: x0 + (i + t) * cell, y: y0 + j * cell }
      : { x: x0 + i * cell, y: y0 + (j + t) * cell }
    point.set(key, p)
    return key
  }
  const next = new Map() // directed segments: from edge -> to edge
  for (let j = 0; j < ny - 1; j++) {
    for (let i = 0; i < nx - 1; i++) {
      const vtl = F[j * nx + i],
        vtr = F[j * nx + i + 1],
        vbr = F[(j + 1) * nx + i + 1],
        vbl = F[(j + 1) * nx + i]
      const tl = vtl >= T,
        tr = vtr >= T,
        br = vbr >= T,
        bl = vbl >= T
      const code = (tl ? 8 : 0) | (tr ? 4 : 0) | (br ? 2 : 0) | (bl ? 1 : 0)
      if (code === 0 || code === 15) continue
      const cx0 = x0 + i * cell,
        cy0 = y0 + j * cell,
        cx1 = cx0 + cell,
        cy1 = cy0 + cell
      // orient a segment with a reference corner whose side is known: the
      // inside always ends up on the same side of the walking direction
      const link = (a, b, px, py, inside) => {
        const pa = point.get(a),
          pb = point.get(b)
        const cross = (pb.x - pa.x) * (py - pa.y) - (pb.y - pa.y) * (px - pa.x)
        if (cross > 0 === inside) next.set(a, b)
        else next.set(b, a)
      }
      const top = () => edgePoint(i, j, true),
        bottom = () => edgePoint(i, j + 1, true)
      const left = () => edgePoint(i, j, false),
        right = () => edgePoint(i + 1, j, false)
      const TL = (s) => (s ? link(left(), top(), cx0, cy0, tl) : null)
      switch (code) {
        case 1:
        case 14:
          link(left(), bottom(), cx0, cy1, bl)
          break
        case 2:
        case 13:
          link(bottom(), right(), cx1, cy1, br)
          break
        case 3:
        case 12:
          link(left(), right(), cx0, cy0, tl)
          break
        case 4:
        case 11:
          link(right(), top(), cx1, cy0, tr)
          break
        case 6:
        case 9:
          link(bottom(), top(), cx0, cy0, tl)
          break
        case 7:
        case 8:
          link(left(), top(), cx0, cy0, tl)
          break
        case 5:
        case 10: {
          const centre = (vtl + vtr + vbr + vbl) / 4 >= T
          // saddle: the two corners that differ from the centre are cut off
          if (centre === tl) {
            link(left(), bottom(), cx0, cy1, bl)
            link(top(), right(), cx1, cy0, tr)
          } else {
            link(left(), top(), cx0, cy0, tl)
            link(bottom(), right(), cx1, cy1, br)
          }
          break
        }
      }
    }
  }
  const loops = []
  const used = new Set()
  next.forEach((_, start) => {
    if (used.has(start)) return
    const loop = []
    let cur = start
    while (cur !== undefined && !used.has(cur)) {
      used.add(cur)
      loop.push(point.get(cur))
      cur = next.get(cur)
    }
    if (loop.length >= 3) {
      loop.closed = cur === start
      loops.push(loop)
    }
  })
  return loops
}

// Smooth closed path (quadratic curves through edge midpoints), simplified first.
export function bubblePathD(loops, map) {
  const f = (v) => Math.round(v * 100) / 100
  let d = ''
  loops.forEach((loop) => {
    // drop nearly collinear points to keep paths small
    const pts = []
    for (let i = 0; i < loop.length; i++) {
      const p = map ? map(loop[i]) : loop[i]
      const last = pts[pts.length - 1]
      if (!last || Math.hypot(p.x - last.x, p.y - last.y) > 0.8) pts.push(p)
    }
    if (pts.length < 3) return
    const n = pts.length
    const mid = (a, b) => ({ x: (a.x + b.x) / 2, y: (a.y + b.y) / 2 })
    const m0 = mid(pts[n - 1], pts[0])
    d += `M${f(m0.x)},${f(m0.y)}`
    for (let i = 0; i < n; i++) {
      const p = pts[i],
        q = mid(p, pts[(i + 1) % n])
      d += `Q${f(p.x)},${f(p.y)} ${f(q.x)},${f(q.y)}`
    }
    d += 'Z'
  })
  return d
}

// page wiring, run by main.ts in the original order
export function init() {
  ;[
    'valueColumn',
    'valueTransform',
    'valueScale',
    'valueCenter',
    'valueRange',
    'valueMin',
    'valueMax',
    'valueMissing',
  ].forEach((id) => {
    const el = document.getElementById(id)
    const run = () => {
      updateValueScalePreview()
      applyValueColors()
    }
    el.addEventListener('change', run)
    el.addEventListener('input', run)
  })

  updateValueScalePreview()

  ;[
    'legendShow',
    'legendScale',
    'legendGroups',
    'legendShapes',
    'legendChannels',
    'legendTitle',
  ].forEach((id) => {
    document.getElementById(id).addEventListener('input', scheduleLegend)
    document.getElementById(id).addEventListener('change', scheduleLegend)
  })

  document.getElementById('sizeMetric').addEventListener('change', scheduleLegend)

  document.getElementById('showGroupHulls').addEventListener('change', scheduleLegend)

  document.getElementById('groupSortSelect').addEventListener('change', () => {
    buildGroupLegend()
    scheduleLegend()
  })

  document.getElementById('spreadSlider').addEventListener('input', scheduleSeparation)

  document.getElementById('btnBench').addEventListener('click', runLayoutBenchmark)

  document.getElementById('btnBenchTsv').addEventListener('click', benchmarkTsv)

  {
    onCommitStyle(scheduleLegend)
  }

  cy.on('data', () => {
    if (
      !document.getElementById('legendPanel').hidden ||
      document.getElementById('legendShow').checked
    )
      scheduleLegend()
  })
}
