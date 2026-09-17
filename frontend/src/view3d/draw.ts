// @ts-nocheck
// ponytail: split verbatim from the former single-file script; typed one file at a time (remove @ts-nocheck as it lands)
import { S } from '../state'
import { UNGROUPED, getUsedGroups } from '../network_state'
import { blobPath, capsulePath } from '../export/shading'
import { bubbleContour, bubblePathD } from '../clustering/wiring'
import { buildCache3d, pos3dOf } from './cache'
import { canvas3d, el3, mixColor, net3d } from './state'
import { canvasPainter } from './painters'
import { convexHull, inflateHull } from '../hulls'
import { currentTheme } from '../themes'
import { cy } from '../cy'
import { makeProjector, updateZoomReadout3d } from './camera'

/* ---------- drawing ---------- */
function nodeShapePoints(shape, x, y, r) {
  const def = cy.renderer().nodeShapes[shape]
  if (!def || !def.points) return null
  const pts = []
  for (let i = 0; i < def.points.length; i += 2)
    pts.push([x + def.points[i] * r, y + def.points[i + 1] * r])
  return pts
}

export function drawScene3d(painter, W, H, opts) {
  if (net3d.cacheDirty || !net3d.cache) buildCache3d()
  const { nodes, edges, byId } = net3d.cache
  const cam = opts.cam || net3d.cam
  const project = makeProjector(W, H, cam)
  const fast = !!opts.fast
  const bg = opts.bg
  painter.clear(W, H, bg)
  const fogColor = opts.fogColor || currentTheme.bg
  const fogAmount = parseFloat(el3('fog3d').value) || 0
  const shaded = el3('style3d').value === 'shaded'
  const k = opts.pixelScale || 1 // extra scaling for high-resolution export

  const T = { t: performance.now() }
  const mark = (k) => {
    const now = performance.now()
    T[k] = Math.round(now - T.t)
    T.t = now
  }
  // project nodes
  const P = new Map()
  let dMin = Infinity,
    dMax = -Infinity
  nodes.forEach((nd) => {
    const pr = project(pos3dOf(nd.id))
    if (!pr) return
    pr.x *= k
    pr.y *= k
    pr.scale *= k
    P.set(nd.id, pr)
    if (pr.depth < dMin) dMin = pr.depth
    if (pr.depth > dMax) dMax = pr.depth
  })
  const span = Math.max(1e-6, dMax - dMin)
  const fogOf = (depth) => (fogAmount * 0.75 * (depth - dMin)) / span

  mark('project')
  const floorPx = labelFloor3d() * k
  const nodeLabelMin = labelCutoff3d(
    nodes.filter((nd) => nd.label && P.has(nd.id)).map((nd) => nd.fontPx * P.get(nd.id).scale),
    floorPx,
    500
  )
  const edgeLabelMin = labelCutoff3d(
    edges
      .filter((e) => e.label && P.has(e.s) && P.has(e.t))
      .map((e) => (e.fontPx * (P.get(e.s).scale + P.get(e.t).scale)) / 2),
    floorPx,
    300
  )
  // floor grid and axes
  if (el3('grid3d').checked && nodes.length) drawFloor3d(painter, project, k)

  // group shading, from the projected positions
  if (document.getElementById('showGroupHulls').checked) {
    const style = document.getElementById('hullStyle').value
    const opacity = parseFloat(document.getElementById('hullOpacity').value) || 0.25
    const groups = getUsedGroups().filter((g) => S.activeGroups.has(g) && g !== UNGROUPED)
    const bubbles = style === 'bubble' && !fast
    const projected = bubbles
      ? nodes
          .filter((nd) => P.has(nd.id))
          .map((nd) => {
            const pr = P.get(nd.id)
            return { x: pr.x, y: pr.y, r: (nd.size / 2) * pr.scale, groups: nd.groups }
          })
      : null
    groups.forEach((g) => {
      const pts = []
      nodes.forEach((nd) => {
        if (!nd.groups.includes(g)) return
        const pr = P.get(nd.id)
        if (pr) pts.push({ x: pr.x, y: pr.y, r: (nd.size / 2) * pr.scale })
      })
      if (!pts.length) return
      const color = S.nodeColorMap[g] || '#888888'
      if (bubbles) {
        const loops = bubbleContour(
          pts,
          projected.filter((p) => !p.groups.includes(g)),
          { margin: 30 * k, edgeRadius: 21 * k, avoid: 12 * k, cell: 3 * k, maxCells: 120000 }
        )
        const d = bubblePathD(loops)
        if (d)
          painter.pathD(d, {
            fill: color,
            fillAlpha: opacity,
            stroke: color,
            strokeAlpha: Math.min(1, opacity + 0.45),
            width: 1.6 * k,
            evenodd: true,
          })
        return
      }
      const pad = 18 * k
      let d
      if (pts.length === 1)
        d = `M${pts[0].x - pts[0].r - pad},${pts[0].y}a${pts[0].r + pad},${pts[0].r + pad} 0 1 0 ${2 * (pts[0].r + pad)},0a${pts[0].r + pad},${pts[0].r + pad} 0 1 0 ${-2 * (pts[0].r + pad)},0Z`
      else if (pts.length === 2) d = capsulePath(pts[0], pts[1], Math.max(pts[0].r, pts[1].r) + pad)
      else d = blobPath(inflateHull(convexHull(pts), pad + Math.max(...pts.map((p) => p.r))))
      painter.pathD(
        d,
        style === 'fog'
          ? { fill: color, fillAlpha: opacity, blur: 14 * k }
          : {
              fill: color,
              fillAlpha: opacity,
              stroke: color,
              strokeAlpha: Math.min(1, opacity + 0.35),
              width: 1.5 * k,
            }
      )
    })
  }

  mark('hulls')
  // edges, far to near
  const curveStyle = document.getElementById('edgeCurveStyle').value
  const fan = curveStyle === 'bezier' || curveStyle === 'bundled'
  const fanStep = (parseInt(document.getElementById('edgeCurvature').value, 10) || 40) * 0.6 * k
  const edgeList = []
  edges.forEach((e) => {
    const a = P.get(e.s),
      b = P.get(e.t)
    if (!a || !b) return
    edgeList.push({ e, a, b, depth: (a.depth + b.depth) / 2 })
  })
  edgeList.sort((p, q) => q.depth - p.depth)
  const projEdges = []
  const labelsLater = []
  edgeList.forEach(({ e, a, b, depth }) => {
    const fog = fogOf(depth)
    const color = mixColor(e.color, fogColor, fog)
    const avgScale = (a.scale + b.scale) / 2
    const width = Math.max(0.6 * k, Math.min(e.width * avgScale, 12 * k))
    let cx, cy0
    if (fan && e.siblings > 1) {
      const off = (e.index - (e.siblings - 1) / 2) * fanStep * Math.min(1.5, avgScale)
      // perpendicular in a direction that doesn't depend on edge orientation
      const [p, q] = e.s < e.t ? [a, b] : [b, a]
      const dx = q.x - p.x,
        dy = q.y - p.y,
        len = Math.hypot(dx, dy) || 1
      cx = (a.x + b.x) / 2 - (dy / len) * off
      cy0 = (a.y + b.y) / 2 + (dx / len) * off
    }
    // shorten the line at the target so an arrow sits on the node's rim
    let x2 = b.x,
      y2 = b.y
    const rb = (byId.get(e.t).size / 2) * b.scale
    if (e.arrow) {
      const fx = cx !== undefined ? cx : a.x,
        fy = cx !== undefined ? cy0 : a.y
      const dx = b.x - fx,
        dy = b.y - fy,
        len = Math.hypot(dx, dy) || 1
      const ux = dx / len,
        uy = dy / len
      const tipX = b.x - ux * rb,
        tipY = b.y - uy * rb
      const size = Math.max(Math.pow(e.width * 13.37, 0.9), 29) * e.arrowScale * avgScale
      x2 = tipX - ux * size * 0.25
      y2 = tipY - uy * size * 0.25
      if (e.underlay) painter.line(a.x, a.y, x2, y2, e.underlay, width + 6 * k, 0.4, cx, cy0)
      painter.line(a.x, a.y, x2, y2, color, width, e.opacity, cx, cy0)
      const def = cy.renderer().arrowShapes[e.arrow]
      if (def && def.points && !fast) {
        const pts = []
        for (let i = 0; i < def.points.length; i += 2) {
          const px = def.points[i] * size,
            py = def.points[i + 1] * size
          pts.push([tipX - uy * px + ux * py, tipY + ux * px + uy * py])
        }
        painter.poly(pts, mixColor(e.arrowColor, fogColor, fog), e.opacity)
      }
    } else {
      if (e.underlay) painter.line(a.x, a.y, x2, y2, e.underlay, width + 6 * k, 0.4, cx, cy0)
      painter.line(a.x, a.y, x2, y2, color, width, e.opacity, cx, cy0)
    }
    const mx = cx !== undefined ? 0.25 * a.x + 0.5 * cx + 0.25 * b.x : (a.x + b.x) / 2
    const my = cx !== undefined ? 0.25 * a.y + 0.5 * cy0 + 0.25 * b.y : (a.y + b.y) / 2
    projEdges.push({ id: e.id, ax: a.x, ay: a.y, bx: b.x, by: b.y, cx, cy: cy0, mx, my, width })
    if (e.label && !fast) {
      const size = e.fontPx * avgScale
      if (size >= edgeLabelMin) {
        let angle = 0
        if (e.rotate) {
          angle = Math.atan2(b.y - a.y, b.x - a.x)
          if (angle > Math.PI / 2 || angle < -Math.PI / 2) angle += Math.PI
        }
        labelsLater.push({
          str: e.label,
          x: mx,
          y: my,
          o: {
            size,
            color: e.labelColor,
            alpha: e.opacity,
            align: 'center',
            baseline: 'middle',
            rotate: angle,
            bg: e.labelBg,
          },
        })
      }
    }
  })

  mark('edges')
  // nodes, far to near
  const order = nodes
    .filter((nd) => P.has(nd.id))
    .sort((p, q) => P.get(q.id).depth - P.get(p.id).depth)
  const proj = []
  order.forEach((nd) => {
    const pr = P.get(nd.id)
    const r = Math.max(1.2 * k, (nd.size / 2) * pr.scale)
    const fog = fogOf(pr.depth)
    const alpha = nd.opacity
    if (nd.underlay)
      painter.circle(
        pr.x,
        pr.y,
        r + nd.underlay.pad * pr.scale,
        nd.underlay.color,
        nd.underlay.alpha * alpha
      )
    const bw = nd.borderW > 0 ? Math.max(0.5 * k, Math.min(nd.borderW * pr.scale, 8 * k)) : 0
    const border = mixColor(nd.border, fogColor, fog)
    if (nd.shape === 'ellipse' || fast || r < 2.5 * k) {
      if (nd.pies.length > 1 && !fast && r > 3 * k) {
        painter.circle(pr.x, pr.y, r, mixColor(nd.color, fogColor, fog), alpha)
        let a0 = -Math.PI / 2
        nd.pies.forEach(([c, share]) => {
          const a1 = a0 + share * Math.PI * 2
          painter.wedge(pr.x, pr.y, r, a0, a1, mixColor(c, fogColor, fog), alpha)
          a0 = a1
        })
        if (bw) painter.circle(pr.x, pr.y, r, null, alpha, border, bw)
      } else {
        const fill = nd.pies.length === 1 ? nd.pies[0][0] : nd.color
        painter.circle(pr.x, pr.y, r, mixColor(fill, fogColor, fog), alpha, bw ? border : null, bw)
      }
      if (shaded && !fast && r > 3 * k) painter.shade(pr.x, pr.y, r, alpha)
    } else {
      const pts =
        nodeShapePoints(nd.shape, pr.x, pr.y, r) || nodeShapePoints('rectangle', pr.x, pr.y, r)
      painter.poly(pts, mixColor(nd.color, fogColor, fog), alpha, bw ? border : null, bw)
    }
    proj.push({ id: nd.id, x: pr.x, y: pr.y, r, depth: pr.depth, scale: pr.scale })
    if (nd.label && !fast) {
      const size = nd.fontPx * pr.scale
      if (size >= nodeLabelMin) {
        let x = pr.x + nd.marginX * pr.scale,
          y = pr.y + nd.marginY * pr.scale
        let align = 'center',
          baseline = 'middle'
        if (nd.valign === 'top') {
          y = pr.y - r + nd.marginY * pr.scale
          baseline = 'bottom'
        } else if (nd.valign === 'bottom') {
          y = pr.y + r + nd.marginY * pr.scale
          baseline = 'top'
        }
        if (nd.halign === 'left') {
          x = pr.x - r + nd.marginX * pr.scale
          align = 'right'
        } else if (nd.halign === 'right') {
          x = pr.x + r + nd.marginX * pr.scale
          align = 'left'
        }
        painter.text(nd.label, x, y, {
          size,
          color: mixColor(nd.labelColor, fogColor, fog * 0.6),
          alpha,
          align,
          baseline,
          halo: nd.halo,
          haloWidth: nd.haloW * Math.min(1.5, pr.scale),
        })
      }
    }
  })
  mark('nodes')
  labelsLater.forEach((l) => painter.text(l.str, l.x, l.y, l.o))
  painter.done()
  mark('labels')
  net3d.timing = T
  if (!opts.offscreen) {
    net3d.proj = proj.reverse() // nearest first, for picking
    net3d.projEdges = projEdges
  }
}

// Smallest label size worth drawing: the user's floor, raised so that at
// most `budget` labels are drawn (the largest, i.e. nearest, ones win).
function labelCutoff3d(sizes, floor, budget) {
  if (sizes.length <= budget) return floor
  const sorted = sizes.filter((v) => v >= floor).sort((a, b) => b - a)
  return sorted.length > budget ? Math.max(floor, sorted[budget]) + 1e-9 : floor
}

function labelFloor3d() {
  const min = parseFloat(document.getElementById('labelMinScreenSize').value) || 0
  return Math.max(3, min)
}

function drawFloor3d(painter, project, k) {
  // a square grid under the network, plus short x/y/z axes at the centre
  let minY = Infinity,
    maxY = -Infinity,
    R = 0
  net3d.cache.nodes.forEach((nd) => {
    const p = pos3dOf(nd.id)
    if (p[1] < minY) minY = p[1]
    if (p[1] > maxY) maxY = p[1]
    R = Math.max(R, Math.hypot(p[0] - net3d.cam.tx, p[2] - net3d.cam.tz))
  })
  const floorY = maxY + 40
  const half = Math.ceil((R + 60) / 100) * 100
  const step = half / 5
  const color = currentTheme.muted
  for (let i = -5; i <= 5; i++) {
    const v = i * step
    const a = project([net3d.cam.tx + v, floorY, net3d.cam.tz - half]),
      b = project([net3d.cam.tx + v, floorY, net3d.cam.tz + half])
    const c = project([net3d.cam.tx - half, floorY, net3d.cam.tz + v]),
      d = project([net3d.cam.tx + half, floorY, net3d.cam.tz + v])
    if (a && b) painter.line(a.x * k, a.y * k, b.x * k, b.y * k, color, k, 0.18)
    if (c && d) painter.line(c.x * k, c.y * k, d.x * k, d.y * k, color, k, 0.18)
  }
  const o = [net3d.cam.tx, net3d.cam.ty, net3d.cam.tz]
  const len = step
  ;[
    [[len, 0, 0], '#e05252', 'x'],
    [[0, -len, 0], '#3fae5a', 'y'],
    [[0, 0, len], '#3b7ddd', 'z'],
  ].forEach(([v, col, name]) => {
    const a = project(o),
      b = project([o[0] + v[0], o[1] + v[1], o[2] + v[2]])
    if (!a || !b) return
    painter.line(a.x * k, a.y * k, b.x * k, b.y * k, col, 2 * k, 0.8)
    painter.text(name, b.x * k, b.y * k, {
      size: 11 * k,
      color: col,
      alpha: 0.9,
      align: 'center',
      baseline: 'middle',
    })
  })
}

export function render3d() {
  if (!net3d.active) return
  const cv = canvas3d()
  const rect = cv.getBoundingClientRect()
  const dpr = window.devicePixelRatio || 1
  const W = Math.max(1, Math.round(rect.width)),
    H = Math.max(1, Math.round(rect.height))
  if (cv.width !== Math.round(W * dpr) || cv.height !== Math.round(H * dpr)) {
    cv.width = Math.round(W * dpr)
    cv.height = Math.round(H * dpr)
  }
  const ctx = cv.getContext('2d')
  ctx.setTransform(dpr, 0, 0, dpr, 0, 0)
  const big =
    (net3d.cache ? net3d.cache.nodes.length + net3d.cache.edges.length : cy.elements().length) >
    3000
  drawScene3d(canvasPainter(ctx), W, H, { fast: net3d.interacting && big })
  updateZoomReadout3d()
}
