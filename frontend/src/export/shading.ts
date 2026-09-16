// @ts-nocheck
// ponytail: split verbatim from the former single-file script; typed one file at a time (remove @ts-nocheck as it lands)
import { S } from '../state'
import { UNGROUPED, effectiveGroupsFor, getUsedGroups } from '../network_state'
import { bubblePathsModel } from '../contours'
import { convexHull, hexToRgba, inflateHull } from '../hulls'
import { cy } from '../cy'
import { hullMarginModel } from './draw'

/* ---------- group shading geometry, in output coordinates ---------- */
export function hullShapes(toOut, k) {
  if (!document.getElementById('showGroupHulls').checked) return []
  const style = document.getElementById('hullStyle').value
  const opacity = parseFloat(document.getElementById('hullOpacity').value) || 0.25
  const margin = hullMarginModel() * k
  const shapes = []
  if (style === 'bubble') {
    const paths = bubblePathsModel(true)
    paths.forEach((d, g) => {
      if (d)
        shapes.push({
          kind: 'path',
          color: S.nodeColorMap[g] || '#888888',
          d: transformPathD(d, toOut),
        })
    })
    return shapes.map((s) => ({ ...s, style, opacity }))
  }
  getUsedGroups()
    .filter((g) => S.activeGroups.has(g) && g !== UNGROUPED)
    .forEach((g) => {
      const nodes = cy
        .nodes()
        .filter((n) => !n.hasClass('hidden-group') && effectiveGroupsFor(n).includes(g))
      if (!nodes.length) return
      const pts = nodes.map((n) => {
        const p = toOut(n.position())
        return { x: p.x, y: p.y, r: (n.width() * k) / 2 }
      })
      const color = S.nodeColorMap[g] || '#888888'
      if (pts.length === 1) {
        shapes.push({
          kind: 'circle',
          color,
          cx: pts[0].x,
          cy: pts[0].y,
          r: pts[0].r + (margin * 28) / 30,
        })
      } else if (pts.length === 2) {
        shapes.push({
          kind: 'capsule',
          color,
          a: pts[0],
          b: pts[1],
          pad: Math.max(pts[0].r, pts[1].r) + margin,
        })
      } else {
        const maxR = Math.max(...pts.map((p) => p.r))
        shapes.push({ kind: 'blob', color, hull: inflateHull(convexHull(pts), margin + maxR) })
      }
    })
  return shapes.map((s) => ({ ...s, style, opacity }))
}

export function capsulePath(a, b, pad) {
  const dx = b.x - a.x,
    dy = b.y - a.y
  const d = Math.hypot(dx, dy) || 1
  const nx = (-dy / d) * pad,
    ny = (dx / d) * pad
  return (
    `M${a.x + nx},${a.y + ny}L${b.x + nx},${b.y + ny}` +
    `A${pad},${pad} 0 0 0 ${b.x - nx},${b.y - ny}` +
    `L${a.x - nx},${a.y - ny}A${pad},${pad} 0 0 0 ${a.x + nx},${a.y + ny}Z`
  )
}

export function blobPath(points) {
  const n = points.length
  const mid = (p, q) => ({ x: (p.x + q.x) / 2, y: (p.y + q.y) / 2 })
  const m0 = mid(points[n - 1], points[0])
  let d = `M${m0.x.toFixed(2)},${m0.y.toFixed(2)}`
  for (let i = 0; i < n; i++) {
    const p = points[i],
      m = mid(p, points[(i + 1) % n])
    d += `Q${p.x.toFixed(2)},${p.y.toFixed(2)} ${m.x.toFixed(2)},${m.y.toFixed(2)}`
  }
  return d + 'Z'
}

// Maps every coordinate pair of an absolute path (M, L, Q, Z commands).
function transformPathD(d, map) {
  return d.replace(/(-?\d*\.?\d+(?:e[-+]?\d+)?),(-?\d*\.?\d+(?:e[-+]?\d+)?)/gi, (_, x, y) => {
    const p = map({ x: parseFloat(x), y: parseFloat(y) })
    return `${Math.round(p.x * 100) / 100},${Math.round(p.y * 100) / 100}`
  })
}

export function hullPathData(s) {
  if (s.kind === 'path') return s.d
  if (s.kind === 'circle')
    return `M${s.cx - s.r},${s.cy}a${s.r},${s.r} 0 1 0 ${2 * s.r},0a${s.r},${s.r} 0 1 0 ${-2 * s.r},0Z`
  if (s.kind === 'capsule') return capsulePath(s.a, s.b, s.pad)
  return blobPath(s.hull)
}

export function drawHullsOnCanvas(ctx, shapes, k) {
  shapes.forEach((s) => {
    const path = new Path2D(hullPathData(s))
    ctx.save()
    ctx.fillStyle = hexToRgba(s.color, s.opacity)
    if (s.style === 'bubble') {
      ctx.fill(path, 'evenodd')
      ctx.lineWidth = 1.6 * Math.max(1, k)
      ctx.strokeStyle = hexToRgba(s.color, Math.min(1, s.opacity + 0.45))
      ctx.stroke(path)
    } else if (s.style === 'fog') {
      ctx.filter = `blur(${Math.max(1, ((16 * k) / Math.max(cy.zoom(), 1e-6)) * Math.max(0.15, Math.min(1, cy.zoom())))}px)`
      ctx.fill(path)
    } else {
      ctx.fill(path)
      ctx.lineWidth = 1.5 * Math.max(1, k)
      ctx.strokeStyle = hexToRgba(s.color, Math.min(1, s.opacity + 0.35))
      ctx.stroke(path)
    }
    ctx.restore()
  })
}
