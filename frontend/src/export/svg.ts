// @ts-nocheck
// ponytail: split verbatim from the former single-file script; typed one file at a time (remove @ts-nocheck as it lands)
import { PIE_MAX_SLICES } from '../network_state'
import { cy } from '../cy'
import { exportFrame, exportPixelSize, hullMarginModel } from './draw'
import { hullPathData, hullShapes } from './shading'

/* ---------- SVG ---------- */
const svgEsc = (v) =>
  String(v).replace(
    /[&<>"]/g,
    (c) => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;' })[c]
  )

const fmt = (v) => (Math.round(v * 100) / 100).toString()

function nodeShapeSvg(n, fill, strokeAttrs) {
  const shape = n.style('shape')
  const p = n.position(),
    w = n.width(),
    h = n.height()
  if (shape === 'ellipse')
    return `<ellipse cx="${fmt(p.x)}" cy="${fmt(p.y)}" rx="${fmt(w / 2)}" ry="${fmt(h / 2)}" fill="${fill}"${strokeAttrs}/>`
  if (
    shape === 'rectangle' ||
    shape === 'square' ||
    shape === 'barrel' ||
    /round-?rectangle/.test(shape)
  ) {
    const rx = /round/.test(shape)
      ? Math.min(w / 4, h / 4, 8)
      : shape === 'barrel'
        ? Math.min(w, h) * 0.15
        : 0
    return `<rect x="${fmt(p.x - w / 2)}" y="${fmt(p.y - h / 2)}" width="${fmt(w)}" height="${fmt(h)}" rx="${fmt(rx)}" fill="${fill}"${strokeAttrs}/>`
  }
  const def = cy.renderer().nodeShapes[shape]
  if (def && def.points) {
    const pts = []
    for (let i = 0; i < def.points.length; i += 2) {
      pts.push(`${fmt(p.x + (def.points[i] * w) / 2)},${fmt(p.y + (def.points[i + 1] * h) / 2)}`)
    }
    return `<polygon points="${pts.join(' ')}" fill="${fill}"${strokeAttrs}/>`
  }
  return `<ellipse cx="${fmt(p.x)}" cy="${fmt(p.y)}" rx="${fmt(w / 2)}" ry="${fmt(h / 2)}" fill="${fill}"${strokeAttrs}/>`
}

function pieSvg(n) {
  if (n.style('shape') !== 'ellipse') return ''
  const p = n.position()
  const r = Math.min(n.width(), n.height()) / 2
  let start = -Math.PI / 2
  let out = ''
  for (let i = 1; i <= PIE_MAX_SLICES; i++) {
    const size = parseFloat(n.data('pieSize' + i)) || 0
    if (size <= 0) continue
    const color = n.data('pieColor' + i)
    if (size >= 99.999) {
      out += `<circle cx="${fmt(p.x)}" cy="${fmt(p.y)}" r="${fmt(r)}" fill="${color}"/>`
      break
    }
    const end = start + (size / 100) * 2 * Math.PI
    const large = end - start > Math.PI ? 1 : 0
    out +=
      `<path d="M${fmt(p.x)},${fmt(p.y)}L${fmt(p.x + r * Math.cos(start))},${fmt(p.y + r * Math.sin(start))}` +
      `A${fmt(r)},${fmt(r)} 0 ${large} 1 ${fmt(p.x + r * Math.cos(end))},${fmt(p.y + r * Math.sin(end))}Z" fill="${color}"/>`
    start = end
  }
  return out
}

function edgePathSvg(e) {
  const rs = e._private.rscratch || {}
  const pts = rs.allpts
  if (pts && pts.length >= 4 && pts.every(Number.isFinite)) {
    if (['bezier', 'multibezier', 'self', 'compound'].includes(rs.edgeType) && pts.length >= 6) {
      let d = `M${fmt(pts[0])},${fmt(pts[1])}`
      for (let i = 2; i + 3 < pts.length; i += 4)
        d += `Q${fmt(pts[i])},${fmt(pts[i + 1])} ${fmt(pts[i + 2])},${fmt(pts[i + 3])}`
      return { d, mid: bezierMid(pts) }
    }
    let d = `M${fmt(pts[0])},${fmt(pts[1])}`
    for (let i = 2; i + 1 < pts.length; i += 2) d += `L${fmt(pts[i])},${fmt(pts[i + 1])}`
    const m = Math.floor((pts.length / 2 - 1) / 2) * 2
    return {
      d,
      mid: {
        x: (pts[m] + pts[m + 2]) / 2,
        y: (pts[m + 1] + pts[m + 3]) / 2,
        angle: Math.atan2(pts[m + 3] - pts[m + 1], pts[m + 2] - pts[m]),
      },
    }
  }
  const a = e.source().position(),
    b = e.target().position()
  return {
    d: `M${fmt(a.x)},${fmt(a.y)}L${fmt(b.x)},${fmt(b.y)}`,
    mid: { x: (a.x + b.x) / 2, y: (a.y + b.y) / 2, angle: Math.atan2(b.y - a.y, b.x - a.x) },
  }
}

function bezierMid(pts) {
  // middle quadratic segment, evaluated at t = 0.5
  const segs = Math.floor((pts.length - 2) / 4)
  const s = Math.floor((segs - 1) / 2) * 4
  const x0 = pts[s],
    y0 = pts[s + 1],
    cx = pts[s + 2],
    cy0 = pts[s + 3],
    x1 = pts[s + 4],
    y1 = pts[s + 5]
  const t = segs % 2 ? 0.5 : 1
  const x = (1 - t) * (1 - t) * x0 + 2 * (1 - t) * t * cx + t * t * x1
  const y = (1 - t) * (1 - t) * y0 + 2 * (1 - t) * t * cy0 + t * t * y1
  const dx = 2 * (1 - t) * (cx - x0) + 2 * t * (x1 - cx),
    dy = 2 * (1 - t) * (cy0 - y0) + 2 * t * (y1 - cy0)
  return { x, y, angle: Math.atan2(dy, dx) }
}

// Arrowhead at an edge's target, drawn like Cytoscape does: its outline in
// unit coordinates (tip at the origin, back toward negative y), scaled with
// the edge width and turned along the edge's last segment.
function arrowSvg(e, shape, width) {
  const rs = e._private.rscratch || {}
  const def = cy.renderer().arrowShapes && cy.renderer().arrowShapes[shape]
  if (!def || !def.points || !Number.isFinite(rs.arrowEndX)) return ''
  const pts = rs.allpts || []
  const endX = Number.isFinite(rs.endX) ? rs.endX : pts[pts.length - 2]
  const endY = Number.isFinite(rs.endY) ? rs.endY : pts[pts.length - 1]
  let dx = rs.arrowEndX - endX,
    dy = rs.arrowEndY - endY
  let len = Math.hypot(dx, dy)
  if (len < 1e-6) {
    // fall back to the direction of the last path segment
    const px = pts[pts.length - 4],
      py = pts[pts.length - 3]
    dx = rs.arrowEndX - px
    dy = rs.arrowEndY - py
    len = Math.hypot(dx, dy) || 1
  }
  const ux = dx / len,
    uy = dy / len // forward along the edge
  const size = Math.max(Math.pow(width * 13.37, 0.9), 29) * (e.pstyle('arrow-scale').value || 1)
  const poly = []
  for (let i = 0; i < def.points.length; i += 2) {
    const px = def.points[i] * size,
      py = def.points[i + 1] * size
    poly.push(`${fmt(rs.arrowEndX - uy * px + ux * py)},${fmt(rs.arrowEndY + ux * px + uy * py)}`)
  }
  return `<polygon points="${poly.join(' ')}" fill="${e.style('target-arrow-color')}" fill-opacity="${fmt(e.effectiveOpacity())}"/>`
}

function textSvg(text, x, y, attrs, fontPx, color, halo, haloWidth, rotate) {
  const upright = rotate
    ? rotate > Math.PI / 2 || rotate < -Math.PI / 2
      ? rotate + Math.PI
      : rotate
    : 0
  const transform = upright
    ? ` transform="rotate(${fmt((upright * 180) / Math.PI)} ${fmt(x)} ${fmt(y)})"`
    : ''
  const haloAttr =
    haloWidth > 0
      ? ` stroke="${halo}" stroke-width="${fmt(haloWidth * 2)}" stroke-linejoin="round" paint-order="stroke"`
      : ''
  return `<text x="${fmt(x)}" y="${fmt(y)}" font-size="${fmt(fontPx)}" fill="${color}"${haloAttr}${attrs}${transform}>${svgEsc(text)}</text>`
}

export function buildSvg(opts) {
  const frame = exportFrame(opts.area)
  if (!frame) throw new Error('Nothing is shown to export.')
  const { W, H } = exportPixelSize(frame, opts.scale)
  const out = []
  out.push(`<?xml version="1.0" encoding="UTF-8"?>`)
  out.push(
    `<svg xmlns="http://www.w3.org/2000/svg" width="${W}" height="${H}" viewBox="${fmt(frame.x1)} ${fmt(frame.y1)} ${fmt(frame.w)} ${fmt(frame.h)}" font-family="Inter, Helvetica, Arial, sans-serif">`
  )
  out.push(`<title>${svgEsc(opts.title || 'NORMA network')}</title>`)
  if (opts.bg)
    out.push(
      `<rect x="${fmt(frame.x1)}" y="${fmt(frame.y1)}" width="${fmt(frame.w)}" height="${fmt(frame.h)}" fill="${opts.bg}"/>`
    )

  if (opts.hulls) {
    const shapes = hullShapes((p) => p, 1)
    if (shapes.length) {
      const fog = shapes[0].style === 'fog'
      if (fog)
        out.push(
          `<defs><filter id="fog" x="-20%" y="-20%" width="140%" height="140%"><feGaussianBlur stdDeviation="${fmt((16 * hullMarginModel()) / 30 / 2)}"/></filter></defs>`
        )
      out.push('<g id="group-shading">')
      shapes.forEach((s) => {
        const stroke = fog
          ? ''
          : ` stroke="${s.color}" stroke-opacity="${fmt(Math.min(1, s.opacity + 0.35))}" stroke-width="${fmt(1.5 / Math.max(cy.zoom(), 0.01))}"`
        const rule = s.kind === 'path' ? ' fill-rule="evenodd"' : ''
        out.push(
          `<path d="${hullPathData(s)}"${rule} fill="${s.color}" fill-opacity="${fmt(s.opacity)}"${stroke}${fog ? ' filter="url(#fog)"' : ''}/>`
        )
      })
      out.push('</g>')
    }
  }

  const visible = (ele) => ele.visible() && ele.effectiveOpacity() > 0
  const inFrame = (bb) =>
    !(
      bb.x2 < frame.x1 ||
      bb.y2 < frame.y1 ||
      bb.x1 > frame.x1 + frame.w ||
      bb.y1 > frame.y1 + frame.h
    )

  out.push('<g id="edges" fill="none">')
  const edgeLabels = []
  cy.edges().forEach((e) => {
    if (!visible(e) || !inFrame(e.boundingBox({ includeLabels: false }))) return
    const { d, mid } = edgePathSvg(e)
    const width = e.pstyle('width').pfValue
    const arrowShape = e.style('target-arrow-shape')
    const hasArrow = arrowShape && arrowShape !== 'none'
    out.push(
      `<path d="${d}" stroke="${e.style('line-color')}" stroke-width="${fmt(width)}" stroke-opacity="${fmt(e.effectiveOpacity())}"><title>${svgEsc(`${e.data('source')} ${hasArrow ? '→' : '–'} ${e.data('target')} (${e.data('type')})`)}</title></path>`
    )
    if (hasArrow) {
      const arrow = arrowSvg(e, arrowShape, width)
      if (arrow) out.push(arrow)
    }
    const label = e.style('label')
    if (label) {
      const rs = e._private.rscratch || {}
      const pos = Number.isFinite(rs.labelX) ? { x: rs.labelX, y: rs.labelY } : mid
      const rot =
        e.style('text-rotation') === 'autorotate'
          ? Number.isFinite(rs.labelAutoAngle)
            ? rs.labelAutoAngle
            : mid.angle
          : 0
      edgeLabels.push(
        textSvg(
          label,
          pos.x,
          pos.y,
          ' text-anchor="middle" dominant-baseline="central"',
          e.pstyle('font-size').pfValue,
          e.style('color'),
          e.style('text-background-color'),
          2,
          rot
        )
      )
    }
  })
  out.push('</g>')

  out.push('<g id="nodes">')
  const nodeLabels = []
  cy.nodes().forEach((n) => {
    if (!visible(n) || !inFrame(n.boundingBox({ includeLabels: false }))) return
    const bw = n.pstyle('border-width').pfValue
    const strokeAttrs =
      bw > 0 ? ` stroke="${n.style('border-color')}" stroke-width="${fmt(bw)}"` : ''
    const opacity = n.effectiveOpacity()
    out.push(
      `<g${opacity < 1 ? ` opacity="${fmt(opacity)}"` : ''}><title>${svgEsc(n.id())}</title>`
    )
    out.push(nodeShapeSvg(n, n.style('background-color'), ''))
    out.push(pieSvg(n))
    if (bw > 0) out.push(nodeShapeSvg(n, 'none', strokeAttrs))
    out.push('</g>')
    const label = n.style('label')
    if (label) {
      const p = n.position(),
        w = n.width(),
        h = n.height()
      const valign = n.style('text-valign'),
        halign = n.style('text-halign')
      const mx = n.pstyle('text-margin-x').pfValue || 0,
        my = n.pstyle('text-margin-y').pfValue || 0
      let x = p.x + mx,
        y = p.y + my,
        anchor = 'middle',
        baseline = 'central'
      if (valign === 'top') {
        y = p.y - h / 2 + my
        baseline = 'text-after-edge'
      } else if (valign === 'bottom') {
        y = p.y + h / 2 + my
        baseline = 'text-before-edge'
      }
      if (halign === 'left') {
        x = p.x - w / 2 + mx
        anchor = 'end'
      } else if (halign === 'right') {
        x = p.x + w / 2 + mx
        anchor = 'start'
      }
      nodeLabels.push(
        textSvg(
          label,
          x,
          y,
          ` text-anchor="${anchor}" dominant-baseline="${baseline}"`,
          n.pstyle('font-size').pfValue,
          n.style('color'),
          n.style('text-outline-color'),
          n.pstyle('text-outline-width').pfValue,
          0
        )
      )
    }
  })
  out.push('</g>')
  if (edgeLabels.length) out.push('<g id="edge-labels">', ...edgeLabels, '</g>')
  if (nodeLabels.length) out.push('<g id="node-labels">', ...nodeLabels, '</g>')
  out.push('</svg>')
  return new Blob([out.join('\n')], { type: 'image/svg+xml' })
}
