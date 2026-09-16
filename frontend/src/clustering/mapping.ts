// @ts-nocheck
// ponytail: split verbatim from the former single-file script; typed one file at a time (remove @ts-nocheck as it lands)
import { EDGE_TYPES } from '../palette'
import { S } from '../state'
import {
  UNGROUPED,
  effectiveGroupsFor,
  formatAttrValue,
  getUsedGroups,
  groupLabel,
  nodeFillMode,
  refreshNodeVisual,
} from '../network_state'
import { VALUE_SCALES, VALUE_TRANSFORMS } from './mcl'
import { applyNodeSizing, shownNodes } from '../metrics'
import { convexHull, displayGroups, getUsedTypes, legendExtra } from '../hulls'
import { currentTheme } from '../themes'
import { cy } from '../cy'
import { mulberry32 } from '../sample_data'
import { plural } from '../layouts/controls'
import { rgbOf } from '../view3d/state'
import { stopFramePlay, updateFrameControls } from '../enrichment'
import { updateContextInfo } from '../recording'

/* ---------- mapping ---------- */
export function viewHasValues() {
  return cy.nodes().some((n) => n.data('values'))
}

export function valueColumns() {
  const cols = new Set()
  cy.nodes().forEach((n) => {
    const v = n.data('values')
    if (v) Object.keys(v).forEach((k) => cols.add(k))
  })
  return [...cols]
}

function valueSettings() {
  const scale =
    VALUE_SCALES[document.getElementById('valueScale').value] || VALUE_SCALES['div-rdbu']
  return {
    column: document.getElementById('valueColumn').value,
    transform:
      VALUE_TRANSFORMS[document.getElementById('valueTransform').value] || VALUE_TRANSFORMS.none,
    scaleKey: document.getElementById('valueScale').value,
    scale,
    center: parseFloat(document.getElementById('valueCenter').value) || 0,
    range: document.getElementById('valueRange').value,
    min: parseFloat(document.getElementById('valueMin').value),
    max: parseFloat(document.getElementById('valueMax').value),
    missing: document.getElementById('valueMissing').value || '#c8c8c8',
  }
}

function nodeValue(node, st) {
  const v = node.data('values')
  if (!v || v[st.column] === null || v[st.column] === undefined) return null
  const t = st.transform.fn(v[st.column])
  return Number.isFinite(t) ? t : null
}

// Range of the scale: [lo, hi], plus the center for diverging scales.
function valueDomain(st) {
  const vals = []
  const cols = document.getElementById('valueSameRange').checked ? valueColumns() : [st.column]
  cy.nodes().forEach((n) => {
    if (n.hasClass('hidden-group')) return
    cols.forEach((c) => {
      const x = nodeValue(n, { ...st, column: c })
      if (x !== null) vals.push(x)
    })
  })
  if (!vals.length) return { lo: 0, hi: 1, center: st.center, count: 0 }
  let lo = Math.min(...vals),
    hi = Math.max(...vals)
  if (
    st.range === 'custom' &&
    Number.isFinite(st.min) &&
    Number.isFinite(st.max) &&
    st.max > st.min
  ) {
    lo = st.min
    hi = st.max
  } else if (st.scale.diverging && st.range === 'symmetric') {
    const m = Math.max(Math.abs(lo - st.center), Math.abs(hi - st.center)) || 1
    lo = st.center - m
    hi = st.center + m
  }
  if (hi === lo) {
    lo -= 1
    hi += 1
  }
  return {
    lo,
    hi,
    center: st.center,
    count: vals.length,
    dataMin: Math.min(...vals),
    dataMax: Math.max(...vals),
  }
}

function interpolateStops(stops, t) {
  t = Math.max(0, Math.min(1, t))
  const pos = t * (stops.length - 1)
  const i = Math.min(stops.length - 2, Math.floor(pos))
  const f = pos - i
  const a = rgbOf(stops[i]),
    b = rgbOf(stops[i + 1])
  const h = (x) => Math.round(x).toString(16).padStart(2, '0')
  return (
    '#' + h(a[0] + (b[0] - a[0]) * f) + h(a[1] + (b[1] - a[1]) * f) + h(a[2] + (b[2] - a[2]) * f)
  )
}

function valueToColor(x, st, dom) {
  if (x === null) return st.missing
  let t
  if (st.scale.diverging) {
    const c = Math.min(Math.max(dom.center, dom.lo), dom.hi)
    t = x < c ? (0.5 * (x - dom.lo)) / (c - dom.lo || 1) : 0.5 + (0.5 * (x - c)) / (dom.hi - c || 1)
  } else {
    t = (x - dom.lo) / (dom.hi - dom.lo)
  }
  return interpolateStops(st.scale.stops, t)
}

// Size by value: distance from the center on diverging scales, the value itself otherwise.
export function valueForSize(node) {
  const st = valueSettings()
  const x = nodeValue(node, st)
  if (x === null) return 0
  return st.scale.diverging ? Math.abs(x - st.center) : x
}

export function applyValueColors() {
  const has = viewHasValues()
  document.getElementById('valueControls').hidden = !has
  const fillSel = document.getElementById('nodeFillSelect')
  fillSel.querySelector('option[value="values"]').disabled = !has
  if (!has && fillSel.value === 'values') fillSel.value = 'groups'
  if (!has) {
    scheduleLegend()
    if (typeof updateFrameControls === 'function') {
      stopFramePlay()
      updateFrameControls()
    }
    return
  }
  // keep the column list current
  const colSel = document.getElementById('valueColumn')
  const cols = valueColumns()
  const current = colSel.value
  if ([...colSel.options].map((o) => o.value).join('\u0000') !== cols.join('\u0000')) {
    colSel.innerHTML = ''
    cols.forEach((c) => colSel.add(new Option(c, c)))
  }
  if (!cols.includes(colSel.value)) colSel.value = cols.includes(current) ? current : cols[0]
  const st = valueSettings()
  document.getElementById('valueCenterRow').hidden = !st.scale.diverging
  document.getElementById('valueCustomRow').hidden = st.range !== 'custom'
  const symOpt = document.getElementById('valueRange').querySelector('option[value="symmetric"]')
  symOpt.disabled = !st.scale.diverging
  if (!st.scale.diverging && st.range === 'symmetric')
    document.getElementById('valueRange').value = 'data'
  const dom = valueDomain(st)
  cy.batch(() => {
    cy.nodes().forEach((n) => {
      const c = valueToColor(nodeValue(n, st), st, dom)
      if (n.data('valueColor') !== c) n.data('valueColor', c)
    })
    if (nodeFillMode() === 'values') cy.nodes().forEach((n) => refreshNodeVisual(n))
  })
  const fmt = (v) => formatAttrValue(Math.round(v * 1000) / 1000)
  const own = cy
    .nodes()
    .filter((n) => !n.hasClass('hidden-group') && nodeValue(n, st) !== null)
    .map((n) => nodeValue(n, st))
  document.getElementById('valueSummary').textContent = own.length
    ? `${plural(own.length, 'shown node')} with a value, from ${fmt(Math.min(...own))} to ${fmt(Math.max(...own))}${st.transform.fn === VALUE_TRANSFORMS.none.fn ? '' : ' after the transform'}${valueColumns().length > 1 && document.getElementById('valueSameRange').checked ? '; the scale covers all columns' : ''}.`
    : 'No shown node has a value in this column.'
  if (document.getElementById('sizeMetric').value === 'value') applyNodeSizing()
  scheduleLegend()
  updateContextInfo()
  if (typeof updateFrameControls === 'function') updateFrameControls()
}

/* ============================================================
   LEGEND
   One layout, drawn three ways: on screen (inline SVG), into exported
   raster images (canvas) and into exported SVG files.
   ============================================================ */
const LEGEND_W = 230

function legendOptions() {
  return {
    show: document.getElementById('legendShow').checked,
    scale: document.getElementById('legendScale').checked,
    groups: document.getElementById('legendGroups').checked,
    shapes: document.getElementById('legendShapes').checked,
    channels: document.getElementById('legendChannels').checked,
    title: document.getElementById('legendTitle').value.trim(),
  }
}

export function legendModel() {
  const o = legendOptions()
  const model = { title: o.title, sections: [], scale: null }
  if (legendExtra)
    legendExtra.forEach((sec) =>
      model.sections.push({
        title: sec.title,
        items: sec.items.map((it) => ({
          kind: it.kind || 'glyph',
          label: it.label,
          color: it.color,
          shape: 'ellipse',
        })),
      })
    )
  if (
    o.scale &&
    viewHasValues() &&
    (nodeFillMode() === 'values' || document.getElementById('sizeMetric').value === 'value')
  ) {
    const st = valueSettings()
    const dom = valueDomain(st)
    const missing = cy.nodes().some((n) => !n.hasClass('hidden-group') && nodeValue(n, st) === null)
    model.scale = {
      title: st.transform.title(st.column),
      stops: st.scale.stops,
      diverging: st.scale.diverging,
      lo: dom.lo,
      hi: dom.hi,
      center: Math.min(Math.max(dom.center, dom.lo), dom.hi),
      missing: missing ? st.missing : null,
      colored: nodeFillMode() === 'values',
      sized: document.getElementById('sizeMetric').value === 'value',
    }
  }
  if (o.groups) {
    const groups = displayGroups(getUsedGroups()).filter(
      (g) => S.activeGroups.has(g) && g !== UNGROUPED
    )
    const showing =
      nodeFillMode() === 'groups' ||
      document.getElementById('showGroupHulls').checked ||
      Object.keys(S.groupShapes).length
    if (groups.length && showing) {
      const items = groups.slice(0, 40).map((g) => ({
        kind: 'glyph',
        label: groupLabel(g),
        color: S.nodeColorMap[g] || '#888888',
        shape: o.shapes && S.groupShapes[g] ? S.groupShapes[g] : 'ellipse',
      }))
      if (groups.length > 40)
        items.push({ kind: 'note', label: `and ${groups.length - 40} more groups` })
      model.sections.push({ title: legendExtra ? 'Nodes (in which networks)' : 'Groups', items })
    }
  }
  if (o.channels) {
    const types = getUsedTypes().filter((t) => S.activeTypes.has(t))
    if (types.length > 1) {
      const items = types
        .slice(0, 30)
        .map((t) => ({
          kind: 'line',
          label: (EDGE_TYPES[t] || { label: t }).label,
          color: (EDGE_TYPES[t] || { color: '#888' }).color,
        }))
      if (types.length > 30)
        items.push({ kind: 'note', label: `and ${types.length - 30} more channels` })
      model.sections.push({
        title: legendExtra ? 'Edges (in which networks)' : 'Edge channels',
        items,
      })
    }
  }
  return model
}

export function legendIsEmpty(m) {
  return !m.scale && !m.sections.length
}

function niceTicks(lo, hi, count = 5) {
  const span = hi - lo
  const step0 = span / (count - 1)
  const mag = Math.pow(10, Math.floor(Math.log10(step0)))
  const norm = step0 / mag
  const step = (norm < 1.5 ? 1 : norm < 3 ? 2 : norm < 7 ? 5 : 10) * mag
  const ticks = []
  for (let v = Math.ceil(lo / step - 1e-9) * step; v <= hi + 1e-9; v += step)
    ticks.push(Math.abs(v) < step * 1e-9 ? 0 : v)
  return ticks
}

function tickLabel(v) {
  const a = Math.abs(v)
  if (a !== 0 && (a >= 1e5 || a < 1e-3)) return v.toExponential(1)
  return String(Math.round(v * 1000) / 1000)
}

// Layout in unit pixels: a list of drawing operations.
export function layoutLegend(m) {
  const ops = []
  const pad = 12,
    w = LEGEND_W
  let y = pad
  const text = (str, x, yy, size, opts = {}) =>
    ops.push({ t: 'text', str, x, y: yy, size, ...opts })
  if (m.title) {
    text(m.title, pad, y + 12, 14, { bold: true })
    y += 24
  }
  if (m.scale) {
    const s = m.scale
    text(s.title, pad, y + 11, 12, { bold: true })
    y += 18
    const barW = w - 2 * pad,
      barH = 12
    ops.push({
      t: 'grad',
      x: pad,
      y,
      w: barW,
      h: barH,
      stops: s.diverging
        ? divergingStopsFor(s)
        : s.stops.map((c, i) => [i / (s.stops.length - 1), c]),
    })
    ops.push({ t: 'rect', x: pad, y, w: barW, h: barH, stroke: true })
    const ticks = niceTicks(s.lo, s.hi)
    ticks.forEach((v) => {
      const tx = pad + ((v - s.lo) / (s.hi - s.lo)) * barW
      ops.push({ t: 'line', x1: tx, y1: y + barH, x2: tx, y2: y + barH + 4, muted: true })
      text(tickLabel(v), tx, y + barH + 15, 10, { anchor: 'middle', muted: true })
    })
    y += barH + 22
    const notes = []
    if (s.colored && s.sized) notes.push('node color and size')
    else if (s.sized) notes.push('node size')
    if (notes.length) {
      text(`Shown as ${notes.join(', ')}`, pad, y + 8, 10, { muted: true })
      y += 14
    }
    if (s.missing) {
      ops.push({ t: 'glyph', shape: 'ellipse', x: pad + 6, y: y + 7, r: 6, fill: s.missing })
      text('no value', pad + 18, y + 11, 11, {})
      y += 18
    }
    y += 6
  }
  m.sections.forEach((sec) => {
    text(sec.title, pad, y + 11, 12, { bold: true })
    y += 18
    sec.items.forEach((it) => {
      if (it.kind === 'glyph')
        ops.push({ t: 'glyph', shape: it.shape, x: pad + 7, y: y + 7, r: 6.5, fill: it.color })
      else if (it.kind === 'line')
        ops.push({
          t: 'line',
          x1: pad,
          y1: y + 7,
          x2: pad + 16,
          y2: y + 7,
          color: it.color,
          width: 3,
        })
      const label = it.label.length > 30 ? it.label.slice(0, 29) + '…' : it.label
      text(label, it.kind === 'note' ? pad : pad + 22, y + 11, 11, {
        muted: it.kind === 'note',
        full: it.label,
      })
      y += 17
    })
    y += 6
  })
  return { w, h: y + pad - 6, ops }
}

function divergingStopsFor(s) {
  // put the middle stop at the center value
  const c = (s.center - s.lo) / (s.hi - s.lo)
  const n = s.stops.length,
    mid = (n - 1) / 2
  return s.stops.map((col, i) => [i <= mid ? (c * i) / mid : c + ((1 - c) * (i - mid)) / mid, col])
}

function legendSvg(layout, scale, colors, x0 = 0, y0 = 0) {
  const f = (v) => (Math.round(v * 100) / 100).toString()
  const esc = (v) =>
    String(v).replace(
      /[&<>"]/g,
      (c) => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;' })[c]
    )
  const id = 'lg' + Math.random().toString(36).slice(2, 8)
  let defs = '',
    body = ''
  let gi = 0
  layout.ops.forEach((op) => {
    if (op.t === 'text') {
      body += `<text x="${f(op.x)}" y="${f(op.y)}" font-size="${op.size}" fill="${op.muted ? colors.muted : colors.text}"${op.bold ? ' font-weight="700"' : ''}${op.anchor ? ` text-anchor="${op.anchor}"` : ''}>${op.full && op.full !== op.str ? `<title>${esc(op.full)}</title>` : ''}${esc(op.str)}</text>`
    } else if (op.t === 'grad') {
      const gid = `${id}g${gi++}`
      defs += `<linearGradient id="${gid}" x1="0" x2="1" y1="0" y2="0">${op.stops.map(([o, c]) => `<stop offset="${f(o)}" stop-color="${c}"/>`).join('')}</linearGradient>`
      body += `<rect x="${f(op.x)}" y="${f(op.y)}" width="${f(op.w)}" height="${f(op.h)}" fill="url(#${gid})"/>`
    } else if (op.t === 'rect') {
      body += `<rect x="${f(op.x)}" y="${f(op.y)}" width="${f(op.w)}" height="${f(op.h)}" fill="none" stroke="${colors.line}"/>`
    } else if (op.t === 'line') {
      body += `<line x1="${f(op.x1)}" y1="${f(op.y1)}" x2="${f(op.x2)}" y2="${f(op.y2)}" stroke="${op.color || colors.muted}" stroke-width="${op.width || 1}" stroke-linecap="round"/>`
    } else if (op.t === 'glyph') {
      body += glyphSvg(op.shape, op.x, op.y, op.r, op.fill, colors.text)
    }
  })
  return `<g transform="translate(${f(x0)} ${f(y0)}) scale(${f(scale)})" font-family="Inter, Helvetica, Arial, sans-serif">${defs ? `<defs>${defs}</defs>` : ''}${body}</g>`
}

function glyphSvg(shape, x, y, r, fill, stroke) {
  const f = (v) => (Math.round(v * 100) / 100).toString()
  const def = shape !== 'ellipse' && cy.renderer().nodeShapes[shape]
  if (def && def.points) {
    const pts = []
    for (let i = 0; i < def.points.length; i += 2)
      pts.push(`${f(x + def.points[i] * r)},${f(y + def.points[i + 1] * r)}`)
    return `<polygon points="${pts.join(' ')}" fill="${fill}" stroke="${stroke}" stroke-opacity="0.35" stroke-width="0.8"/>`
  }
  return `<circle cx="${f(x)}" cy="${f(y)}" r="${f(r)}" fill="${fill}" stroke="${stroke}" stroke-opacity="0.35" stroke-width="0.8"/>`
}

function drawLegendOnCanvas(ctx, layout, scale, colors, x0, y0, bg) {
  ctx.save()
  ctx.translate(x0, y0)
  ctx.scale(scale, scale)
  if (bg) {
    ctx.fillStyle = bg
    ctx.fillRect(0, 0, layout.w, layout.h)
  }
  layout.ops.forEach((op) => {
    if (op.t === 'text') {
      ctx.font = `${op.bold ? '700 ' : ''}${op.size}px Inter, Helvetica, Arial, sans-serif`
      ctx.fillStyle = op.muted ? colors.muted : colors.text
      ctx.textAlign = op.anchor === 'middle' ? 'center' : 'left'
      ctx.textBaseline = 'alphabetic'
      ctx.fillText(op.str, op.x, op.y)
    } else if (op.t === 'grad') {
      const g = ctx.createLinearGradient(op.x, 0, op.x + op.w, 0)
      op.stops.forEach(([o, c]) => g.addColorStop(Math.max(0, Math.min(1, o)), c))
      ctx.fillStyle = g
      ctx.fillRect(op.x, op.y, op.w, op.h)
    } else if (op.t === 'rect') {
      ctx.strokeStyle = colors.line
      ctx.lineWidth = 1
      ctx.strokeRect(op.x, op.y, op.w, op.h)
    } else if (op.t === 'line') {
      ctx.strokeStyle = op.color || colors.muted
      ctx.lineWidth = op.width || 1
      ctx.lineCap = 'round'
      ctx.beginPath()
      ctx.moveTo(op.x1, op.y1)
      ctx.lineTo(op.x2, op.y2)
      ctx.stroke()
    } else if (op.t === 'glyph') {
      ctx.fillStyle = op.fill
      ctx.strokeStyle = colors.text
      ctx.globalAlpha = 1
      ctx.lineWidth = 0.8
      const def = op.shape !== 'ellipse' && cy.renderer().nodeShapes[op.shape]
      ctx.beginPath()
      if (def && def.points) {
        for (let i = 0; i < def.points.length; i += 2) {
          const px = op.x + def.points[i] * op.r,
            py = op.y + def.points[i + 1] * op.r
          i ? ctx.lineTo(px, py) : ctx.moveTo(px, py)
        }
        ctx.closePath()
      } else ctx.arc(op.x, op.y, op.r, 0, Math.PI * 2)
      ctx.fill()
      ctx.globalAlpha = 0.35
      ctx.stroke()
      ctx.globalAlpha = 1
    }
  })
  ctx.restore()
}

function legendColorsFor(bg) {
  // readable text on the export background (or the theme on screen)
  const dark = bg
    ? (() => {
        const [r, g, b] = rgbOf(bg)
        return (0.2126 * r + 0.7152 * g + 0.0722 * b) / 255 < 0.45
      })()
    : isDarkTheme()
  return dark
    ? { text: '#e5e7eb', muted: '#9ca3af', line: '#6b7280' }
    : { text: '#111827', muted: '#6b7280', line: '#9ca3af' }
}

function isDarkTheme() {
  const [r, g, b] = rgbOf(currentTheme.bg)
  return (0.2126 * r + 0.7152 * g + 0.0722 * b) / 255 < 0.45
}

// On-screen legend
let legendQueued = false

export function scheduleLegend() {
  if (legendQueued) return
  legendQueued = true
  requestAnimationFrame(() => {
    legendQueued = false
    renderLegendPanel()
  })
}

function renderLegendPanel() {
  const el = document.getElementById('legendPanel')
  if (!el) return
  const o = legendOptions()
  const m = o.show ? legendModel() : null
  if (!m || legendIsEmpty(m) || !cy.nodes().length) {
    el.hidden = true
    return
  }
  const layout = layoutLegend(m)
  const colors = { text: currentTheme.text, muted: currentTheme.muted, line: currentTheme.line }
  el.innerHTML = `<svg viewBox="0 0 ${layout.w} ${layout.h}" width="${layout.w}" height="${layout.h}" role="img" aria-label="Legend">${legendSvg(layout, 1, colors)}</svg>`
  el.hidden = false
}

// Adds the legend to the right of an exported picture.
export function exportLegendLayout(o) {
  if (!o.legend) return null
  const m = legendModel()
  return legendIsEmpty(m) ? null : layoutLegend(m)
}

export function composeCanvasWithLegend(canvas, layout, scale, bg) {
  if (!layout) return canvas
  const lw = Math.round(layout.w * scale),
    lh = Math.round(layout.h * scale)
  const out = document.createElement('canvas')
  out.width = canvas.width + lw
  out.height = Math.max(canvas.height, lh)
  const ctx = out.getContext('2d')
  if (bg) {
    ctx.fillStyle = bg
    ctx.fillRect(0, 0, out.width, out.height)
  }
  ctx.drawImage(canvas, 0, 0)
  drawLegendOnCanvas(ctx, layout, scale, legendColorsFor(bg || '#ffffff'), canvas.width, 0, null)
  return out
}

export function composeSvgWithLegend(svgText, layout, scale, bg) {
  if (!layout) return svgText
  const body = svgText.replace(/^<\?xml[^>]*>\s*/, '')
  const wm = /<svg[^>]*\swidth="([\d.]+)"/.exec(body),
    hm = /<svg[^>]*\sheight="([\d.]+)"/.exec(body)
  const W = wm ? parseFloat(wm[1]) : 1000,
    H = hm ? parseFloat(hm[1]) : 800
  const lw = layout.w * scale,
    lh = layout.h * scale
  const totalW = Math.ceil(W + lw),
    totalH = Math.ceil(Math.max(H, lh))
  const inner = body.replace(/<svg /, `<svg x="0" y="0" `)
  return (
    `<?xml version="1.0" encoding="UTF-8"?>\n<svg xmlns="http://www.w3.org/2000/svg" width="${totalW}" height="${totalH}" viewBox="0 0 ${totalW} ${totalH}">\n` +
    (bg ? `<rect width="${totalW}" height="${totalH}" fill="${bg}"/>\n` : '') +
    inner +
    '\n' +
    `<g id="legend">${legendSvg(layout, scale, legendColorsFor(bg || '#ffffff'), W, 0)}</g>\n</svg>`
  )
}

/* ============================================================
   GROUP SEPARATION
   How well a layout separates the ticked groups:
     silhouette  mean silhouette width of nodes in exactly one group,
                 from their positions (-1..1, higher = better separated)
     intruders   share of shown nodes lying inside the convex outline of
                 a group they don't belong to (lower = better)
     overlap     share of the area covered by group outlines that is
                 covered by two or more of them (lower = better)
   Scores are computed after every layout and by the layout benchmark.
   ============================================================ */
const SEPARATION_SAMPLE = 1500

export function groupSeparation(positions, groupsOf) {
  const ids = Object.keys(groupsOf).filter((id) => positions[id])
  const out = {
    nodes: ids.length,
    groups: 0,
    silhouette: NaN,
    intruders: NaN,
    overlap: NaN,
    silhouetteNodes: 0,
  }
  const members = new Map()
  ids.forEach((id) =>
    groupsOf[id].forEach((g) => {
      if (!members.has(g)) members.set(g, [])
      members.get(g).push(id)
    })
  )
  out.groups = members.size
  if (members.size < 2) return out

  // --- silhouette (nodes in exactly one group)
  const single = ids.filter((id) => groupsOf[id].length === 1)
  const byGroup = new Map()
  single.forEach((id) => {
    const g = groupsOf[id][0]
    if (!byGroup.has(g)) byGroup.set(g, [])
    byGroup.get(g).push(id)
  })
  if (byGroup.size >= 2) {
    const rand = mulberry32(99)
    let sample = single
    if (single.length > SEPARATION_SAMPLE) {
      sample = [...single]
      for (let i = sample.length - 1; i > 0; i--) {
        const j = Math.floor(rand() * (i + 1))
        ;[sample[i], sample[j]] = [sample[j], sample[i]]
      }
      sample = sample.slice(0, SEPARATION_SAMPLE)
    }
    // reference points per group (all single-group nodes, capped per group), as flat arrays
    const refs = new Map()
    byGroup.forEach((list, g) => {
      let pts = list
      if (list.length > 400) {
        pts = []
        const stepK = list.length / 400
        for (let k = 0; k < 400; k++) pts.push(list[Math.floor(k * stepK)])
      }
      const xs = new Float64Array(pts.length),
        ys = new Float64Array(pts.length)
      pts.forEach((id, k) => {
        xs[k] = positions[id].x
        ys[k] = positions[id].y
      })
      refs.set(g, { xs, ys, ids: new Set(pts) })
    })
    const groupKeys = [...refs.keys()]
    let total = 0
    sample.forEach((id) => {
      const g = groupsOf[id][0]
      const px = positions[id].x,
        py = positions[id].y
      const own = refs.get(g)
      let a = 0,
        na = 0
      for (let k = 0; k < own.xs.length; k++) {
        const dx = px - own.xs[k],
          dy = py - own.ys[k]
        const d = Math.sqrt(dx * dx + dy * dy)
        if (d === 0 && own.ids.has(id)) continue // the node itself
        a += d
        na++
      }
      if (!na) return // a group of one: silhouette 0
      a /= na
      let b = Infinity
      for (const h of groupKeys) {
        if (h === g) continue
        const r = refs.get(h)
        let sum = 0
        for (let k = 0; k < r.xs.length; k++) {
          const dx = px - r.xs[k],
            dy = py - r.ys[k]
          sum += Math.sqrt(dx * dx + dy * dy)
        }
        const mean = sum / r.xs.length
        if (mean < b) b = mean
      }
      const m = Math.max(a, b)
      total += m > 0 ? (b - a) / m : 0
    })
    out.silhouette = total / sample.length
    out.silhouetteNodes = sample.length
  }

  // --- outlines: convex hulls of groups with three or more members
  const hulls = []
  members.forEach((list, g) => {
    if (list.length < 3) return
    const hull = convexHull(list.map((id) => positions[id]))
    if (hull.length < 3) return
    const xs = hull.map((p) => p.x),
      ys = hull.map((p) => p.y)
    hulls.push({
      g,
      hull,
      set: new Set(list),
      x1: Math.min(...xs),
      x2: Math.max(...xs),
      y1: Math.min(...ys),
      y2: Math.max(...ys),
    })
  })
  if (!hulls.length) return out
  const inside = (hull, x, y) => {
    // convex polygon in either orientation
    let sign = 0
    for (let i = 0; i < hull.length; i++) {
      const a = hull[i],
        b = hull[(i + 1) % hull.length]
      const cr = (b.x - a.x) * (y - a.y) - (b.y - a.y) * (x - a.x)
      if (Math.abs(cr) < 1e-9) continue
      const s = cr > 0 ? 1 : -1
      if (!sign) sign = s
      else if (s !== sign) return false
    }
    return true
  }
  let intr = 0
  ids.forEach((id) => {
    const p = positions[id]
    for (const h of hulls) {
      if (h.set.has(id) || p.x < h.x1 || p.x > h.x2 || p.y < h.y1 || p.y > h.y2) continue
      if (inside(h.hull, p.x, p.y)) {
        intr++
        break
      }
    }
  })
  out.intruders = intr / ids.length

  // --- overlap by rasterising the outlines
  const X1 = Math.min(...hulls.map((h) => h.x1)),
    X2 = Math.max(...hulls.map((h) => h.x2))
  const Y1 = Math.min(...hulls.map((h) => h.y1)),
    Y2 = Math.max(...hulls.map((h) => h.y2))
  const G = 160
  const sx = (X2 - X1) / G || 1,
    sy = (Y2 - Y1) / G || 1
  const cover = new Uint16Array(G * G)
  hulls.forEach((h) => {
    const i1 = Math.max(0, Math.floor((h.x1 - X1) / sx)),
      i2 = Math.min(G - 1, Math.floor((h.x2 - X1) / sx))
    const j1 = Math.max(0, Math.floor((h.y1 - Y1) / sy)),
      j2 = Math.min(G - 1, Math.floor((h.y2 - Y1) / sy))
    for (let j = j1; j <= j2; j++) {
      const y = Y1 + (j + 0.5) * sy
      for (let i = i1; i <= i2; i++) {
        if (inside(h.hull, X1 + (i + 0.5) * sx, y)) cover[j * G + i]++
      }
    }
  })
  let any = 0,
    multi = 0
  for (let k = 0; k < cover.length; k++) {
    if (cover[k]) any++
    if (cover[k] > 1) multi++
  }
  out.overlap = any ? multi / any : 0
  return out
}

// Current 2D positions and ticked groups of the shown nodes.
function currentSeparationInput() {
  const positions = {},
    groupsOf = {}
  shownNodes().forEach((n) => {
    positions[n.id()] = n.position()
    groupsOf[n.id()] = effectiveGroupsFor(n).filter((g) => g !== UNGROUPED)
  })
  return { positions, groupsOf }
}

function formatSeparation(r) {
  const pct = (v) => (Number.isFinite(v) ? `${(v * 100).toFixed(v < 0.1 ? 1 : 0)}%` : '—')
  return {
    silhouette: Number.isFinite(r.silhouette) ? r.silhouette.toFixed(2) : '—',
    intruders: pct(r.intruders),
    overlap: pct(r.overlap),
  }
}

let separationTimer = null

export function scheduleSeparation() {
  clearTimeout(separationTimer)
  separationTimer = setTimeout(updateSeparationBox, 350)
}

function updateSeparationBox() {
  const box = document.getElementById('sepValues')
  if (!box) return
  if (!cy.nodes().length) {
    box.innerHTML = '<span class="sep-empty">No network shown.</span>'
    return
  }
  const { positions, groupsOf } = currentSeparationInput()
  const r = groupSeparation(positions, groupsOf)
  if (r.groups < 2) {
    box.innerHTML = '<span class="sep-empty">Needs at least two ticked groups.</span>'
    return
  }
  const f = formatSeparation(r)
  const bar = Number.isFinite(r.silhouette)
    ? `<span class="sep-bar"><i style="left:${(((r.silhouette + 1) / 2) * 100).toFixed(1)}%"></i></span>`
    : ''
  box.innerHTML = `
    <div class="sep-row" title="Mean silhouette width of nodes in one group, from −1 (mixed) to 1 (cleanly separated)"><span>Silhouette</span><b>${f.silhouette}</b>${bar}</div>
    <div class="sep-row" title="Share of shown nodes lying inside the outline of a group they don't belong to"><span>Inside other outlines</span><b>${f.intruders}</b></div>
    <div class="sep-row" title="Share of the outlined area covered by two or more group outlines"><span>Outline overlap</span><b>${f.overlap}</b></div>`
}

// page wiring, run by main.ts in the original order
export function init() {
  cy.on('layoutstop dragfree', scheduleSeparation)
}
