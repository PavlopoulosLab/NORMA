// @ts-nocheck
// ponytail: split verbatim from the former single-file script; typed one file at a time (remove @ts-nocheck as it lands)
import {
  EDGE_PALETTES,
  EDGE_TYPES,
  NODE_PALETTES,
  ORIGINAL_EDGE_TYPES,
  colorAtIndex,
} from './palette'
import {
  LARGE_NETWORK_NODES,
  applyNodeSizing,
  cancelFrJobs,
  invalidateFullMetrics,
  updateStats,
} from './metrics'
import {
  MAX_NETWORK_NODES,
  RESERVED_EDGE_KEYS,
  RESERVED_NODE_KEYS,
  UNGROUPED,
  UNGROUPED_LABEL,
  buildAttrSchema,
  cleanValues,
  collectAttrs,
  colorForGroup,
  computeNodeVisualFields,
  effectiveGroupsFor,
  escapeHtml,
  getUsedGroups,
  groupDescription,
  groupLabel,
  nextAutoEdgeColor,
  refreshNodeVisual,
  renderAttrSchema,
  sanitizeColor,
  setGroupOrder,
} from './network_state'
import { S } from './state'
import {
  SHAPE_BY_KEY,
  applyEdgeCurveStyle,
  applyEdgeOpacity,
  applyEdgeWidth,
  closeShapePopover,
  hideEdgePopup,
  hideInfo,
  invalidateViewBox,
  openShapePopover,
  scheduleBundling,
  shapePopoverGroup,
  shapeSvg,
  showGroupInfo,
  updateLabelStyle,
} from './profiler'
import { applyEdgeDirection } from './export/dialog'
import { applyEdgeMerge, applyTypeVisibility } from './parallel_edges'
import { applyTheme } from './themes'
import { applyValueColors } from './clustering/mapping'
import { bumpDataVersion } from './demo_downloads'
import { currentTab, switchTab } from './wiring'
import { cy } from './cy'
import { drawBubbleSets } from './contours'
import { layoutMode, runLayout, runStrategyLayout } from './layouts/run'
import { plural, updateRefreshState, updateStrategyUI } from './layouts/controls'
import { reset3dForNewData } from './view3d/tab'
import { takeNextLoadLayout } from './examples'
import { toast } from './enrichment'
import { updateValueScalePreview } from './clustering/wiring'

/* ---------- group highlighting: convex hulls / fog clouds ---------- */
const hullCanvas = document.getElementById('groupHullCanvas')

const hullCtx = hullCanvas.getContext('2d')

// The shading canvas must match Cytoscape's drawing area and the current
// pixel ratio. The pixel ratio can change without a 'resize' event (a window
// dragged between a Retina and a standard display on macOS), and the canvas
// area can change size without one too, so drawGroupHulls() re-checks both.
export let hullCanvasCss = { w: 0, h: 0, dpr: 0 }

export function resizeHullCanvas() {
  const rect = document.getElementById('canvas').getBoundingClientRect()
  const w = rect.width || cy.width(),
    h = rect.height || cy.height()
  const dpr = window.devicePixelRatio || 1
  hullCanvas.width = Math.max(1, Math.round(w * dpr))
  hullCanvas.height = Math.max(1, Math.round(h * dpr))
  hullCanvas.style.width = w + 'px'
  hullCanvas.style.height = h + 'px'
  hullCanvasCss = { w, h, dpr }
  hullCtx.setTransform(dpr, 0, 0, dpr, 0, 0)
}

function hullCanvasStale() {
  return (
    hullCanvasCss.dpr !== (window.devicePixelRatio || 1) ||
    Math.abs(hullCanvasCss.w - cy.width()) > 0.5 ||
    Math.abs(hullCanvasCss.h - cy.height()) > 0.5
  )
}

// Andrew's monotone chain convex hull.
export function convexHull(points) {
  const pts = points.slice().sort((a, b) => a.x - b.x || a.y - b.y)
  const n = pts.length
  if (n < 3) return pts
  const cross = (o, a, b) => (a.x - o.x) * (b.y - o.y) - (a.y - o.y) * (b.x - o.x)
  const lower = []
  for (const p of pts) {
    while (lower.length >= 2 && cross(lower[lower.length - 2], lower[lower.length - 1], p) <= 0)
      lower.pop()
    lower.push(p)
  }
  const upper = []
  for (let i = n - 1; i >= 0; i--) {
    const p = pts[i]
    while (upper.length >= 2 && cross(upper[upper.length - 2], upper[upper.length - 1], p) <= 0)
      upper.pop()
    upper.push(p)
  }
  upper.pop()
  lower.pop()
  return lower.concat(upper)
}

// Pushes each hull vertex outward from the hull's centroid so the shape
// clears the nodes themselves rather than hugging their centers.
export function inflateHull(hull, padding) {
  const cx = hull.reduce((s, p) => s + p.x, 0) / hull.length
  const cy0 = hull.reduce((s, p) => s + p.y, 0) / hull.length
  return hull.map((p) => {
    const dx = p.x - cx,
      dy = p.y - cy0
    const len = Math.sqrt(dx * dx + dy * dy) || 1
    return { x: p.x + (dx / len) * padding, y: p.y + (dy / len) * padding }
  })
}

export function hexToRgba(hex, alpha) {
  const r = parseInt(hex.slice(1, 3), 16) || 0
  const g = parseInt(hex.slice(3, 5), 16) || 0
  const b = parseInt(hex.slice(5, 7), 16) || 0
  return `rgba(${r},${g},${b},${alpha})`
}

function paintHullPath(ctx, color, opacity, style) {
  ctx.fillStyle = hexToRgba(color, opacity)
  if (style === 'fog') {
    ctx.filter = 'blur(16px)'
    ctx.fill()
    ctx.filter = 'none'
  } else {
    ctx.fill()
    ctx.lineWidth = 1.5
    ctx.strokeStyle = hexToRgba(color, Math.min(1, opacity + 0.35))
    ctx.stroke()
  }
}

// Shading margin in screen pixels: full size from 100% zoom up, shrinking
// with the network when zoomed out so neighbouring groups stay apart.
let hullMarginScale = 1

export function drawGroupHulls() {
  hullMarginScale = Math.max(0.15, Math.min(1, cy.zoom()))
  if (!hullCtx) return
  if (hullCanvasStale()) resizeHullCanvas()
  hullCtx.setTransform(hullCanvasCss.dpr, 0, 0, hullCanvasCss.dpr, 0, 0)
  hullCtx.clearRect(0, 0, hullCanvasCss.w, hullCanvasCss.h)
  if (!document.getElementById('showGroupHulls').checked) return

  const style = document.getElementById('hullStyle').value
  const opacity = parseFloat(document.getElementById('hullOpacity').value) || 0.25
  if (style === 'bubble') {
    drawBubbleSets(hullCtx, opacity)
    return
  }
  const groups = getUsedGroups().filter((g) => S.activeGroups.has(g) && g !== UNGROUPED)

  groups.forEach((g) => {
    const nodes = cy
      .nodes()
      .filter((n) => !n.hasClass('hidden-group') && effectiveGroupsFor(n).includes(g))
    if (!nodes.length) return
    const color = S.nodeColorMap[g] || '#888888'
    const points = nodes.map((n) => {
      const rp = n.renderedPosition()
      return { x: rp.x, y: rp.y, r: (n.renderedWidth() || n.width() * cy.zoom()) / 2 }
    })

    hullCtx.save()
    if (points.length === 1) {
      const p = points[0]
      hullCtx.beginPath()
      hullCtx.arc(p.x, p.y, p.r + 28 * hullMarginScale, 0, Math.PI * 2)
      hullCtx.closePath()
      paintHullPath(hullCtx, color, opacity, style)
    } else if (points.length === 2) {
      const [a, b] = points
      const dx = b.x - a.x,
        dy = b.y - a.y
      const dist = Math.sqrt(dx * dx + dy * dy) || 1
      const nx = -dy / dist,
        ny = dx / dist
      const pad = Math.max(a.r, b.r) + 30 * hullMarginScale
      hullCtx.beginPath()
      hullCtx.moveTo(a.x + nx * pad, a.y + ny * pad)
      hullCtx.lineTo(b.x + nx * pad, b.y + ny * pad)
      // end caps bulge outward: around b away from a, around a away from b
      hullCtx.arc(b.x, b.y, pad, Math.atan2(ny, nx), Math.atan2(-ny, -nx), true)
      hullCtx.lineTo(a.x - nx * pad, a.y - ny * pad)
      hullCtx.arc(a.x, a.y, pad, Math.atan2(-ny, -nx), Math.atan2(ny, nx), true)
      hullCtx.closePath()
      paintHullPath(hullCtx, color, opacity, style)
    } else {
      const maxR = Math.max(...points.map((p) => p.r || 0))
      const hull = inflateHull(convexHull(points), 30 * hullMarginScale + maxR)
      // Both styles must actually contain every member node, so trace the
      // true polygon through the inflated hull points rather than rounding
      // the corners: a rounded corner falls short of the padded vertex by
      // an amount that grows with its distance from its neighbors, so a
      // far-dragged node ends up outside the shape (too far for "fog"'s
      // blur in paintHullPath to hide). "fog" gets its soft look from that
      // blur filter, not from rounding this path.
      hullCtx.beginPath()
      hullCtx.moveTo(hull[0].x, hull[0].y)
      for (let i = 1; i < hull.length; i++) hullCtx.lineTo(hull[i].x, hull[i].y)
      hullCtx.closePath()
      paintHullPath(hullCtx, color, opacity, style)
    }
    hullCtx.restore()
  })
}

export function applyGroupVisibility() {
  cy.batch(() => cy.nodes().forEach((n) => refreshNodeVisual(n)))
  invalidateViewBox()
  invalidateFullMetrics()
  if (document.getElementById('sizeMetric').value !== 'fixed') applyNodeSizing()
  updateStats()
  if (typeof scheduleBundling === 'function') scheduleBundling()
}

function groupFilterText() {
  const el = document.getElementById('groupFilter')
  return el ? el.value.trim().toLowerCase() : ''
}

// Groups whose label or key contains the filter text (all groups if empty).
// A–Z with numbers in order (Group-2 before Group-10), ignoring case and accents
const NAME_COLLATOR = new Intl.Collator(undefined, { numeric: true, sensitivity: 'base' })

export const byName = (a, b) => NAME_COLLATOR.compare(String(a), String(b))

export function sortedByName(list, nameOf = (x) => x) {
  return [...list].sort((a, b) => byName(nameOf(a), nameOf(b)))
}

function groupSortMode() {
  const el = document.getElementById('groupSortSelect')
  return el ? el.value : 'az'
}

// nodes per group, as the Node groups list counts them
function groupNodeCounts() {
  const counts = {}
  cy.nodes().forEach((n) =>
    (n.data('groups') || []).forEach((g) => {
      counts[g] = (counts[g] || 0) + 1
    })
  )
  return counts
}

// groups in the order lists and legends show them (colors keep their own order):
// A–Z, by number of nodes (either way, ties A–Z), or as in the file
export function displayGroups(groups) {
  const mode = groupSortMode()
  if (mode === 'file') return groups
  let rest = sortedByName(
    groups.filter((g) => g !== UNGROUPED),
    groupLabel
  )
  if (mode === 'size-desc' || mode === 'size-asc') {
    const counts = groupNodeCounts()
    const dir = mode === 'size-desc' ? -1 : 1
    rest = rest.sort(
      (a, b) => dir * ((counts[a] || 0) - (counts[b] || 0)) || byName(groupLabel(a), groupLabel(b))
    )
  }
  return groups.includes(UNGROUPED) ? rest.concat([UNGROUPED]) : rest
}

export function filteredGroups() {
  const q = groupFilterText()
  const groups = displayGroups(getUsedGroups())
  return q
    ? groups.filter((g) => groupLabel(g).toLowerCase().includes(q) || g.toLowerCase().includes(q))
    : groups
}

function updateGroupButtons() {
  const filtered = !!groupFilterText()
  document.getElementById('btnGroupsAll').textContent = filtered
    ? 'Activate matches'
    : 'Activate all'
  document.getElementById('btnGroupsNone').textContent = filtered
    ? 'Deactivate matches'
    : 'Deactivate all'
}

export function buildGroupLegend() {
  const el = document.getElementById('nodeGroupLegend')
  if (!el) return
  el.innerHTML = ''
  const allGroups = getUsedGroups()
  updateGroupButtons()
  if (!allGroups.length) {
    el.innerHTML =
      '<p style="font-size:12px; color:var(--muted); margin:0;">Load a network to see its groups.</p>'
    return
  }
  const groups = filteredGroups()
  if (!groups.length) {
    el.innerHTML =
      '<p style="font-size:12px; color:var(--muted); margin:0;">No group matches the filter.</p>'
    return
  }
  const counts = {}
  cy.nodes().forEach((n) =>
    (n.data('groups') || []).forEach((g) => {
      counts[g] = (counts[g] || 0) + 1
    })
  )
  groups.forEach((g) => {
    const count = counts[g] || 0
    const label = groupLabel(g)
    const description = groupDescription(g)
    const row = document.createElement('label')
    row.className = 'toggle-row'
    if (description) row.title = description
    row.innerHTML = `
      <input type="checkbox" ${S.activeGroups.has(g) ? 'checked' : ''} data-group="${escapeHtml(g)}">
      <input type="color" class="swatch-picker round" value="${S.nodeColorMap[g] || '#888888'}" title="Click to recolor the ${escapeHtml(label)} group">
      <button type="button" class="gshape" title="Node shape for this group" aria-label="Node shape for ${escapeHtml(label)}: ${escapeHtml((SHAPE_BY_KEY[S.groupShapes[g] || 'ellipse'] || [])[1] || 'Circle')}" aria-haspopup="listbox">${shapeSvg(S.groupShapes[g] || 'ellipse')}</button>
      <span class="label">${escapeHtml(label)}</span>
      <span class="count">${count}</span>
      <button type="button" class="ginfo" title="Show group details" aria-label="Show details for ${escapeHtml(label)}">i</button>
    `
    row.querySelector('.ginfo').addEventListener('click', (ev) => {
      ev.preventDefault()
      ev.stopPropagation()
      showGroupInfo(g)
    })
    row.querySelector('.gshape').addEventListener('click', (ev) => {
      ev.preventDefault()
      ev.stopPropagation()
      if (shapePopoverGroup === g) closeShapePopover(true)
      else openShapePopover(g, ev.currentTarget)
    })
    row.querySelector('input[type=checkbox]').addEventListener('change', (ev) => {
      if (ev.target.checked) S.activeGroups.add(g)
      else S.activeGroups.delete(g)
      applyGroupVisibility()
    })
    const picker = row.querySelector('.swatch-picker')
    picker.addEventListener('click', (ev) => ev.stopPropagation())
    picker.addEventListener('input', (ev) => {
      ev.stopPropagation()
      S.nodeColorMap[g] = ev.target.value
      cy.batch(() => cy.nodes().forEach((n) => refreshNodeVisual(n)))
    })
    el.appendChild(row)
  })
}

/* ============================================================
   CONFIG MODULE
   Every user-adjustable visual/style control is listed once here.
   getCurrentConfig() / applyConfig() are generated from this single
   table, so adding a new control to the "settings" file format is a
   one-line change here rather than hand-wiring an export and an
   import path separately. This is what backs the "Settings" section
   (export/import a standalone settings JSON) and the optional
   top-level "config" object a network-data file may also carry.
   ============================================================ */
export const CONFIG_FIELDS = [
  { id: 'themeSelect', prop: 'value' },
  { id: 'nodePaletteSelect', prop: 'value' },
  { id: 'nodeLabelSize', prop: 'value' },
  { id: 'nodeLabelColorMode', prop: 'value' },
  { id: 'nodeLabelColor', prop: 'value' },
  { id: 'edgeLabelColorMode', prop: 'value' },
  { id: 'edgeLabelColor', prop: 'value' },
  { id: 'labelScaleWithNode', prop: 'checked' },
  { id: 'edgeLabelContent', prop: 'value' },
  { id: 'edgeLabelOrientation', prop: 'value' },
  { id: 'edgeLabelSize', prop: 'value' },
  { id: 'labelMinScreenSize', prop: 'value' },
  { id: 'edgeOpacity', prop: 'value' },
  { id: 'bundleStrength', prop: 'value' },
  { id: 'valueColumn', prop: 'value' },
  { id: 'valueTransform', prop: 'value' },
  { id: 'valueScale', prop: 'value' },
  { id: 'valueCenter', prop: 'value' },
  { id: 'valueRange', prop: 'value' },
  { id: 'valueMin', prop: 'value' },
  { id: 'valueMax', prop: 'value' },
  { id: 'valueMissing', prop: 'value' },
  { id: 'valueSameRange', prop: 'checked' },
  { id: 'groupSortSelect', prop: 'value' },
  { id: 'legendShow', prop: 'checked' },
  { id: 'legendScale', prop: 'checked' },
  { id: 'legendGroups', prop: 'checked' },
  { id: 'legendShapes', prop: 'checked' },
  { id: 'legendChannels', prop: 'checked' },
  { id: 'legendTitle', prop: 'value' },
  { id: 'layout3d', prop: 'value' },
  { id: 'layout3dChannels', prop: 'checked' },
  { id: 'layer3dSpacing', prop: 'value' },
  { id: 'persp3d', prop: 'value' },
  { id: 'style3d', prop: 'value' },
  { id: 'fog3d', prop: 'value' },
  { id: 'grid3d', prop: 'checked' },
  { id: 'autoRotate3d', prop: 'checked' },
  { id: 'autoSpeed3d', prop: 'value' },
  { id: 'edgeDirection', prop: 'value' },
  { id: 'arrowShape', prop: 'value' },
  { id: 'arrowScale', prop: 'value' },
  { id: 'exportShownOnly', prop: 'checked' },
  { id: 'nodeFillSelect', prop: 'value' },
  { id: 'edgePaletteSelect', prop: 'value' },
  { id: 'showNodeLabels', prop: 'checked' },
  { id: 'showEdgeLabels', prop: 'checked' },
  { id: 'labelPosition', prop: 'value' },
  { id: 'edgeCurveStyle', prop: 'value' },
  { id: 'edgeMergeMode', prop: 'value' },
  { id: 'edgeCurvature', prop: 'value' },
  { id: 'edgeWidthMode', prop: 'value' },
  { id: 'edgeWidthFixed', prop: 'value' },
  { id: 'edgeWidthMin', prop: 'value' },
  { id: 'edgeWidthMax', prop: 'value' },
  { id: 'sizeMetric', prop: 'value' },
  { id: 'sizeMin', prop: 'value' },
  { id: 'sizeMax', prop: 'value' },
  { id: 'sizeChannelOnly', prop: 'checked' },
  { id: 'nodeScale', prop: 'value' },
  { id: 'showGroupHulls', prop: 'checked' },
  { id: 'hullStyle', prop: 'value' },
  { id: 'hullOpacity', prop: 'value' },
  { id: 'layoutSelect', prop: 'value' },
  { id: 'layoutOnActiveOnly', prop: 'checked' },
  { id: 'layoutMode', prop: 'value' },
  { id: 'groupArrangement', prop: 'value' },
  { id: 'strategyAlgorithm', prop: 'value' },
  { id: 'localGroupLayout', prop: 'value' },
  { id: 'groupForce', prop: 'value' },
  { id: 'groupClusterRadius', prop: 'value' },
  { id: 'searchMode', prop: 'value' },
  { id: 'searchCase', prop: 'checked' },
  { id: 'searchAttrs', prop: 'checked' },
]

export function getCurrentConfig() {
  const cfg = {}
  CONFIG_FIELDS.forEach(({ id, prop }) => {
    const el = document.getElementById(id)
    if (!el) return
    cfg[id] = prop === 'checked' ? el.checked : el.value
  })
  return cfg
}

// Sets every control named in cfg, then re-runs each style/layout
// function once so the change actually takes visual effect -- a control's
// own change-listener only fires on real user interaction, not on a value
// set programmatically, so this step can't be skipped.
// Settings files saved before the three NORMA-2.0 strategies had a single
// "layout by groups" toggle (the super-node strategy) with its own global
// layout and a 0.5-4 spacing slider.
function upgradeLegacyConfig(cfg) {
  const out = { ...cfg }
  if (out.groupStrategy === undefined && 'groupedLayoutToggle' in out) {
    out.groupStrategy = out.groupedLayoutToggle ? 'supernodes' : 'none'
    if (out.groupedLayoutToggle && out.globalGroupLayout && out.layoutSelect === undefined) {
      out.layoutSelect = out.globalGroupLayout
    }
  }
  // settings from before "by connections / by groups": a strategy select
  // whose algorithm was the main layout choice
  if (out.layoutMode === undefined && out.groupStrategy !== undefined) {
    if (out.groupStrategy === 'none') {
      out.layoutMode = 'connections'
    } else {
      out.layoutMode = 'groups'
      out.groupArrangement = out.groupStrategy
      if (out.layoutSelect) out.strategyAlgorithm = out.layoutSelect
    }
  }
  if (out.groupForce === undefined && out.groupSpacing !== undefined) {
    const spacing = parseFloat(out.groupSpacing)
    if (Number.isFinite(spacing))
      out.groupForce = String(Math.min(20, Math.max(1, Math.round(spacing / 0.16))))
  }
  return out
}

export function applyConfig(cfg) {
  if (!cfg || typeof cfg !== 'object') return
  cfg = upgradeLegacyConfig(cfg)
  CONFIG_FIELDS.forEach(({ id, prop }) => {
    if (!(id in cfg)) return
    const el = document.getElementById(id)
    if (!el) return
    if (prop === 'checked') el.checked = !!cfg[id]
    else el.value = cfg[id]
  })
  refreshAllDerivedUI()
}

export function refreshAllDerivedUI() {
  applyTheme(document.getElementById('themeSelect').value)
  if (typeof applyValueColors === 'function') {
    updateValueScalePreview()
    applyValueColors()
  }
  applyNodePalette(document.getElementById('nodePaletteSelect').value, true)
  applyEdgePalette(document.getElementById('edgePaletteSelect').value)
  updateLabelStyle()
  applyEdgeCurveStyle()
  applyEdgeWidth()
  applyNodeSizing()
  applyEdgeOpacity()
  applyEdgeDirection()
  document.getElementById('hullControls').style.display = document.getElementById('showGroupHulls')
    .checked
    ? 'block'
    : 'none'
  updateStrategyUI()
  drawGroupHulls()
}

// Cuts data with too many nodes to the first MAX_NETWORK_NODES.
function capNetworkData(data) {
  if (!data || !Array.isArray(data.nodes) || data.nodes.length <= MAX_NETWORK_NODES) return data
  const total = data.nodes.length
  const nodes = data.nodes.slice(0, MAX_NETWORK_NODES)
  const keep = new Set(nodes.map((n) => n.id))
  const edges = (data.edges || []).filter((e) => keep.has(e.source) && keep.has(e.target))
  const text = `This network has ${total.toLocaleString('en-US')} nodes; NORMA shows up to ${MAX_NETWORK_NODES.toLocaleString('en-US')}, so only the first ${MAX_NETWORK_NODES.toLocaleString('en-US')} and the ${plural(edges.length, 'edge')} among them are shown.`
  setTimeout(() => {
    if (typeof toast === 'function') toast(text, 'warn')
    const el = document.getElementById('normaStatus')
    if (el) {
      const note = document.createElement('div')
      note.className = 'note warn'
      note.textContent = text
      el.prepend(note)
    }
  }, 0)
  return { ...data, nodes, edges }
}

// Extra legend sections that come with generated views (e.g. the comparison).
export let legendExtra = null

export function loadData(data, opts = {}) {
  data = capNetworkData(data)
  legendExtra = Array.isArray(data.legendExtra) ? data.legendExtra : null
  bumpDataVersion()
  S.currentLibView = null
  S.nodeColorMap = {}
  S.groupShapes =
    data.groupShapes && typeof data.groupShapes === 'object' ? { ...data.groupShapes } : {}
  S.groupAttrs =
    data.groupAttrs && typeof data.groupAttrs === 'object' ? { ...data.groupAttrs } : {}
  setGroupOrder(data.groupOrder)
  // give groups their palette colors in display order
  if (Array.isArray(data.groupOrder)) {
    data.groupOrder.forEach((g) => {
      const explicit = sanitizeColor(
        (data.nodeColors && data.nodeColors[g]) || (S.groupAttrs[g] && S.groupAttrs[g].color)
      )
      if (explicit) S.nodeColorMap[g] = explicit
      else colorForGroup(g)
    })
  }
  cy.elements().remove()

  const usedTypes = new Set()
  const els = []

  data.nodes.forEach((n) => {
    const groupsArr = n.groups && n.groups.length ? n.groups : [n.group || UNGROUPED]
    if (groupsArr.includes(UNGROUPED) && !S.groupAttrs[UNGROUPED]) {
      S.groupAttrs[UNGROUPED] = {
        label: UNGROUPED_LABEL,
        description: 'Nodes that no group lists as a member',
      }
    }
    groupsArr.forEach((g) => {
      // explicit color precedence: nodeColors[g], then groupAttrs[g].color
      const explicit = sanitizeColor(
        (data.nodeColors && data.nodeColors[g]) || (S.groupAttrs[g] && S.groupAttrs[g].color)
      )
      if (explicit) S.nodeColorMap[g] = explicit
    })
    // a node's own "color" is kept separately and used when
    // Colors -> Node fill is set to "Node colors from data"
    const visual = computeNodeVisualFields(groupsArr)
    els.push({
      group: 'nodes',
      data: {
        id: n.id,
        size: n.size || 42,
        baseSize: n.size || 42,
        group: groupsArr.join(', '),
        groups: groupsArr,
        nodeColor: n.color || null,
        values: n.values && typeof n.values === 'object' ? cleanValues(n.values) : null,
        valueColor: null,
        labelPx: 12,
        labelColor: '#111827',
        shape: 'ellipse',
        attrs: collectAttrs(n, RESERVED_NODE_KEYS),
        ...visual,
      },
    })
  })

  data.edges.forEach((e, i) => {
    const type = e.type || 'link'
    usedTypes.add(type)
    const explicit = sanitizeColor((data.edgeColors && data.edgeColors[type]) || e.color)
    if (!EDGE_TYPES[type]) {
      EDGE_TYPES[type] = { color: explicit || nextAutoEdgeColor(), label: type }
    } else if (explicit) {
      EDGE_TYPES[type].color = explicit
    }
    els.push({
      group: 'edges',
      data: {
        id: e.id || 'e' + i,
        source: e.source,
        target: e.target,
        type,
        color: EDGE_TYPES[type].color,
        elabel: '',
        directed: e.directed === undefined ? !!data.directed : !!e.directed,
        arrow: 'none',
        attrs: collectAttrs(e, RESERVED_EDGE_KEYS),
        ...(typeof e.weight === 'number' ? { weight: e.weight } : {}),
      },
    })
  })

  cy.add(els)
  S.activeTypes = new Set(usedTypes.size ? usedTypes : Object.keys(EDGE_TYPES))
  S.activeGroups = new Set(getUsedGroups())
  buildLegend([...usedTypes])
  buildGroupLegend()
  applyTypeVisibility()
  applyGroupVisibility()
  invalidateFullMetrics()
  buildAttrSchema()
  renderAttrSchema()
  hideInfo()
  hideEdgePopup()
  if (currentTab === 'welcome' && typeof switchTab === 'function') switchTab('network')
  if (data.config) {
    applyConfig(data.config) // also covers applyEdgeWidth() and applyNodeSizing()
  } else {
    applyEdgeWidth()
    applyNodeSizing()
  }
  const nodeCount = cy.nodes().length
  updateRefreshState()
  if (!opts.positions && typeof reset3dForNewData === 'function') reset3dForNewData()
  if (opts.positions) {
    // restoring a saved view: put nodes back where they were
    cancelFrJobs()
    S.layoutRunSeq++
    cy.nodes().positions((n) => opts.positions[n.id()] || n.position())
  } else if (layoutMode() === 'groups' && nodeCount <= LARGE_NETWORK_NODES) {
    runStrategyLayout()
  } else {
    // a data file's settings may pick the opening layout
    const preferred = (data.config && data.config.layoutSelect) || takeNextLoadLayout()
    runLayout(nodeCount > LARGE_NETWORK_NODES ? 'grid' : preferred || 'cose')
  }
  applyEdgeDirection() // also merges parallel edges
  if (typeof applyValueColors === 'function') applyValueColors()
  updateStats()
}

export function buildLegend(typesInData) {
  const el = document.getElementById('edgeLegend')
  el.innerHTML = ''
  const list = typesInData.length ? typesInData : Object.keys(EDGE_TYPES)
  list.forEach((type) => {
    const meta = EDGE_TYPES[type] || { color: '#6c7793', label: type }
    const count = cy.edges().filter((e) => e.data('type') === type).length
    const row = document.createElement('label')
    row.className = 'toggle-row'
    row.innerHTML = `
      <input type="checkbox" ${S.activeTypes.has(type) ? 'checked' : ''} data-type="${escapeHtml(type)}">
      <input type="color" class="swatch-picker" value="${meta.color}" title="Click to recolor the ${escapeHtml(meta.label)} channel">
      <span class="label">${escapeHtml(meta.label)}</span>
      <span class="count">${count}</span>
    `
    row.querySelector('input[type=checkbox]').addEventListener('change', (ev) => {
      if (ev.target.checked) S.activeTypes.add(type)
      else S.activeTypes.delete(type)
      applyTypeVisibility()
    })
    const picker = row.querySelector('.swatch-picker')
    picker.addEventListener('click', (ev) => ev.stopPropagation())
    picker.addEventListener('input', (ev) => {
      ev.stopPropagation()
      const newColor = ev.target.value
      if (!EDGE_TYPES[type]) EDGE_TYPES[type] = { color: newColor, label: type }
      else EDGE_TYPES[type].color = newColor
      cy.edges()
        .filter((ed) => ed.data('type') === type)
        .forEach((ed) => ed.data('color', newColor))
      applyEdgeMerge()
    })
    el.appendChild(row)
  })
}

export function getUsedTypes() {
  return [...new Set(cy.edges().map((e) => e.data('type')))]
}

// keepDataColors: groups whose data gives a color keep it (settings applied
// from files or the API); choosing a palette by hand recolors every group.
export function applyNodePalette(name, keepDataColors = false) {
  S.currentNodePalette = name
  const palette = NODE_PALETTES[name] || NODE_PALETTES.vivid
  const groups = Object.keys(S.nodeColorMap).filter((g) => g !== UNGROUPED)
  groups.forEach((g, i) => {
    const own = keepDataColors && sanitizeColor(S.groupAttrs[g] && S.groupAttrs[g].color)
    S.nodeColorMap[g] = own || colorAtIndex(palette, i)
  })
  cy.batch(() => cy.nodes().forEach((n) => refreshNodeVisual(n)))
  buildGroupLegend()
}

export function applyEdgePalette(name) {
  const types = getUsedTypes()
  if (name === 'classic') {
    types.forEach((t) => {
      EDGE_TYPES[t] = ORIGINAL_EDGE_TYPES[t]
        ? { ...ORIGINAL_EDGE_TYPES[t] }
        : EDGE_TYPES[t] || { color: nextAutoEdgeColor(), label: t }
    })
  } else {
    const palette = EDGE_PALETTES[name] || EDGE_PALETTES.categorical
    types.forEach((t, i) => {
      const label = (EDGE_TYPES[t] && EDGE_TYPES[t].label) || t
      EDGE_TYPES[t] = { color: colorAtIndex(palette, i), label }
    })
  }
  cy.edges().forEach((e) => {
    const meta = EDGE_TYPES[e.data('type')]
    if (meta) e.data('color', meta.color)
  })
  buildLegend(types)
  applyEdgeMerge()
}

// page wiring, run by main.ts in the original order
export function init() {
  window.addEventListener('resize', () => {
    resizeHullCanvas()
    drawGroupHulls()
  })

  if (typeof ResizeObserver !== 'undefined') {
    new ResizeObserver(() => {
      resizeHullCanvas()
      drawGroupHulls()
    }).observe(document.getElementById('canvas'))
  }
}
