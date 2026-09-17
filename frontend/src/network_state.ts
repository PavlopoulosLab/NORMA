// @ts-nocheck
// ponytail: split verbatim from the former single-file script; typed one file at a time (remove @ts-nocheck as it lands)
import { EDGE_TYPES, NODE_PALETTES, colorAtIndex } from './palette'
import { NORMA_CFG } from './config'
import { S } from './state'
import { cy } from './cy'
import { nodeShapeFor, refreshEdgeLabelOptions } from './profiler'
import { parseNumber } from './clustering/mcl'

const AUTO_EDGE_COLORS = ['#6c7793', '#d68fc0', '#7fd68a', '#e8a15f', '#8fb8e8', '#e07a7a']

export const PIE_MAX_SLICES = 16

// Nodes that belong to no group are collected under this key so they still
// have something to toggle in the legend. It is never written out as a
// real group (JSON export omits it; NORMA export skips it).
export const UNGROUPED = '__ungrouped__'

export const UNGROUPED_LABEL = 'Not in any group'

// Fill for nodes without their own color when coloring nodes from data,
// matching NORMA's default gray.
const NO_NODE_COLOR = '#9aa0a6'

// Keeps only numeric (or missing) entries of a JSON values object.
export function cleanValues(obj) {
  const out = {}
  let any = false
  Object.entries(obj).forEach(([k, v]) => {
    const x = typeof v === 'number' ? v : parseNumber(v)
    if (x === undefined) return
    out[k] = x
    any = true
  })
  return any ? out : null
}

export function nodeFillMode() {
  const el = document.getElementById('nodeFillSelect')
  return el ? el.value : 'groups'
}

/* ============================================================
   ATTRIBUTE MODULE
   One model for all three levels of the network:

     level  | structural (reserved) keys          | where custom attrs live
     -------+-------------------------------------+--------------------------------
     node   | id, group, groups, size, color      | extra fields, or node.attrs
     edge   | id, source, target, type, color,    | extra fields, or edge.attrs
            | weight                              |
     group  | label, description, color           | extra fields in groupAttrs[g],
            |                                     | or groupAttrs[g].attrs

   Reserved keys drive rendering; everything else is carried through
   untouched, shown in the inspectors via one shared formatter, summarised
   per level in the sidebar "Attributes" section, and written back out
   on export in the same shape it came in.
   ============================================================ */
// NORMA shows networks of up to this many nodes; larger ones are cut to
// their first nodes (in file order) with a message.
export const MAX_NETWORK_NODES = Math.max(
  100,
  Math.min(50000, parseInt(NORMA_CFG.app.maxNodes, 10) || 10000)
)

export const RESERVED_NODE_KEYS = new Set([
  'id',
  'group',
  'groups',
  'size',
  'color',
  'values',
  'attrs',
])

export const RESERVED_EDGE_KEYS = new Set([
  'id',
  'source',
  'target',
  'type',
  'color',
  'weight',
  'directed',
  'attrs',
])

const RESERVED_GROUP_KEYS = new Set(['label', 'description', 'color', 'attrs'])

export function collectAttrs(obj, reservedKeys) {
  if (!obj || typeof obj !== 'object') return {}
  const attrs = { ...(obj.attrs && typeof obj.attrs === 'object' ? obj.attrs : {}) }
  Object.keys(obj).forEach((k) => {
    if (!reservedKeys.has(k)) attrs[k] = obj[k]
  })
  return attrs
}

// Colors from loaded network/session data (nodeColors, groupAttrs.color, ...)
// are untrusted and end up in style/attribute values, so only a strict CSS
// color syntax is let through; anything else (e.g. an attribute-breakout
// attempt) is rejected rather than escaped, since these values are used both
// in innerHTML templates and as literal Cytoscape style colors.
export function sanitizeColor(c) {
  if (typeof c !== 'string') return null
  const v = c.trim()
  if (/^#[0-9a-fA-F]{3,4}$|^#[0-9a-fA-F]{6}$|^#[0-9a-fA-F]{8}$/.test(v)) return v
  if (/^(rgb|rgba|hsl|hsla)\(\s*[\d.]+%?(\s*,\s*[\d.]+%?){2,3}\s*\)$/.test(v)) return v
  if (/^[a-zA-Z]{3,20}$/.test(v)) return v
  return null
}

export function escapeHtml(str) {
  return String(str).replace(
    /[&<>"']/g,
    (ch) =>
      ({
        '&': '&amp;',
        '<': '&lt;',
        '>': '&gt;',
        '"': '&quot;',
        "'": '&#39;',
      })[ch]
  )
}

export function attrKind(v) {
  if (v === null || v === undefined || v === '') return 'empty'
  if (Array.isArray(v)) return 'list'
  if (typeof v === 'number') return Number.isFinite(v) ? 'number' : 'text'
  if (typeof v === 'boolean') return 'boolean'
  if (typeof v === 'object') return 'object'
  return 'text'
}

export function formatAttrValue(v) {
  switch (attrKind(v)) {
    case 'empty':
      return '—'
    case 'number':
      return Number.isInteger(v) ? String(v) : String(+v.toPrecision(4))
    case 'boolean':
      return v ? 'yes' : 'no'
    case 'list':
      return v
        .map((x) => {
          const k = attrKind(x)
          return k === 'list' || k === 'object' ? JSON.stringify(x) : formatAttrValue(x)
        })
        .join(', ')
    case 'object':
      return JSON.stringify(v)
    default:
      return String(v)
  }
}

// Plain-text form used by search, so lists and objects are matched on
// their contents rather than on "[object Object]".
function attrSearchText(attrs) {
  return Object.values(attrs || {})
    .map(formatAttrValue)
    .join(' ')
    .toLowerCase()
}

export function attrRowsHtml(attrs, rowClass) {
  return Object.entries(attrs || {})
    .map(
      ([k, v]) =>
        `<div class="${rowClass}"><span>${escapeHtml(k)}</span><span>${escapeHtml(formatAttrValue(v))}</span></div>`
    )
    .join('')
}

export function groupLabel(g) {
  return (S.groupAttrs[g] && S.groupAttrs[g].label) || g
}

export function groupDescription(g) {
  return (S.groupAttrs[g] && S.groupAttrs[g].description) || ''
}

export function customGroupAttrs(g) {
  return collectAttrs(S.groupAttrs[g], RESERVED_GROUP_KEYS)
}

// Summary of which attribute keys the loaded network uses, per level:
// { node: { key: { count, kinds:Set, min, max, values:Set } }, edge: {...}, group: {...} }
export let attrSchema = { node: {}, edge: {}, group: {} }

const SCHEMA_DISTINCT_CAP = 50

function recordAttrs(bucket, attrs) {
  Object.entries(attrs || {}).forEach(([k, v]) => {
    const entry =
      bucket[k] ||
      (bucket[k] = { count: 0, kinds: new Set(), min: Infinity, max: -Infinity, values: new Set() })
    const kind = attrKind(v)
    if (kind === 'empty') return
    entry.count++
    entry.kinds.add(kind)
    if (kind === 'number') {
      entry.min = Math.min(entry.min, v)
      entry.max = Math.max(entry.max, v)
    } else if (kind === 'text' && entry.values.size <= SCHEMA_DISTINCT_CAP) {
      entry.values.add(v)
    } else if (kind === 'list' && entry.values.size <= SCHEMA_DISTINCT_CAP) {
      v.forEach((x) => entry.values.add(formatAttrValue(x)))
    }
  })
}

export function buildAttrSchema() {
  attrSchema = { node: {}, edge: {}, group: {} }
  cy.nodes().forEach((n) => recordAttrs(attrSchema.node, n.data('attrs')))
  cy.edges().forEach((e) => recordAttrs(attrSchema.edge, e.data('attrs')))
  getUsedGroups().forEach((g) => recordAttrs(attrSchema.group, customGroupAttrs(g)))
}

function describeSchemaEntry(entry) {
  if (entry.kinds.size > 1) return 'mixed: ' + [...entry.kinds].join(', ')
  const kind = [...entry.kinds][0]
  if (kind === 'number') {
    return entry.min === entry.max
      ? `number, always ${formatAttrValue(entry.min)}`
      : `number, ${formatAttrValue(entry.min)} to ${formatAttrValue(entry.max)}`
  }
  if (kind === 'text') {
    const n = entry.values.size
    if (n > SCHEMA_DISTINCT_CAP) return `text, over ${SCHEMA_DISTINCT_CAP} values`
    return n <= 4 ? `text: ${[...entry.values].join(', ')}` : `text, ${n} values`
  }
  if (kind === 'boolean') return 'yes / no'
  if (kind === 'list') {
    const n = entry.values.size
    return n > SCHEMA_DISTINCT_CAP
      ? `list, over ${SCHEMA_DISTINCT_CAP} distinct items`
      : `list, ${n} distinct ${n === 1 ? 'item' : 'items'}`
  }
  return kind || 'empty'
}

export function renderAttrSchema() {
  refreshEdgeLabelOptions()
  const el = document.getElementById('attrSchema')
  if (!el) return
  const totals = { node: cy.nodes().length, edge: cy.edges().length, group: getUsedGroups().length }
  if (!totals.node) {
    el.innerHTML = '<p class="schema-empty">Load a network to see its attributes.</p>'
    return
  }
  const titles = { node: 'Nodes', edge: 'Edges', group: 'Groups' }
  el.innerHTML = ['node', 'edge', 'group']
    .map((level) => {
      const entries = Object.entries(attrSchema[level]).filter(([, e]) => e.count > 0)
      const rows = entries.length
        ? entries
            .map(
              ([k, e]) => `
          <div class="schema-row" title="${escapeHtml(k)}">
            <span class="k">${escapeHtml(k)}</span>
            <span class="cov">${e.count}/${totals[level]}</span>
            <span class="kind">${escapeHtml(describeSchemaEntry(e))}</span>
          </div>`
            )
            .join('')
        : '<p class="schema-empty">No custom attributes</p>'
      return `<div class="schema-block">
      <div class="schema-head">${titles[level]}<span>${totals[level]}</span></div>
      ${rows}
    </div>`
    })
    .join('')
}

// "Not in any group" is always neutral gray and doesn't use up a palette color.
export function colorForGroup(g) {
  if (!S.nodeColorMap[g]) {
    if (g === UNGROUPED) {
      S.nodeColorMap[g] = NO_NODE_COLOR
    } else {
      const palette = NODE_PALETTES[S.currentNodePalette] || NODE_PALETTES.vivid
      const idx = Object.keys(S.nodeColorMap).filter((k) => k !== UNGROUPED).length
      S.nodeColorMap[g] = colorAtIndex(palette, idx)
    }
  }
  return S.nodeColorMap[g]
}

export function nextAutoEdgeColor() {
  const c = colorAtIndex(AUTO_EDGE_COLORS, S.autoEdgeIdx)
  S.autoEdgeIdx++
  return c
}

// Computes the color/pie-slice data fields for a node given its list of groups.
// A single-group node gets one 100% slice (looks like a plain colored circle);
// a multi-group node gets one evenly-sized slice per group, in order.
// Slices are equal-sized. A node in more groups than there are slices shows
// its first PIE_MAX_SLICES - 1 groups plus one gray slice sized for the rest.
export function computeNodeVisualFields(groupsArr) {
  const all = groupsArr && groupsArr.length ? groupsArr : [UNGROUPED]
  const overflow = all.length > PIE_MAX_SLICES
  const shown = overflow ? all.slice(0, PIE_MAX_SLICES - 1) : all
  const slices = shown.map((g) => ({ color: colorForGroup(g), size: 100 / all.length }))
  if (overflow)
    slices.push({ color: NO_NODE_COLOR, size: (100 * (all.length - shown.length)) / all.length })
  const fields = { color: slices[0].color }
  for (let i = 0; i < PIE_MAX_SLICES; i++) {
    const idx = i + 1
    fields['pieColor' + idx] = i < slices.length ? slices[i].color : '#000000'
    fields['pieSize' + idx] = i < slices.length ? slices[i].size.toFixed(3) + '%' : '0%'
  }
  return fields
}

// Optional display order for groups (e.g. the line order of an annotation
// file); groups not listed keep node order, and "Not in any group" is last.
let groupOrderHint = null

export function getUsedGroups() {
  const groups = [
    ...new Set(
      cy
        .nodes()
        .map((n) => n.data('groups') || [])
        .flat()
    ),
  ]
  const rank = (g) =>
    g === UNGROUPED
      ? Infinity
      : groupOrderHint && groupOrderHint.has(g)
        ? groupOrderHint.get(g)
        : 1e9
  return groups
    .map((g, i) => [g, i])
    .sort((a, b) => rank(a[0]) - rank(b[0]) || a[1] - b[1])
    .map((x) => x[0])
}

export function setGroupOrder(names) {
  groupOrderHint = Array.isArray(names) ? new Map(names.map((g, i) => [g, i])) : null
}

// A node's *effective* groups are whichever of its assigned groups are
// currently active. Deactivating one of a node's several groups doesn't just
// toggle visibility -- the node is redrawn using only its remaining active
// groups, so a 2-group node with one group turned off becomes a plain single
// color instead of staying a two-slice pie.
export function effectiveGroupsFor(node) {
  const groups = node.data('groups') || []
  return groups.filter((g) => S.activeGroups.has(g))
}

// One solid color across the whole node, used when node fill comes from
// each node's own "color" field (e.g. a NORMA node-coloring file).
function solidVisualFields(color) {
  const fields = { color, pieColor1: color, pieSize1: '100%' }
  for (let i = 2; i <= PIE_MAX_SLICES; i++) {
    fields['pieColor' + i] = '#000000'
    fields['pieSize' + i] = '0%'
  }
  return fields
}

export function refreshNodeVisual(node) {
  const eff = effectiveGroupsFor(node)
  if (!eff.length) {
    node.addClass('hidden-group')
    return
  }
  if (node.hasClass('hidden-group')) node.removeClass('hidden-group')
  const fill = nodeFillMode()
  const visual =
    fill === 'data'
      ? solidVisualFields(node.data('nodeColor') || NO_NODE_COLOR)
      : fill === 'values'
        ? solidVisualFields(
            node.data('valueColor') ||
              document.getElementById('valueMissing').value ||
              NO_NODE_COLOR
          )
        : computeNodeVisualFields(eff)
  visual.shape = nodeShapeFor(eff)
  visual.labelColor = visual.color
  if (visual.shape !== 'ellipse') {
    // a single-color shaped node: the fill carries the color, no pie
    for (let i = 1; i <= PIE_MAX_SLICES; i++) visual['pieSize' + i] = '0%'
  }
  // only write fields that changed, in a single update
  const changed = {}
  let any = false
  for (const k in visual) {
    if (node.data(k) !== visual[k]) {
      changed[k] = visual[k]
      any = true
    }
  }
  if (any) node.data(changed)
}
