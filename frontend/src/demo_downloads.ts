// @ts-nocheck
// ponytail: split verbatim from the former single-file script; typed one file at a time (remove @ts-nocheck as it lands)
import { EDGE_TYPES } from './palette'
import { S } from './state'
import { SAMPLE_GENERATORS } from './sample_data'
import { activeView, restoreView, snapshotData, viewSettings } from './profiler'
import { cy } from './cy'
import { downloadText, libSelection } from './layouts/controls'
import { escapeHtml } from './network_state'
import { getUsedTypes } from './hulls'

/* ---------- built-in demo downloads (Help > Example files) ---------- */
// Converts a demo in loadData's JSON shape to NORMA's text formats.
export function demoToNormaTexts(data) {
  const weighted = data.edges.some((e) => typeof e.weight === 'number')
  const typed = new Set(data.edges.map((e) => e.type || 'link')).size > 1
  const isDirected = (e) => (e.directed === undefined ? !!data.directed : !!e.directed)
  const withDirection = data.edges.some(isDirected)
  const header = ['Source', 'Target']
  if (weighted) header.push('Weight')
  if (typed) header.push('Type')
  if (withDirection) header.push('Direction')
  const netLines = [header.join('\t')]
  data.edges.forEach((e) => {
    const row = [e.source, e.target]
    if (weighted) row.push(String(typeof e.weight === 'number' ? e.weight : 1))
    if (typed) row.push(e.type || 'link')
    if (withDirection) row.push(isDirected(e) ? 'directed' : 'undirected')
    netLines.push(row.join('\t'))
  })
  const members = new Map()
  ;(data.groupOrder || []).forEach((g) => members.set(g, []))
  data.nodes.forEach((n) => {
    const groups = n.groups && n.groups.length ? n.groups : n.group ? [n.group] : []
    groups.forEach((g) => {
      if (!members.has(g)) members.set(g, [])
      members.get(g).push(n.id)
    })
  })
  const attrs = data.groupAttrs || {}
  const annLines = [...members]
    .filter(([, m]) => m.length)
    .map(([g, m]) => `${(attrs[g] && attrs[g].label) || g}\t${m.join(',')}`)
  return { network: netLines.join('\n') + '\n', annotation: annLines.join('\n') + '\n' }
}

const DEMO_DOWNLOADS = [
  [
    'trp',
    'E. coli trp operon, multi-edge',
    'Channels, weights, gene function and EC number attributes, group descriptions',
  ],
  ['modules50', '4 overlapping modules (50 nodes)', 'Attributes on nodes, edges and groups'],
  ['modules100', '4 overlapping modules (100 nodes)', 'Attributes on nodes, edges and groups'],
  [
    'tiny',
    'Random network with edge labels (20 nodes)',
    'Each edge has an interaction attribute, shown as its label',
  ],
  [
    'directed',
    'Random directed multi-edge network (40 nodes)',
    'Arrows from source to target, up to three channels per pair, some reciprocal edges; the network file has a Direction column',
  ],
  ['small', 'Random network (60 nodes)', ''],
  ['medium', 'Random network (200 nodes)', ''],
  ['large', 'Random network (800 nodes)', ''],
  ['massive', 'Random network (5,000 nodes)', ''],
]

function renderDemoDownloads() {
  const root = document.getElementById('helpDemoFiles')
  if (!root) return
  const table = document.createElement('table')
  table.className = 'help-table'
  table.innerHTML = '<thead><tr><th>Demo</th><th>Download</th></tr></thead>'
  const body = document.createElement('tbody')
  DEMO_DOWNLOADS.forEach(([key, title, note]) => {
    const tr = document.createElement('tr')
    const td1 = document.createElement('td')
    td1.innerHTML = `${escapeHtml(title)}${note ? `<span class="stat-desc">${escapeHtml(note)}</span>` : ''}`
    const td2 = document.createElement('td')
    td2.className = 'dl-buttons'
    const make = (label, fileName, build) => {
      const btn = document.createElement('button')
      btn.type = 'button'
      btn.className = 'dl-copy'
      btn.textContent = label
      btn.title = `Download ${fileName}`
      btn.addEventListener('click', () => downloadText(fileName, build()))
      td2.appendChild(btn)
    }
    const gen = () => SAMPLE_GENERATORS[key]()
    make('Network', `demo-${key}-network.txt`, () => demoToNormaTexts(gen()).network)
    make('Groups', `demo-${key}-annotation.txt`, () => demoToNormaTexts(gen()).annotation)
    make('JSON', `demo-${key}.json`, () => JSON.stringify(gen(), null, 2))
    tr.append(td1, td2)
    body.appendChild(tr)
  })
  table.appendChild(body)
  root.appendChild(table)
}

/* ============================================================
   UNDO / REDO
   Each view keeps its own history. An entry is the view's full visual
   state (settings, node positions, ticked groups and channels, colors,
   spread) plus a reference to its data; data snapshots are reused until
   the data actually changes, so moving a node doesn't copy the network.
   Recording: the first change after a quiet moment pushes the last
   settled state onto the undo stack; further changes within the same
   burst (slider drags, layout animations) join that entry, and the state
   settles once nothing has changed for a moment. Entries that turn out to
   change nothing are dropped. Zoom, pan, search and selection are not
   part of the history, and neither is the theme.
   ============================================================ */
const HISTORY_LIMIT = 60

const HISTORY_SETTLE_MS = 450

export let historyTimer = null

export let dataVersion = 0

export function bumpDataVersion() {
  dataVersion++
}

function viewHistory() {
  const v = activeView()
  if (!v) return null
  if (!v.history) v.history = { undo: [], redo: [], committed: null }
  return v.history
}

function currentDataSnapshot() {
  if (S.dataCache.version !== dataVersion)
    S.dataCache = { version: dataVersion, data: snapshotData() }
  return S.dataCache.data
}

function captureHistoryState() {
  const positions = {}
  cy.nodes().forEach((n) => {
    const p = n.position()
    positions[n.id()] = { x: p.x, y: p.y }
  })
  const typeColors = {}
  getUsedTypes().forEach((t) => {
    if (EDGE_TYPES[t]) typeColors[t] = EDGE_TYPES[t].color
  })
  return {
    data: currentDataSnapshot(),
    selection: {
      networks: [...libSelection.networks],
      annotation: libSelection.annotation,
      colors: libSelection.colors,
    },
    state: {
      config: viewSettings(),
      positions,
      activeGroups: [...S.activeGroups],
      activeTypes: [...S.activeTypes],
      groupColors: { ...S.nodeColorMap },
      typeColors,
      libView: S.currentLibView ? { ...S.currentLibView } : null,
      spread: parseFloat(document.getElementById('spreadSlider').value) || 0,
      groupFilter: document.getElementById('groupFilter').value,
    },
  }
}

function sameHistoryState(a, b) {
  if (!a || !b || a.data !== b.data) return false
  const pa = a.state.positions,
    pb = b.state.positions
  const ids = Object.keys(pa)
  if (ids.length !== Object.keys(pb).length) return false
  for (const id of ids) {
    const p = pa[id],
      q = pb[id]
    if (!q || Math.abs(p.x - q.x) > 0.01 || Math.abs(p.y - q.y) > 0.01) return false
  }
  const strip = (e) => JSON.stringify({ ...e.state, positions: null, sel: e.selection })
  return strip(a) === strip(b)
}

function layoutStillRunning() {
  return (
    S.cyLayoutsRunning > 0 ||
    document.getElementById('btnRunLayout').disabled ||
    cy.nodes().animated()
  )
}

export function scheduleSettle() {
  clearTimeout(historyTimer)
  historyTimer = setTimeout(settleHistory, HISTORY_SETTLE_MS)
}

function settleHistory() {
  clearTimeout(historyTimer)
  historyTimer = null
  const h = viewHistory()
  if (!h) {
    S.historyGestureOpen = false
    return
  }
  if (layoutStillRunning()) {
    scheduleSettle()
    return
  }
  const now = captureHistoryState()
  if (S.historyGestureOpen && h.undo.length && sameHistoryState(h.undo[h.undo.length - 1], now)) {
    h.undo.pop()
  }
  h.committed = now
  S.historyGestureOpen = false
  updateUndoButtons()
}

// Settles right away (used before switching views and before undo/redo).
export function flushHistory() {
  if (historyTimer || S.historyGestureOpen) {
    clearTimeout(historyTimer)
    historyTimer = null
    const h = viewHistory()
    if (h) {
      const now = captureHistoryState()
      if (S.historyGestureOpen && h.undo.length && sameHistoryState(h.undo[h.undo.length - 1], now))
        h.undo.pop()
      h.committed = now
    }
    S.historyGestureOpen = false
  }
}

// Call after something the user did changed (or is about to change) the view.
export function noteChange() {
  if (S.historySuspended) return
  const h = viewHistory()
  if (!h) return
  if (!S.historyGestureOpen) {
    if (h.committed) {
      h.undo.push(h.committed)
      if (h.undo.length > HISTORY_LIMIT) h.undo.shift()
      h.redo.length = 0
    }
    S.historyGestureOpen = true
  }
  scheduleSettle()
  updateUndoButtons()
}

// Records the current state as the starting point (no undo entry).
export function setHistoryBaseline() {
  S.historyGestureOpen = false
  scheduleSettle()
}

function applyHistoryState(entry) {
  S.historySuspended++
  try {
    const reuse = entry.data === currentDataSnapshot() && cy.nodes().length > 0
    restoreView(
      { data: entry.data, selection: entry.selection, state: { ...entry.state } },
      { reuseData: reuse }
    )
    if (!reuse) S.dataCache = { version: dataVersion, data: entry.data }
  } finally {
    S.historySuspended--
  }
}

export function undo() {
  flushHistory()
  const h = viewHistory()
  if (!h || !h.undo.length) return
  const target = h.undo.pop()
  if (h.committed) h.redo.push(h.committed)
  applyHistoryState(target)
  h.committed = target
  updateUndoButtons()
}

export function redo() {
  flushHistory()
  const h = viewHistory()
  if (!h || !h.redo.length) return
  const target = h.redo.pop()
  if (h.committed) h.undo.push(h.committed)
  applyHistoryState(target)
  h.committed = target
  updateUndoButtons()
}

export function updateUndoButtons() {
  const h = viewHistory()
  const u = document.getElementById('btnUndo'),
    r = document.getElementById('btnRedo')
  const canUndo = !!h && h.undo.length > 0
  const canRedo = !!h && h.redo.length > 0 && !S.historyGestureOpen
  u.disabled = !canUndo
  r.disabled = !canRedo
  u.title = canUndo
    ? `Undo (Ctrl+Z), ${h.undo.length} step${h.undo.length === 1 ? '' : 's'} available`
    : 'Nothing to undo'
  r.title = canRedo
    ? `Redo (Ctrl+Shift+Z), ${h.redo.length} step${h.redo.length === 1 ? '' : 's'} available`
    : 'Nothing to redo'
}

// page wiring, run by main.ts in the original order
export function init() {
  renderDemoDownloads()

  cy.on('layoutstart', () => {
    S.cyLayoutsRunning++
  })
}
