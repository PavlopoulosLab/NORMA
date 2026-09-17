// @ts-nocheck
// ponytail: split verbatim from the former single-file script; typed one file at a time (remove @ts-nocheck as it lands)
import { S } from './state'
import { UNGROUPED, escapeHtml, getUsedGroups, nodeFillMode } from './network_state'
import { activeView, fitView } from './profiler'
import { currentTab } from './wiring'
import { cy } from './cy'
import {
  historyTimer,
  noteChange,
  redo,
  scheduleSettle,
  setHistoryBaseline,
  undo,
} from './demo_downloads'
import { libEntry, libSelection, normaLibrary, renderLibraryLists } from './layouts/controls'
import { refreshLibraryView } from './library'
import { sortedByName } from './hulls'
import { updateStats } from './metrics'
import { updateStringUI } from './string/ui_state'

/* ---------- what gets recorded ---------- */
const panelDisplayEl = document.getElementById('panelDisplay')

function typingInField(target) {
  if (!target) return false
  if (target.isContentEditable) return true
  if (target.tagName === 'TEXTAREA') return true
  if (target.tagName === 'INPUT') {
    const t = (target.type || '').toLowerCase()
    return !['checkbox', 'radio', 'range', 'button', 'color', 'file'].includes(t)
  }
  return false
}

/* ============================================================
   MULTIPLE SELECTION
   Shift/Ctrl/Cmd-click adds or removes nodes, Shift-drag on the
   background draws a selection box, Ctrl/Cmd+A selects every visible
   node, Escape clears. Selected nodes get a thick border and a halo;
   dragging one moves them all.
   ============================================================ */
let selectionUpdateQueued = false

function updateSelectionBar() {
  selectionUpdateQueued = false
  const nodes = cy.nodes(':selected').length
  const edges = cy.edges(':selected').length
  const bar = document.getElementById('selectionBar')
  bar.hidden = nodes + edges < 2 && !(nodes === 1 && edges > 0)
  if (bar.hidden) return
  const parts = []
  if (nodes) parts.push(`${nodes.toLocaleString()} node${nodes === 1 ? '' : 's'}`)
  if (edges) parts.push(`${edges.toLocaleString()} edge${edges === 1 ? '' : 's'}`)
  document.getElementById('selectionText').textContent = `${parts.join(' and ')} selected`
  document.getElementById('btnSelPaths').hidden = nodes !== 2
  document.getElementById('btnSelNeighbours').hidden = !nodes
}

/* ============================================================
   CONTEXT LINE: what the current view shows
   Network, grouping and node colors of the active view, shown next to
   the page tabs so they stay visible on every page.
   ============================================================ */
function contextChip(tint, label, value, title) {
  return `<span class="ctx-chip" data-tint="${tint}" title="${escapeHtml(label + ': ' + (title || value))}"><b>${escapeHtml(label)}</b><span>${escapeHtml(value)}</span></span>`
}

// Grouping choices for the current view: every annotation in the file
// library when the view was built from files, otherwise the groups that
// came with the data.
let groupingSelectKey = ''

export function renderGroupingSelect() {
  const sel = document.getElementById('viewGrouping')
  if (!sel) return
  const lib = S.currentLibView
  const groupCount = getUsedGroups().filter((g) => g !== UNGROUPED).length
  let options, value, disabled, title
  if (lib) {
    options = [['', 'None']].concat(
      sortedByName(normaLibrary.annotation, (e) => e.name).map((e) => [
        e.id,
        `${e.name} (${e.parsed.summary})`,
      ])
    )
    value = lib.annotation || ''
    disabled = false
    title = 'Group the nodes of this view by another annotation. Node positions are kept.'
  } else if (cy.nodes().length) {
    options = [
      ['data', groupCount ? `From the data (${groupCount} groups)` : 'No groups in the data'],
    ]
    value = 'data'
    disabled = true
    title =
      'This view was opened from a built-in demo or a JSON file, so its groups come with the data. Views opened from network files can switch annotations here.'
  } else {
    options = [['', 'No network shown']]
    value = ''
    disabled = true
    title = 'Open an example or files first.'
  }
  const key = JSON.stringify([options, value, disabled])
  if (key === groupingSelectKey) {
    updateGroupingDelete()
    return
  }
  groupingSelectKey = key
  sel.innerHTML = ''
  options.forEach(([v, t]) => sel.add(new Option(t, v)))
  sel.value = value
  sel.disabled = disabled
  sel.title = title
  updateGroupingDelete()
}

function updateGroupingDelete() {
  const btn = document.getElementById('btnGroupingDelete')
  if (!btn) return
  const id = S.currentLibView && S.currentLibView.annotation
  const e = id && libEntry('annotation', id)
  btn.disabled = !e
  btn.title = e ? `Completely remove the grouping "${e.name}"` : 'No grouping file to remove'
}

export function updateContextInfo() {
  renderGroupingSelect()
  if (typeof updateStringUI === 'function') updateStringUI()
  const el = document.getElementById('contextInfo')
  if (!el) return
  const v = typeof activeView === 'function' ? activeView() : null
  if (!cy.nodes().length) {
    el.innerHTML =
      '<span class="ctx-empty">Empty view. Open an example or files in the Upload Data tab, or fetch a network under Database importers.</span>'
    return
  }
  const lib = S.currentLibView
  const groups = getUsedGroups().filter((g) => g !== UNGROUPED)
  let network, grouping, colors
  if (lib) {
    const nets = lib.nets
      .split('|')
      .map((id) => libEntry('network', id))
      .filter(Boolean)
      .map((e) => e.name)
    network = nets.length ? nets.join(' + ') : 'network files'
    const ann = lib.annotation ? libEntry('annotation', lib.annotation) : null
    grouping = ann ? ann.name : 'none'
    const col = lib.colors ? libEntry('colors', lib.colors) : null
    const fill = nodeFillMode()
    colors =
      fill === 'groups'
        ? groups.length
          ? 'group colors'
          : 'default'
        : fill === 'values'
          ? `${col ? col.name : 'values'}: ${document.getElementById('valueColumn').value}`
          : col
            ? col.name
            : 'from the data'
  } else {
    network = v ? v.name : 'current data'
    grouping = groups.length ? 'from the data' : 'none'
    const hasOwn = cy.nodes().some((n) => n.data('nodeColor'))
    const fill = nodeFillMode()
    colors =
      fill === 'values'
        ? `values: ${document.getElementById('valueColumn').value}`
        : fill === 'data' && hasOwn
          ? 'from the data'
          : groups.length
            ? 'group colors'
            : 'default'
  }
  const groupText = groups.length
    ? `${grouping} (${groups.length.toLocaleString()} groups)`
    : grouping
  el.innerHTML =
    contextChip('data', 'Network', network) +
    contextChip('groups', 'Grouping', groupText) +
    contextChip('colors', 'Colors', colors)
}

// page wiring, run by main.ts in the original order
export function init() {
  ;['input', 'change'].forEach((type) => {
    panelDisplayEl.addEventListener(
      type,
      (e) => {
        if (e.target.id === 'groupFilter') return
        noteChange()
      },
      true
    )
  })

  panelDisplayEl.addEventListener(
    'click',
    (e) => {
      const btn = e.target.closest('button')
      if (
        !btn ||
        btn.disabled ||
        btn.classList.contains('helplink') ||
        btn.classList.contains('ginfo') ||
        btn.id === 'btnFit'
      )
        return
      if (btn.getAttribute('role') === 'radio') return // switching layout mode alone moves nothing
      noteChange()
    },
    true
  )

  ;['btnRefreshView', 'btnClear'].forEach((id) => {
    document.getElementById(id).addEventListener('click', noteChange, true)
  })

  document.getElementById('configFileInput').addEventListener('change', noteChange, true)

  cy.on('dragfree', 'node', noteChange)

  cy.on('layoutstop', () => {
    S.cyLayoutsRunning = Math.max(0, S.cyLayoutsRunning - 1)
    if (S.historySuspended) return
    // a layout the user started belongs to the open entry; one that ran by
    // itself (e.g. when data loads) just moves the starting point
    if (historyTimer || S.historyGestureOpen) scheduleSettle()
    else setHistoryBaseline()
  })

  document.getElementById('btnUndo').addEventListener('click', undo)

  document.getElementById('btnRedo').addEventListener('click', redo)

  document.addEventListener('keydown', (e) => {
    if (typingInField(e.target)) return
    const mod = e.ctrlKey || e.metaKey
    const key = e.key.toLowerCase()
    if (mod && key === 'z') {
      e.preventDefault()
      if (e.shiftKey) redo()
      else undo()
    } else if (mod && key === 'y') {
      e.preventDefault()
      redo()
    } else if (
      (currentTab === 'network' || currentTab === 'network3d') &&
      mod &&
      key === 'a' &&
      !['SELECT'].includes(e.target.tagName)
    ) {
      e.preventDefault()
      cy.nodes(':visible').select()
    } else if (
      (currentTab === 'network' || currentTab === 'network3d') &&
      key === 'escape' &&
      cy.$(':selected').length
    ) {
      cy.$(':selected').unselect()
    }
  })

  cy.on('select unselect remove', () => {
    if (selectionUpdateQueued) return
    selectionUpdateQueued = true
    requestAnimationFrame(updateSelectionBar)
  })

  document
    .getElementById('btnSelClear')
    .addEventListener('click', () => cy.$(':selected').unselect())

  document.getElementById('btnSelFit').addEventListener('click', () => {
    const sel = cy.$(':selected')
    if (sel.length)
      fitView(sel.union(sel.connectedNodes ? sel.connectedNodes() : cy.collection()), 60)
  })

  document.getElementById('viewGrouping').addEventListener('change', (e) => {
    if (!S.currentLibView) return
    libSelection.networks = new Set(S.currentLibView.nets.split('|').filter(Boolean))
    libSelection.annotation = e.target.value
    if (S.currentLibView.colors !== undefined) libSelection.colors = S.currentLibView.colors
    renderLibraryLists()
    refreshLibraryView()
    groupingSelectKey = ''
    updateStats()
  })
}
