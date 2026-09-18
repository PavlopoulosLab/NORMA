// @ts-nocheck
// ponytail: split verbatim from the former single-file script; typed one file at a time (remove @ts-nocheck as it lands)
import cytoscape from 'cytoscape'
import {
  ARENA3D_LAYER_SPACING,
  ARENA3D_LAYER_WIDTH,
  ARENA3D_MAX_LAYERS,
  arenaSafe,
  exportArena3d,
  hexColor,
  openInArena3d,
} from './contours'
import {
  BUNDLE_MAX_EDGES,
  activeView,
  captureActiveView,
  createView,
  openInNewView,
  renderViewBar,
  restoreView,
  setControls,
  snapshotData,
  viewSettings,
  views,
} from './profiler'
import { EDGE_TYPES, NODE_PALETTES, colorAtIndex } from './palette'
import {
  MAX_NETWORK_NODES,
  UNGROUPED,
  effectiveGroupsFor,
  escapeHtml,
  groupLabel,
  nextAutoEdgeColor,
  nodeFillMode,
  sanitizeColor,
} from './network_state'
import { NORMA_CFG } from './config'
import {
  NORMA_PARSERS,
  addNormaEntry,
  downloadText,
  fileStem,
  libEntry,
  libSelection,
  listSample,
  nextPaint,
  normaLibrary,
  parseNormaNetwork,
  plural,
  readTextWithProgress,
  renderLibraryLists,
  setStatus,
  startProgress,
} from './layouts/controls'
import { S } from './state'
import { WEBGL_ACTIVE, cy } from './cy'
import {
  applyGroupColorOverrides,
  benjaminiHochberg,
  groupStatsTsv,
  hypergeomUpper,
  renderGroupStats,
} from './arena3d'
import { applyGroupsAndColorsInPlace, refreshLibraryView } from './library'
import { applyTheme } from './themes'
import {
  applyValueColors,
  groupSeparation,
  valueColumns,
  viewHasValues,
} from './clustering/mapping'
import {
  bundleAsync,
  exportEdges,
  exportNodes,
  exportShownOnly,
  frLayoutAsync,
  shownEdges,
  shownNodes,
} from './metrics'
import { compareState } from './label_colors'
import { currentTab, louvain, profileGraph, simpleGraph, switchTab } from './wiring'
import {
  dataVersion,
  demoToNormaTexts,
  setHistoryBaseline,
  updateUndoButtons,
} from './demo_downloads'
import { edgeIsDirected } from './export/dialog'
import { generateRandomNetwork } from './sample_data'
import { loadData, refreshAllDerivedUI, sortedByName } from './hulls'
import { net3d, requestRender3d, rgbOf } from './view3d/state'
import { openGroupNetwork } from './group_network'
import { readFileText } from './uploads'
import { resolveStringRoute } from './string/requests'

/* ---------- enrichment ---------- */
export const groupAnalysisState = { stats: null, enrichment: null }

function annotationChoices() {
  const out = [{ value: 'view', label: 'Groups of the current view' }]
  sortedByName(normaLibrary.annotation, (e) => e.name).forEach((e) =>
    out.push({ value: e.id, label: e.name })
  )
  return out
}

// Map term -> Set(member ids) for an annotation choice
function termSetsFor(choice) {
  const terms = new Map()
  if (choice === 'view') {
    cy.nodes().forEach((n) =>
      (n.data('groups') || []).forEach((g) => {
        if (g === UNGROUPED) return
        const label = groupLabel(g)
        if (!terms.has(label)) terms.set(label, new Set())
        terms.get(label).add(n.id())
      })
    )
    return terms
  }
  const entry = libEntry('annotation', choice)
  if (!entry) return terms
  entry.parsed.groups.forEach(({ name, members }) => terms.set(name, new Set(members)))
  return terms
}

export function refreshEnrichmentChoices() {
  ;['gaTerms', 'gaSetsFrom'].forEach((id) => {
    const sel = document.getElementById(id)
    const current = sel.value
    sel.innerHTML = ''
    annotationChoices().forEach((o) => sel.add(new Option(o.label, o.value)))
    if ([...sel.options].some((o) => o.value === current)) sel.value = current
    else if (id === 'gaTerms') {
      // default: terms from a file other than the view's own grouping
      const own = S.currentLibView && S.currentLibView.annotation
      const other = normaLibrary.annotation.find((e) => e.id !== own)
      if (other) sel.value = other.id
    }
  })
  const groupsMode = document.getElementById('gaSets').value === 'groups'
  document.getElementById('gaSetsFromRow').hidden = !groupsMode
  document.getElementById('gaSetsFromLabel').hidden = !groupsMode
  const selected = cy.nodes(':selected').length
  document.getElementById('gaSelectedNote').textContent =
    document.getElementById('gaSets').value === 'selected'
      ? selected
        ? `${plural(selected, 'node')} selected in the view.`
        : 'Select nodes in the 2D or 3D view first (Shift-drag or Shift-click).'
      : ''
}

function runEnrichment() {
  const setsMode = document.getElementById('gaSets').value
  const termChoice = document.getElementById('gaTerms').value
  const bgMode = document.getElementById('gaBackground').value
  const minOverlap = Math.max(1, parseInt(document.getElementById('gaMinOverlap').value, 10) || 2)
  const fdrMax = parseFloat(document.getElementById('gaFdr').value) || 0.05
  const shown = new Set(shownNodes().map((n) => n.id()))
  if (!shown.size) {
    setStatus('gaEnrichStatus', [{ level: 'error', text: 'Show a network first.' }])
    return
  }
  const terms = termSetsFor(termChoice)
  if (!terms.size) {
    setStatus('gaEnrichStatus', [{ level: 'error', text: 'The chosen annotation has no groups.' }])
    return
  }
  // universe
  let universe
  if (bgMode === 'view') universe = shown
  else {
    universe = new Set()
    terms.forEach((set) => set.forEach((id) => universe.add(id)))
    shown.forEach((id) => universe.add(id))
  }
  // test sets
  const tests = []
  if (setsMode === 'selected') {
    const sel = cy
      .nodes(':selected')
      .map((n) => n.id())
      .filter((id) => universe.has(id))
    if (!sel.length) {
      setStatus('gaEnrichStatus', [
        {
          level: 'error',
          text: 'Select nodes in the view first (Shift-drag or Shift-click), or test each group of an annotation.',
        },
      ])
      return
    }
    tests.push({ name: 'Selected nodes', members: new Set(sel) })
  } else {
    termSetsFor(document.getElementById('gaSetsFrom').value).forEach((set, name) => {
      const members = new Set([...set].filter((id) => universe.has(id)))
      if (members.size >= minOverlap) tests.push({ name, members })
    })
  }
  const N = universe.size
  const termList = [...terms]
    .map(([name, set]) => ({ name, set: new Set([...set].filter((id) => universe.has(id))) }))
    .filter((t) => t.set.size >= minOverlap)
  const rows = []
  let tested = 0
  tests.forEach((t) => {
    const n = t.members.size
    const cand = termList
      .map((term) => {
        let k = 0
        term.set.forEach((id) => {
          if (t.members.has(id)) k++
        })
        return {
          term: term.name,
          k,
          K: term.set.size,
          genes: [...term.set].filter((id) => t.members.has(id)),
        }
      })
      .filter(
        (c) =>
          c.k >= 1 &&
          !(
            setsMode === 'groups' &&
            c.term === t.name &&
            document.getElementById('gaSetsFrom').value === termChoice
          )
      )
    const ps = cand.map((c) => hypergeomUpper(c.k, N, c.K, n))
    const qs = benjaminiHochberg(ps)
    tested += cand.length
    cand.forEach((c, i) => {
      if (c.k < minOverlap || qs[i] > fdrMax) return
      rows.push({
        set: t.name,
        n,
        term: c.term,
        k: c.k,
        K: c.K,
        N,
        fold: c.k / n / (c.K / N),
        p: ps[i],
        fdr: qs[i],
        genes: c.genes,
      })
    })
  })
  rows.sort((a, b) => a.fdr - b.fdr || a.p - b.p)
  groupAnalysisState.enrichment = { rows, tests: tests.length, tested, N, termChoice, setsMode }
  renderEnrichment()
  setStatus('gaEnrichStatus', [
    {
      level: rows.length ? 'ok' : 'warn',
      text: `Tested ${plural(tested, 'term–set pair')} in ${plural(tests.length, 'set')} against a background of ${plural(N, 'node')}: ${rows.length ? `${plural(rows.length, 'enriched term')} with FDR ≤ ${fdrMax}` : `no term reaches FDR ≤ ${fdrMax}`}.`,
    },
  ])
  document.getElementById('btnGaEnrichTsv').disabled = !rows.length
}

function renderEnrichment() {
  const st = groupAnalysisState.enrichment
  const root = document.getElementById('gaEnrichResults')
  if (!st || !st.rows.length) {
    root.innerHTML = ''
    return
  }
  const fp = (v) => (v < 1e-3 ? v.toExponential(2) : v.toFixed(4))
  const shown = st.rows.slice(0, 300)
  root.innerHTML = `<div class="table-wrap"><table class="data">
    <thead><tr>${st.setsMode === 'groups' ? '<th scope="col">Set</th>' : ''}<th scope="col">Term</th><th scope="col">In set</th><th scope="col">Term size</th><th scope="col">Fold</th><th scope="col">p-value</th><th scope="col">FDR</th><th scope="col"></th></tr></thead>
    <tbody>${shown
      .map(
        (r, i) => `<tr>
      ${st.setsMode === 'groups' ? `<td>${escapeHtml(r.set)}</td>` : ''}
      <td>${escapeHtml(r.term)}</td><td class="num">${r.k} / ${r.n}</td><td class="num">${r.K} / ${r.N}</td>
      <td class="num">${r.fold.toFixed(2)}</td><td class="num">${fp(r.p)}</td><td class="num">${fp(r.fdr)}</td>
      <td><button type="button" class="ga-show" data-row="${i}" title="Select these nodes in the view">Select</button></td></tr>`
      )
      .join('')}</tbody>
  </table></div>${st.rows.length > shown.length ? `<p class="sub">Showing the first ${shown.length} of ${st.rows.length}; the download has all.</p>` : ''}`
  root.querySelectorAll('.ga-show').forEach((b) =>
    b.addEventListener('click', () => {
      const r = shown[+b.dataset.row]
      cy.batch(() => {
        cy.$(':selected').unselect()
        r.genes.forEach((id) => cy.$id(id).select())
      })
      switchTab('network')
      const sel = cy.nodes(':selected')
      if (sel.length) cy.animate({ fit: { eles: sel, padding: 80 } }, { duration: 300 })
    })
  )
}

function enrichmentTsv() {
  const st = groupAnalysisState.enrichment
  if (!st) return
  const lines = [
    [
      'Set',
      'Set size',
      'Term',
      'Overlap',
      'Term size',
      'Background',
      'Fold enrichment',
      'p-value',
      'FDR (BH)',
      'Nodes',
    ].join('\t'),
    ...st.rows.map((r) =>
      [
        r.set,
        r.n,
        r.term,
        r.k,
        r.K,
        r.N,
        r.fold.toFixed(4),
        r.p.toExponential(4),
        r.fdr.toExponential(4),
        r.genes.join(','),
      ].join('\t')
    ),
  ]
  downloadText('enrichment.tsv', lines.join('\n') + '\n')
}

/* ============================================================
   COMPARISON AS A NETWORK
   The compared networks merged into one view: each edge's channel says
   which networks contain it (A only, B only, A + B, ...), and each node's
   group says which networks contain the node.
   ============================================================ */
function patternLabel(mask, nets) {
  const letters = nets.map((_, i) => String.fromCharCode(65 + i)).filter((_, i) => mask & (1 << i))
  if (letters.length === nets.length) return nets.length === 2 ? 'Shared' : 'In all'
  return letters.length === 1 ? `${letters[0]} only` : letters.join(' + ')
}

function openComparisonNetwork() {
  const nets = compareState.results
  if (!nets || nets.length < 2) return
  const nodeMask = new Map()
  nets.forEach((net, i) =>
    net.ids.forEach((id) => nodeMask.set(id, (nodeMask.get(id) || 0) | (1 << i)))
  )
  const edgeMask = new Map()
  nets.forEach((net, i) =>
    net.edgeSet.forEach((key) => edgeMask.set(key, (edgeMask.get(key) || 0) | (1 << i)))
  )
  const full = (1 << nets.length) - 1
  const colorFor = (mask) => {
    if (mask === full) return '#9ca3af'
    const bits = nets.map((_, i) => i).filter((i) => mask & (1 << i))
    if (bits.length === 1) return nets[bits[0]].color
    return colorAtIndex(NODE_PALETTES.vivid, mask + 3)
  }
  const edgeColors = {},
    nodeColors = {}
  const edges = [...edgeMask].map(([key, mask], i) => {
    const [a, b, dir] = key.split('\t')
    const type = patternLabel(mask, nets)
    edgeColors[type] = colorFor(mask)
    return {
      id: 'c' + i,
      source: a,
      target: b,
      type,
      directed: dir === 'directed',
      'in networks': type,
    }
  })
  const nodes = [...nodeMask].map(([id, mask]) => {
    const g = `Nodes: ${patternLabel(mask, nets)}`
    nodeColors[g] = colorFor(mask)
    return { id, groups: [g] }
  })
  // groups and channels in a readable order: single networks first, shared last
  const order = (masks) =>
    [...new Set(masks)].sort(
      (x, y) => (x === full) - (y === full) || popcount(x) - popcount(y) || x - y
    )
  const popcount = (x) => {
    let c = 0
    while (x) {
      c += x & 1
      x >>= 1
    }
    return c
  }
  const data = {
    nodes,
    edges,
    nodeColors,
    edgeColors,
    groupOrder: order([...nodeMask.values()]).map((m) => `Nodes: ${patternLabel(m, nets)}`),
    groupAttrs: Object.fromEntries(
      nets.map((net, i) => [
        `Nodes: ${String.fromCharCode(65 + i)} only`,
        { description: `Only in ${net.name}` },
      ])
    ),
    config: {
      layoutSelect: 'fr',
      edgeCurveStyle: 'bezier',
      legendShow: true,
      legendGroups: true,
      legendChannels: true,
      legendTitle: 'Network comparison',
      nodeFillSelect: 'groups',
    },
    legendExtra: [
      {
        title: 'Networks',
        items: nets.map((net, i) => ({
          kind: 'glyph',
          label: `${String.fromCharCode(65 + i)}: ${net.name}`,
          color: net.color,
        })),
      },
    ],
  }
  const title = nets.map((n, i) => `${String.fromCharCode(65 + i)}`).join(' vs ')
  openInNewView(`Comparison ${title}: ${nets.map((n) => n.name).join(' / ')}`.slice(0, 80), () => {
    loadData(data)
    applyGroupColorOverrides(data.nodeColors)
  })
  switchTab('network')
  const counts = order([...edgeMask.values()]).map(
    (m) => `${patternLabel(m, nets)}: ${[...edgeMask.values()].filter((x) => x === m).length}`
  )
  setStatus('cmpStatus', [
    {
      level: 'ok',
      text: `Opened the merged network with ${plural(nodes.length, 'node')} and ${plural(edges.length, 'edge')} (${counts.join(', ')}). Tick channels in the Display tab to show or hide each part.`,
    },
  ])
}

/* ============================================================
   LOCAL EXPLORATION
   Neighbourhood of a node (or the selected nodes) and all shortest paths
   between two selected nodes, each opened as a new view that keeps the
   nodes' positions, groups, colors and attributes.
   ============================================================ */
function adjacencyForPaths() {
  // follows edge directions where edges are directed
  const out = new Map(),
    inn = new Map()
  const add = (m, a, b) => {
    if (!m.has(a)) m.set(a, new Set())
    m.get(a).add(b)
  }
  shownEdges(true).forEach((e) => {
    const s = e.data('source'),
      t = e.data('target')
    if (s === t) return
    add(out, s, t)
    add(inn, t, s)
    if (!edgeIsDirected(e)) {
      add(out, t, s)
      add(inn, s, t)
    }
  })
  return { out, inn }
}

function bfsDistances(start, adj, limit = Infinity) {
  const dist = new Map([[start, 0]])
  const q = [start]
  for (let h = 0; h < q.length; h++) {
    const v = q[h]
    const d = dist.get(v)
    if (d >= limit) continue
    ;(adj.get(v) || []).forEach((w) => {
      if (!dist.has(w)) {
        dist.set(w, d + 1)
        q.push(w)
      }
    })
  }
  return dist
}

function openSubnetworkView(name, keepIds, marks, keepEdge) {
  const data = snapshotData()
  const keep = new Set(keepIds)
  data.nodes = data.nodes
    .filter((n) => keep.has(n.id))
    .map((n) => (marks.has(n.id) ? { ...n, role: marks.get(n.id) } : n))
  const shownEdgeIds = new Set(shownEdges(true).map((e) => e.id()))
  data.edges = data.edges.filter(
    (e) =>
      shownEdgeIds.has(e.id) &&
      keep.has(e.source) &&
      keep.has(e.target) &&
      (!keepEdge || keepEdge(e))
  )
  const used = new Set(data.nodes.flatMap((n) => n.groups))
  data.groupOrder = data.groupOrder.filter((g) => used.has(g))
  const positions = {}
  cy.nodes().forEach((n) => {
    if (keep.has(n.id())) positions[n.id()] = { ...n.position() }
  })
  const config = viewSettings()
  openInNewView(name, () => {
    setControls(config)
    loadData(data, { positions })
    refreshAllDerivedUI()
  })
  switchTab(net3dActiveTab())
  cy.batch(() =>
    cy.nodes().forEach((n) => {
      if (marks.has(n.id())) n.select()
    })
  )
  cy.fit(undefined, 60)
}

function net3dActiveTab() {
  return net3d.active ? 'network3d' : 'network'
}

function openNeighbourhood(ids, steps) {
  const { out, inn } = adjacencyForPaths()
  // neighbours in either direction
  const both = new Map()
  ;[out, inn].forEach((m) =>
    m.forEach((set, k) => {
      if (!both.has(k)) both.set(k, new Set())
      set.forEach((v) => both.get(k).add(v))
    })
  )
  const keep = new Set()
  ids.forEach((id) => bfsDistances(id, both, steps).forEach((_, v) => keep.add(v)))
  const marks = new Map(ids.map((id) => [id, 'center']))
  const label = ids.length === 1 ? ids[0] : `${ids.length} nodes`
  openSubnetworkView(`${label}: ${steps}-step neighbourhood`, [...keep], marks)
  toast(`Opened the ${steps}-step neighbourhood of ${label}: ${plural(keep.size, 'node')}.`)
}

function openShortestPaths(a, b) {
  const { out, inn } = adjacencyForPaths()
  const ds = bfsDistances(a, out),
    dt = bfsDistances(b, inn)
  if (!ds.has(b)) {
    toast(`No path leads from ${a} to ${b} in the shown part of the network.`, 'warn')
    return
  }
  const d = ds.get(b)
  const keep = [...ds.keys()].filter((v) => dt.has(v) && ds.get(v) + dt.get(v) === d)
  const keepSet = new Set(keep)
  // only edges that lie on a shortest path
  const onPath = (e) => {
    const s = e.source,
      t = e.target
    const fwd = ds.has(s) && dt.has(t) && ds.get(s) + 1 + dt.get(t) === d
    const rev = !e.directed && ds.has(t) && dt.has(s) && ds.get(t) + 1 + dt.get(s) === d
    return keepSet.has(s) && keepSet.has(t) && (fwd || rev)
  }
  const marks = new Map([
    [a, 'source'],
    [b, 'target'],
  ])
  openSubnetworkView(`Shortest paths ${a} → ${b}`, keep, marks, onPath)
  const count = countShortestPaths(a, b, out, ds, d)
  toast(
    `Opened ${plural(count, 'shortest path')} of length ${d} from ${a} to ${b} (${plural(keep.length, 'node')}).`
  )
}

function countShortestPaths(a, b, out, ds, d) {
  const ways = new Map([[a, 1]])
  const layers = [...ds].filter(([, x]) => x <= d).sort((x, y) => x[1] - y[1])
  layers.forEach(([v, x]) => {
    const w = ways.get(v) || 0
    ;(out.get(v) || []).forEach((u) => {
      if (ds.get(u) === x + 1) ways.set(u, (ways.get(u) || 0) + w)
    })
  })
  return ways.get(b) || 0
}

export function toast(text, level = 'ok') {
  let el = document.getElementById('toast')
  if (!el) {
    el = document.createElement('div')
    el.id = 'toast'
    el.setAttribute('role', 'status')
    el.setAttribute('aria-live', 'polite')
    document.getElementById('canvas').appendChild(el)
  }
  el.className = `toast ${level}`
  el.textContent = text
  el.hidden = false
  clearTimeout(toast.timer)
  toast.timer = setTimeout(() => {
    el.hidden = true
  }, 6000)
}

// remembers the order in which nodes were selected (paths go first -> second)
const selectionOrder = []

/* ============================================================
   TIME SERIES AND CONDITIONS
   With several numeric columns (time points, conditions) the view can
   step through them, by hand or as an animation, with the layout fixed
   and, by default, one color scale for all steps. Columns of other
   numeric expression files can be added as further steps.
   ============================================================ */
const frameState = { playing: false, timer: null }

function frameColumns() {
  return valueColumns()
}

function numericLibraryEntries() {
  return normaLibrary.colors.filter((e) => e.parsed.numeric)
}

export function updateFrameControls() {
  const box = document.getElementById('valueFrames')
  if (!box) return
  const cols = viewHasValues() ? frameColumns() : []
  const otherFiles = numericLibraryEntries().length
  box.hidden = !(cols.length > 1 || (cols.length && otherFiles > 1))
  const slider = document.getElementById('frameSlider')
  slider.max = Math.max(0, cols.length - 1)
  const i = Math.max(0, cols.indexOf(document.getElementById('valueColumn').value))
  slider.value = i
  document.getElementById('frameLabel').textContent = cols.length
    ? `Step ${i + 1} of ${cols.length}: ${cols[i]}`
    : ''
  document.getElementById('btnFramePlay').textContent = frameState.playing ? '❚❚ Pause' : '▶ Play'
  const badge = document.getElementById('frameBadge')
  badge.hidden =
    box.hidden ||
    (nodeFillMode() !== 'values' && document.getElementById('sizeMetric').value !== 'value')
  badge.textContent = cols.length ? `${cols[i]}  ·  ${i + 1} / ${cols.length}` : ''
}

function showFrame(i) {
  const cols = frameColumns()
  if (!cols.length) return
  const k = ((i % cols.length) + cols.length) % cols.length
  document.getElementById('valueColumn').value = cols[k]
  applyValueColors()
  if (net3d.active) requestRender3d()
}

function stepFrame(delta) {
  const cols = frameColumns()
  showFrame(Math.max(0, cols.indexOf(document.getElementById('valueColumn').value)) + delta)
}

function toggleFramePlay() {
  frameState.playing = !frameState.playing
  clearInterval(frameState.timer)
  if (frameState.playing) {
    if (nodeFillMode() !== 'values') {
      document.getElementById('nodeFillSelect').value = 'values'
      document
        .getElementById('nodeFillSelect')
        .dispatchEvent(new Event('change', { bubbles: true }))
    }
    const ms = parseInt(document.getElementById('frameSpeed').value, 10) || 1000
    frameState.timer = setInterval(() => {
      if (!cy.nodes().length || !viewHasValues()) {
        toggleFramePlay()
        return
      }
      stepFrame(1)
    }, ms)
  }
  updateFrameControls()
}

export function stopFramePlay() {
  if (frameState.playing) toggleFramePlay()
}

// Adds the columns of every numeric expression file in Files as steps.
function addFramesFromFiles() {
  const entries = numericLibraryEntries()
  if (!entries.length) return
  const nodeIds = new Set(cy.nodes().map((n) => n.id()))
  const existing = new Set(frameColumns())
  const added = []
  cy.batch(() => {
    entries.forEach((entry) => {
      entry.parsed.columns.forEach((col) => {
        const name = entry.parsed.columns.length === 1 ? entry.name : `${entry.name}: ${col}`
        // the view's own numeric file is already there under its plain column names
        const ownFile = S.currentLibView && S.currentLibView.colors === entry.id
        if (existing.has(name) || (ownFile && existing.has(col))) return
        existing.add(name)
        added.push(name)
        cy.nodes().forEach((n) => {
          const row = entry.parsed.values.get(n.id())
          const vals = { ...(n.data('values') || {}) }
          vals[name] = row ? row[col] : null
          n.data('values', vals)
        })
      })
    })
  })
  applyValueColors()
  setStatus('frameStatus', [
    {
      level: added.length ? 'ok' : 'warn',
      text: added.length
        ? `Added ${plural(added.length, 'step')}: ${listSample(added)}.`
        : 'All numeric files are already steps.',
    },
  ])
  updateFrameControls()
  void nodeIds
}

/* ============================================================
   EXPORT TO OTHER TOOLS
   GraphML (yEd, Cytoscape, igraph, NetworkX), GEXF 1.3 with colors,
   sizes and positions (Gephi), SIF (Cytoscape) and Cytoscape JSON
   (.cyjs, Cytoscape desktop and Cytoscape.js).
   ============================================================ */
function xmlEsc(v) {
  return String(v).replace(
    /[&<>"']/g,
    (c) => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&apos;' })[c]
  )
}

function exportModel() {
  const nodes = exportNodes()
  const idSet = new Set(nodes.map((n) => n.id()))
  const edges = exportEdges().filter(
    (e) => idSet.has(e.data('source')) && idSet.has(e.data('target'))
  )
  const nodeAttrKeys = new Set(),
    edgeAttrKeys = new Set(),
    valueKeys = new Set()
  nodes.forEach((n) => {
    Object.keys(n.data('attrs') || {}).forEach((k) => nodeAttrKeys.add(k))
    Object.keys(n.data('values') || {}).forEach((k) => valueKeys.add(k))
  })
  edges.forEach((e) => Object.keys(e.data('attrs') || {}).forEach((k) => edgeAttrKeys.add(k)))
  const kind = (list, key, get) =>
    list.every((x) => {
      const v = get(x)
      return v === undefined || v === null || typeof v === 'number'
    })
      ? 'double'
      : 'string'
  return {
    nodes,
    edges,
    nodeAttrs: [...nodeAttrKeys].map((k) => ({
      key: k,
      type: kind(nodes, k, (n) => (n.data('attrs') || {})[k]),
    })),
    edgeAttrs: [...edgeAttrKeys].map((k) => ({
      key: k,
      type: kind(edges, k, (e) => (e.data('attrs') || {})[k]),
    })),
    valueKeys: [...valueKeys],
    weighted: edges.some((e) => typeof e.data('weight') === 'number'),
    anyDirected: edges.some((e) => edgeIsDirected(e)),
  }
}

function nodeGroupsText(n) {
  return effectiveGroupsFor(n)
    .filter((g) => g !== UNGROUPED)
    .map(groupLabel)
    .join(';')
}

function attrText(v) {
  return v === null || v === undefined ? '' : typeof v === 'object' ? JSON.stringify(v) : String(v)
}

function channelLabel(t) {
  return (EDGE_TYPES[t] || { label: t }).label
}

function toGraphML() {
  const m = exportModel()
  const keys = [
    ['n_label', 'node', 'label', 'string'],
    ['n_groups', 'node', 'groups', 'string'],
    ['n_color', 'node', 'color', 'string'],
    ['n_size', 'node', 'size', 'double'],
    ['n_x', 'node', 'x', 'double'],
    ['n_y', 'node', 'y', 'double'],
    ...m.valueKeys.map((k, i) => [`n_v${i}`, 'node', `value: ${k}`, 'double']),
    ...m.nodeAttrs.map((a, i) => [`n_a${i}`, 'node', a.key, a.type]),
    ['e_channel', 'edge', 'channel', 'string'],
    ['e_color', 'edge', 'color', 'string'],
    ...(m.weighted ? [['e_weight', 'edge', 'weight', 'double']] : []),
    ...m.edgeAttrs.map((a, i) => [`e_a${i}`, 'edge', a.key, a.type]),
  ]
  const lines = [
    '<?xml version="1.0" encoding="UTF-8"?>',
    '<graphml xmlns="http://graphml.graphdrawing.org/xmlns" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://graphml.graphdrawing.org/xmlns http://graphml.graphdrawing.org/xmlns/1.0/graphml.xsd">',
    '  <!-- exported by NORMA 3.0 -->',
    ...keys.map(
      ([id, forWhat, name, type]) =>
        `  <key id="${id}" for="${forWhat}" attr.name="${xmlEsc(name)}" attr.type="${type}"/>`
    ),
    `  <graph id="G" edgedefault="${m.anyDirected && m.edges.every((e) => edgeIsDirected(e)) ? 'directed' : 'undirected'}">`,
  ]
  const d = (k, v) =>
    v === '' || v === null || v === undefined || (typeof v === 'number' && !Number.isFinite(v))
      ? ''
      : `<data key="${k}">${xmlEsc(v)}</data>`
  m.nodes.forEach((n) => {
    const p = n.position()
    const vals = n.data('values') || {}
    const attrs = n.data('attrs') || {}
    lines.push(
      `    <node id="${xmlEsc(n.id())}">` +
        d('n_label', n.id()) +
        d('n_groups', nodeGroupsText(n)) +
        d('n_color', hexColor(n.style('background-color'))) +
        d('n_size', Math.round(n.width() * 100) / 100) +
        d('n_x', Math.round(p.x * 100) / 100) +
        d('n_y', Math.round(p.y * 100) / 100) +
        m.valueKeys.map((k, i) => d(`n_v${i}`, vals[k])).join('') +
        m.nodeAttrs.map((a, i) => d(`n_a${i}`, attrText(attrs[a.key]))).join('') +
        '</node>'
    )
  })
  m.edges.forEach((e, i) => {
    const attrs = e.data('attrs') || {}
    lines.push(
      `    <edge id="e${i}" source="${xmlEsc(e.data('source'))}" target="${xmlEsc(e.data('target'))}" directed="${edgeIsDirected(e)}">` +
        d('e_channel', channelLabel(e.data('type'))) +
        d('e_color', hexColor(e.style('line-color'))) +
        (m.weighted ? d('e_weight', e.data('weight')) : '') +
        m.edgeAttrs.map((a, k) => d(`e_a${k}`, attrText(attrs[a.key]))).join('') +
        '</edge>'
    )
  })
  lines.push('  </graph>', '</graphml>')
  return lines.join('\n') + '\n'
}

function toGEXF() {
  const m = exportModel()
  const nodeAttrDefs = [
    ['groups', 'string'],
    ...m.valueKeys.map((k) => [`value: ${k}`, 'double']),
    ...m.nodeAttrs.map((a) => [a.key, a.type]),
  ]
  const edgeAttrDefs = [['channel', 'string'], ...m.edgeAttrs.map((a) => [a.key, a.type])]
  const lines = [
    '<?xml version="1.0" encoding="UTF-8"?>',
    '<gexf xmlns="http://gexf.net/1.3" xmlns:viz="http://gexf.net/1.3/viz" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://gexf.net/1.3 http://gexf.net/1.3/gexf.xsd" version="1.3">',
    `  <meta><creator>NORMA 3.0</creator><description>${xmlEsc((activeView() || {}).name || 'NORMA network')}</description></meta>`,
    `  <graph defaultedgetype="${m.anyDirected ? 'directed' : 'undirected'}" mode="static">`,
    '    <attributes class="node">',
    ...nodeAttrDefs.map(
      ([k, t], i) => `      <attribute id="${i}" title="${xmlEsc(k)}" type="${t}"/>`
    ),
    '    </attributes>',
    '    <attributes class="edge">',
    ...edgeAttrDefs.map(
      ([k, t], i) => `      <attribute id="${i}" title="${xmlEsc(k)}" type="${t}"/>`
    ),
    '    </attributes>',
    '    <nodes>',
  ]
  const att = (i, v) =>
    v === '' || v === null || v === undefined || (typeof v === 'number' && !Number.isFinite(v))
      ? ''
      : `<attvalue for="${i}" value="${xmlEsc(v)}"/>`
  m.nodes.forEach((n) => {
    const [r, g, b] = rgbOf(n.style('background-color'))
    const p = n.position()
    const vals = n.data('values') || {},
      attrs = n.data('attrs') || {}
    const values = [
      nodeGroupsText(n),
      ...m.valueKeys.map((k) => vals[k]),
      ...m.nodeAttrs.map((a) => attrText(attrs[a.key])),
    ]
    lines.push(
      `      <node id="${xmlEsc(n.id())}" label="${xmlEsc(n.id())}"><attvalues>${values.map((v, i) => att(i, v)).join('')}</attvalues>` +
        `<viz:color r="${Math.round(r)}" g="${Math.round(g)}" b="${Math.round(b)}"/><viz:position x="${p.x.toFixed(2)}" y="${(-p.y).toFixed(2)}" z="0.0"/><viz:size value="${(n.width() / 2).toFixed(2)}"/></node>`
    )
  })
  lines.push('    </nodes>', '    <edges>')
  m.edges.forEach((e, i) => {
    const [r, g, b] = rgbOf(e.style('line-color'))
    const attrs = e.data('attrs') || {}
    const values = [channelLabel(e.data('type')), ...m.edgeAttrs.map((a) => attrText(attrs[a.key]))]
    const w = typeof e.data('weight') === 'number' ? ` weight="${e.data('weight')}"` : ''
    lines.push(
      `      <edge id="${i}" source="${xmlEsc(e.data('source'))}" target="${xmlEsc(e.data('target'))}" type="${edgeIsDirected(e) ? 'directed' : 'undirected'}" kind="${xmlEsc(channelLabel(e.data('type')))}" label="${xmlEsc(channelLabel(e.data('type')))}"${w}>` +
        `<attvalues>${values.map((v, k) => att(k, v)).join('')}</attvalues><viz:color r="${Math.round(r)}" g="${Math.round(g)}" b="${Math.round(b)}"/></edge>`
    )
  })
  lines.push('    </edges>', '  </graph>', '</gexf>')
  return lines.join('\n') + '\n'
}

function toSIF() {
  const m = exportModel()
  const lines = []
  const connected = new Set()
  const clean = (s) => String(s).replace(/[\t\r\n]+/g, ' ')
  m.edges.forEach((e) => {
    connected.add(e.data('source'))
    connected.add(e.data('target'))
    lines.push(
      [
        clean(e.data('source')),
        clean(channelLabel(e.data('type'))).replace(/\s+/g, '_') || 'pp',
        clean(e.data('target')),
      ].join('\t')
    )
  })
  m.nodes.forEach((n) => {
    if (!connected.has(n.id())) lines.push(clean(n.id()))
  })
  return lines.join('\n') + '\n'
}

function toCyJS() {
  const m = exportModel()
  const v = activeView()
  const json = {
    format_version: '1.0',
    generated_by: 'NORMA 3.0',
    target_cytoscapejs_version: '~3.0',
    data: {
      name: v ? v.name : 'NORMA network',
      shared_name: v ? v.name : 'NORMA network',
      selected: true,
    },
    elements: {
      nodes: m.nodes.map((n) => {
        const p = n.position()
        return {
          data: {
            id: n.id(),
            name: n.id(),
            shared_name: n.id(),
            groups: nodeGroupsText(n),
            color: hexColor(n.style('background-color')),
            size: Math.round(n.width() * 100) / 100,
            ...Object.fromEntries(
              Object.entries(n.data('values') || {}).map(([k, x]) => [`value: ${k}`, x])
            ),
            ...(n.data('attrs') || {}),
          },
          position: { x: Math.round(p.x * 100) / 100, y: Math.round(p.y * 100) / 100 },
        }
      }),
      edges: m.edges.map((e, i) => ({
        data: {
          id: `e${i}`,
          source: e.data('source'),
          target: e.data('target'),
          interaction: channelLabel(e.data('type')),
          shared_interaction: channelLabel(e.data('type')),
          name: `${e.data('source')} (${channelLabel(e.data('type'))}) ${e.data('target')}`,
          directed: edgeIsDirected(e),
          color: hexColor(e.style('line-color')),
          ...(typeof e.data('weight') === 'number' ? { weight: e.data('weight') } : {}),
          ...(e.data('attrs') || {}),
        },
      })),
    },
  }
  return JSON.stringify(json, null, 1)
}

function exportOtherFormat(kind) {
  if (!cy.nodes().length) {
    setStatus('otherExportStatus', [{ level: 'error', text: 'Show a network first.' }])
    return
  }
  const v = activeView()
  const stem = v ? fileStem(v.name) : 'network'
  const spec = {
    graphml: ['GraphML', 'graphml', toGraphML],
    gexf: ['GEXF', 'gexf', toGEXF],
    sif: ['SIF', 'sif', toSIF],
    cyjs: ['Cytoscape JSON', 'cyjs', toCyJS],
  }[kind]
  const text = spec[2]()
  downloadText(`${stem}.${spec[1]}`, text)
  const m = exportModel()
  setStatus('otherExportStatus', [
    {
      level: 'ok',
      text: `Saved ${stem}.${spec[1]} (${spec[0]}): ${plural(m.nodes.length, 'node')}, ${plural(m.edges.length, 'edge')}${exportShownOnly() ? ', ticked groups and channels only' : ''}.`,
    },
  ])
}

/* ============================================================
   SESSIONS
   One file with every view (data, positions, settings, 3D camera) and
   every file in Files (including STRING extras), to continue later.
   ============================================================ */
const SESSION_FORMAT = 'norma3-session'

function sessionObject() {
  captureActiveView()
  const lib = {}
  Object.keys(normaLibrary).forEach((kind) => {
    lib[kind] = normaLibrary[kind].map((e) => {
      const { parsed, ...rest } = e
      return rest
    })
  })
  return {
    format: SESSION_FORMAT,
    version: 1,
    savedAt: new Date().toISOString(),
    app: 'NORMA 3.0',
    activeViewId: S.activeViewId,
    theme: document.getElementById('themeSelect').value,
    views: views.map((v) => ({
      id: v.id,
      name: v.name,
      autoName: v.autoName,
      data: v.data,
      selection: v.selection,
      state: v.state,
      ...(v.needsRefresh ? { needsRefresh: v.needsRefresh } : {}),
    })),
    library: lib,
    edgeTypes: JSON.parse(JSON.stringify(EDGE_TYPES)),
  }
}

function saveSession() {
  const s = sessionObject()
  const stamp = new Date().toISOString().slice(0, 16).replace(/[:T]/g, '-')
  downloadText(`norma-session-${stamp}.json`, JSON.stringify(s))
  setStatus('sessionSaveStatus', [
    {
      level: 'ok',
      text: `Saved the session: ${plural(s.views.length, 'view')} and ${plural(
        Object.values(s.library).reduce((n, l) => n + l.length, 0),
        'file'
      )}.`,
    },
  ])
}

export function isSessionObject(obj) {
  return obj && obj.format === SESSION_FORMAT && Array.isArray(obj.views)
}

export function loadSession(obj) {
  if (!isSessionObject(obj)) throw new Error('This is not a NORMA session file.')
  stopFramePlay()
  // files
  const kinds = Object.keys(normaLibrary)
  const rebuilt = {}
  let maxSeq = S.normaEntrySeq
  kinds.forEach((kind) => {
    rebuilt[kind] = []
    ;((obj.library && obj.library[kind]) || []).forEach((saved) => {
      const parsed = NORMA_PARSERS[kind](saved.text, saved.options || {})
      rebuilt[kind].push({ ...saved, parsed })
      const n = parseInt(String(saved.id).replace(/\D/g, ''), 10)
      if (Number.isFinite(n)) maxSeq = Math.max(maxSeq, n)
    })
  })
  kinds.forEach((kind) => {
    normaLibrary[kind].length = 0
    normaLibrary[kind].push(...rebuilt[kind])
  })
  S.normaEntrySeq = maxSeq
  if (obj.edgeTypes)
    Object.entries(obj.edgeTypes).forEach(([k, t]) => {
      const safe = {
        ...t,
        color:
          sanitizeColor(t && t.color) ||
          (EDGE_TYPES[k] && EDGE_TYPES[k].color) ||
          nextAutoEdgeColor(),
      }
      if (!EDGE_TYPES[k]) EDGE_TYPES[k] = safe
      else Object.assign(EDGE_TYPES[k], safe)
    })
  // views
  views.length = 0
  let maxView = S.viewSeq
  obj.views.forEach((v) => {
    views.push({
      id: v.id,
      name: v.name,
      autoName: v.autoName,
      data: v.data,
      selection: v.selection || { networks: [], annotation: '', colors: '' },
      state: v.state || { config: { ...S.DEFAULT_VIEW_CONFIG } },
      needsRefresh: v.needsRefresh || null,
    })
    const n = parseInt(String(v.id).replace(/\D/g, ''), 10)
    if (Number.isFinite(n)) maxView = Math.max(maxView, n)
  })
  S.viewSeq = maxView
  if (!views.length) {
    S.activeViewId = null
    createView('Untitled view')
  } else {
    S.activeViewId = views.some((v) => v.id === obj.activeViewId) ? obj.activeViewId : views[0].id
    const v = activeView()
    libSelection.networks = new Set(v.selection.networks || [])
    libSelection.annotation = v.selection.annotation || ''
    libSelection.colors = v.selection.colors || ''
    S.historySuspended++
    try {
      restoreView(v)
    } finally {
      S.historySuspended--
    }
    S.dataCache = { version: dataVersion, data: v.data }
  }
  if (obj.theme) {
    document.getElementById('themeSelect').value = obj.theme
    applyTheme(obj.theme)
  }
  renderLibraryLists()
  setHistoryBaseline()
  renderViewBar()
  updateUndoButtons()
  return { views: views.length, files: kinds.reduce((n, k) => n + normaLibrary[k].length, 0) }
}

async function openSessionFile(file) {
  try {
    const obj = JSON.parse(await readFileText(file))
    const r = loadSession(obj)
    setStatus('sessionStatus', [
      {
        level: 'ok',
        text: `Opened the session "${file.name}": ${plural(r.views, 'view')} and ${plural(r.files, 'file')}.`,
      },
    ])
  } catch (err) {
    setStatus('sessionStatus', [
      { level: 'error', text: `The session couldn't be opened: ${err.message}` },
    ])
  }
}

/* ============================================================
   WEBGL DRAWING (experimental)
   Cytoscape.js can draw with WebGL, which is much faster on large
   networks. It is chosen before the network canvas is created, so the
   setting takes effect after reloading the page.
   ============================================================ */
export function webglPreference() {
  try {
    const q = new URLSearchParams(location.search).get('webgl')
    if (q !== null) return q === '1' || q === 'true'
    return localStorage.getItem('norma3-webgl') === '1'
  } catch (e) {
    return false
  }
}

/* ============================================================
   RUNTIME TABLE (Network Profiler)
   Times the main steps on random networks of growing size, for a
   supplementary table: reading the network file, building the network,
   weighted layout, edge bundling, profiling, Louvain and the group
   separation score. Nothing on screen changes.
   ============================================================ */
const RUNTIME_SIZES = [100, 500, 1000, 2500, 5000, 10000]

const runtimeState = { rows: [], running: false, cancel: false }

async function runRuntimeTable() {
  if (runtimeState.running) {
    runtimeState.cancel = true
    return
  }
  const btn = document.getElementById('btnRuntime')
  const sizes = RUNTIME_SIZES.filter(
    (n) => n <= (parseInt(document.getElementById('runtimeMax').value, 10) || 5000)
  )
  runtimeState.running = true
  runtimeState.cancel = false
  btn.textContent = 'Stop'
  runtimeState.rows = []
  const time = async (fn) => {
    const t = performance.now()
    const r = await fn()
    return [performance.now() - t, r]
  }
  try {
    // larger networks take much longer: weigh each size by n^1.5
    const weightOf = (n) => Math.pow(n, 1.5)
    const totalWeight = sizes.reduce((a, n) => a + weightOf(n), 0)
    let doneWeight = 0
    for (const n of sizes) {
      if (runtimeState.cancel) break
      const text = `Timing a network of ${n.toLocaleString()} nodes…`
      const show = (f) =>
        setStatus('runtimeStatus', [
          { level: 'busy', text, progress: (doneWeight + weightOf(n) * f) / totalWeight },
        ])
      show(0)
      await nextPaint()
      const demo = generateRandomNetwork(n, { seed: n + 3 })
      const texts = demoToNormaTexts(demo)
      const row = { nodes: n }
      const [tParse, parsed] = await time(() => parseNormaNetwork(texts.network, {}))
      row.edges = parsed.edges.length
      row.parse = tParse
      const [tBuild, headless] = await time(() =>
        cytoscape({
          headless: true,
          styleEnabled: false,
          elements: [
            ...demo.nodes.map((x) => ({ data: { id: x.id } })),
            ...demo.edges.map((e, i) => ({
              data: { id: 'r' + i, source: e.source, target: e.target },
            })),
          ],
        })
      )
      row.build = tBuild
      headless.destroy()
      const edges = demo.edges.map((e) => ({ source: e.source, target: e.target, weight: 1 }))
      const ids = demo.nodes.map((x) => x.id)
      const [tLayout, pos] = await time(() => frLayoutAsync(ids, edges, (f) => show(0.1 + 0.4 * f)))
      row.layout = tLayout
      if (runtimeState.cancel) break
      const segs = demo.edges.map((e) => ({
        sx: pos[e.source].x * 40,
        sy: pos[e.source].y * 40,
        tx: pos[e.target].x * 40,
        ty: pos[e.target].y * 40,
      }))
      if (segs.length <= BUNDLE_MAX_EDGES) {
        show(0.5)
        const [tBundle] = await time(() =>
          bundleAsync(segs, { threshold: 0.6, iterations: 60 }, (f) => show(0.5 + 0.4 * f))
        )
        row.bundle = tBundle
      } else row.bundle = NaN
      show(0.9)
      await nextPaint()
      const g = simpleGraph(
        ids,
        demo.edges.map((e) => [e.source, e.target])
      )
      const [tProfile] = await time(() => profileGraph(g))
      row.profile = tProfile
      const [tLouvain] = await time(() => louvain(g))
      row.louvain = tLouvain
      const groupsOf = Object.fromEntries(demo.nodes.map((x) => [x.id, x.groups || []]))
      const [tSep] = await time(() => groupSeparation(pos, groupsOf))
      row.separation = tSep
      runtimeState.rows.push(row)
      renderRuntimeTable()
      doneWeight += weightOf(n)
      await new Promise((r) => setTimeout(r, 30))
    }
    setStatus('runtimeStatus', [
      {
        level: runtimeState.cancel ? 'warn' : 'ok',
        text: `${runtimeState.cancel ? 'Stopped' : 'Done'}: timed ${plural(runtimeState.rows.length, 'network size')} in this browser (${navigator.hardwareConcurrency || '?'} logical processors). Rendering time depends on the screen and is not included.`,
      },
    ])
    document.getElementById('btnRuntimeTsv').disabled = !runtimeState.rows.length
  } finally {
    runtimeState.running = false
    btn.textContent = 'Time the main steps'
  }
}

const RUNTIME_COLUMNS = [
  ['parse', 'Read file'],
  ['build', 'Build network'],
  ['layout', 'Weighted layout'],
  ['bundle', 'Edge bundling'],
  ['profile', 'Profile statistics'],
  ['louvain', 'Louvain'],
  ['separation', 'Separation score'],
]

function renderRuntimeTable() {
  const root = document.getElementById('runtimeResults')
  const fmt = (v) =>
    !Number.isFinite(v) ? '—' : v < 1000 ? `${Math.round(v)} ms` : `${(v / 1000).toFixed(2)} s`
  root.innerHTML = `<div class="table-wrap"><table class="data">
    <thead><tr><th scope="col">Nodes</th><th scope="col">Edges</th>${RUNTIME_COLUMNS.map(([, l]) => `<th scope="col">${l}</th>`).join('')}</tr></thead>
    <tbody>${runtimeState.rows.map((r) => `<tr><td class="num">${r.nodes.toLocaleString()}</td><td class="num">${r.edges.toLocaleString()}</td>${RUNTIME_COLUMNS.map(([k]) => `<td class="num">${fmt(r[k])}</td>`).join('')}</tr>`).join('')}</tbody>
  </table></div>`
}

function runtimeTsv() {
  const lines = [
    ['Nodes', 'Edges', ...RUNTIME_COLUMNS.map(([, l]) => `${l} (ms)`)].join('\t'),
    ...runtimeState.rows.map((r) =>
      [
        r.nodes,
        r.edges,
        ...RUNTIME_COLUMNS.map(([k]) => (Number.isFinite(r[k]) ? r[k].toFixed(1) : '')),
      ].join('\t')
    ),
  ]
  lines.push(
    `# ${navigator.userAgent}; ${navigator.hardwareConcurrency || '?'} logical processors; WebGL drawing ${WEBGL_ACTIVE ? 'on' : 'off'}`
  )
  downloadText('norma-runtime.tsv', lines.join('\n') + '\n')
}

/* ---------- Network Comparison -> Arena3D ----------
   Each compared network is one layer holding its own nodes and edges.
   Every node keeps the same position in every layer (one layout of all
   networks together), so layers line up. Between layers, the common
   edges (and, if chosen, the copies of common nodes) link the layers:
   an edge present in networks A and B also runs from its source in
   layer A to its target in layer B. */
export async function buildCompareArena3dModel() {
  const nets = compareState.results
  if (!nets || nets.length < 2) throw new Error('Compare at least two networks first.')
  if (nets.length > ARENA3D_MAX_LAYERS)
    throw new Error(`Arena3D takes up to ${ARENA3D_MAX_LAYERS} layers.`)
  const between = document.getElementById('cmpArenaBetween').value
  const letter = (i) => String.fromCharCode(65 + i)
  const layerName = nets.map((n, i) => arenaSafe(`${letter(i)} ${n.name}`).slice(0, 60))
  // union network and one shared layout
  const nodeMask = new Map(),
    edgeMask = new Map()
  nets.forEach((net, i) => {
    net.ids.forEach((id) => nodeMask.set(id, (nodeMask.get(id) || 0) | (1 << i)))
    net.edgeSet.forEach((k) => edgeMask.set(k, (edgeMask.get(k) || 0) | (1 << i)))
  })
  const ids = [...nodeMask.keys()]
  if (ids.length > MAX_NETWORK_NODES)
    throw new Error(
      `The compared networks have ${ids.length.toLocaleString('en-US')} nodes together; NORMA handles up to ${MAX_NETWORK_NODES.toLocaleString('en-US')}.`
    )
  const unionEdges = [...edgeMask.keys()].map((k) => {
    const [a, b] = k.split('\t')
    return { source: a, target: b, weight: 1 }
  })
  const raw = ids.length > 1 ? await frLayoutAsync(ids, unionEdges) : { [ids[0]]: { x: 0, y: 0 } }
  const xs = ids.map((id) => raw[id].x),
    ys = ids.map((id) => raw[id].y)
  const cx = (Math.min(...xs) + Math.max(...xs)) / 2,
    cy0 = (Math.min(...ys) + Math.max(...ys)) / 2
  const span = Math.max(Math.max(...xs) - Math.min(...xs), Math.max(...ys) - Math.min(...ys)) || 1
  const half = ARENA3D_LAYER_WIDTH * 0.42
  const k = ids.length > 1 ? (2 * half) / span : 0
  const pos = (id) => ({ y: String(-(raw[id].y - cy0) * k), z: String((raw[id].x - cx) * k) })
  const full = (1 << nets.length) - 1
  const bits = (m) => nets.map((_, i) => i).filter((i) => m & (1 << i))
  const COMMON = '#9CA3AF'

  const layers = nets.map((net, i) => ({
    name: layerName[i],
    position_x: String((i - (nets.length - 1) / 2) * ARENA3D_LAYER_SPACING),
    position_y: '0',
    position_z: '0',
    last_layer_scale: '1',
    rotation_x: '0',
    rotation_y: '0',
    rotation_z: '0',
    floor_current_color: hexColor(net.color),
    geometry_parameters_width: String(ARENA3D_LAYER_WIDTH),
  }))
  const nodes = []
  nets.forEach((net, i) =>
    net.ids.forEach((id) => {
      const m = nodeMask.get(id)
      const p = pos(id)
      nodes.push({
        name: arenaSafe(id),
        layer: layerName[i],
        position_x: '0',
        position_y: p.y,
        position_z: p.z,
        scale: '1',
        color: bits(m).length === 1 ? hexColor(net.color) : COMMON,
        url: '',
        descr:
          bits(m).length === 1 ? `Only in ${net.name}` : `In ${bits(m).map(letter).join(', ')}`,
      })
    })
  )
  const edges = [],
    rows = []
  const add = (a, la, b, lb, color, channel, opacity = '1') => {
    edges.push({
      src: `${arenaSafe(a)}_${la}`,
      trg: `${arenaSafe(b)}_${lb}`,
      opacity,
      color,
      channel,
    })
    rows.push([arenaSafe(a), la, arenaSafe(b), lb, 1, channel])
  }
  let within = 0,
    across = 0,
    nodeLinks = 0,
    directed = false
  // edges inside each layer
  nets.forEach((net, i) =>
    net.edgeSet.forEach((key) => {
      const [a, b, dir] = key.split('\t')
      if (dir === 'directed') directed = true
      const m = edgeMask.get(key)
      const only = bits(m).length === 1
      add(
        a,
        layerName[i],
        b,
        layerName[i],
        only ? hexColor(net.color) : COMMON,
        only ? `${letter(i)} only` : 'common',
        '1'
      )
      within++
    })
  )
  // common edges between consecutive layers that contain them
  if (between === 'edges' || between === 'both') {
    edgeMask.forEach((m, key) => {
      const bs = bits(m)
      if (bs.length < 2) return
      const [a, b] = key.split('\t')
      for (let j = 0; j + 1 < bs.length; j++) {
        add(
          a,
          layerName[bs[j]],
          b,
          layerName[bs[j + 1]],
          '#FFFFFF',
          'common edge between layers',
          '0.7'
        )
        across++
      }
    })
  }
  // copies of common nodes
  if (between === 'nodes' || between === 'both') {
    nodeMask.forEach((m, id) => {
      const bs = bits(m)
      for (let j = 0; j + 1 < bs.length; j++) {
        add(id, layerName[bs[j]], id, layerName[bs[j + 1]], '#FACC15', 'same node', '0.5')
        nodeLinks++
      }
    })
  }
  const json = {
    scene: {
      position_x: '0',
      position_y: '0',
      scale: String(
        nets.length > 4 ? Math.round(((0.6561 * 4) / nets.length) * 1e4) / 1e4 : 0.6561
      ),
      color: '#000000',
      rotation_x: '0.261799387799149',
      rotation_y: '0.261799387799149',
      rotation_z: '0.0872664625997165',
    },
    layers,
    nodes,
    edges,
    universalLabelColor: '#FFFFFF',
    direction: directed,
    edgeOpacityByWeight: false,
    edgeWidthByWeight: false,
  }
  const commonEdges = [...edgeMask.values()].filter((m) => bits(m).length > 1).length
  const commonNodes = [...nodeMask.values()].filter((m) => bits(m).length > 1).length
  return {
    json,
    rows,
    stats: {
      layers: layers.length,
      nodes: nodes.length,
      edges: edges.length,
      interLayer: across + nodeLinks,
      copyEdges: nodeLinks,
      multiLayerNodes: commonNodes,
      distinctNodes: ids.length,
      droppedLayers: 0,
      droppedNodes: 0,
      compare: {
        within,
        across,
        nodeLinks,
        commonEdges,
        full: [...edgeMask.values()].filter((m) => m === full).length,
      },
    },
  }
}

/* ============================================================
   DATABASE IMPORTERS
   Reactome, OmniPath, NDEx, IntAct and the Gene Ontology, next to
   STRING. Each one builds a network and/or groupings and adds them to
   Files like any uploaded file. Requests go through server.py's relay
   when NORMA runs on it (no cross-site restrictions), otherwise straight
   to the service. Bodies are always strings (see the STRING importer).
   ============================================================ */
const DB_URLS = {
  reactome: 'https://reactome.org/ContentService',
  omnipath: 'https://omnipathdb.org',
  ndex: 'https://www.ndexbio.org',
  intact: 'https://www.ebi.ac.uk/Tools/webservices/psicquic/intact/webservices/current/search',
  quickgo: 'https://www.ebi.ac.uk/QuickGO/services',
  goapi: 'https://api.geneontology.org/api',
}

const DB_LABELS = {
  reactome: 'Reactome',
  omnipath: 'OmniPath',
  ndex: 'NDEx',
  intact: 'IntAct',
  go: 'Gene Ontology',
}

const dbState = { busy: {}, abort: {}, task: {}, mapping: {} }

async function dbFetch(
  key,
  url,
  {
    method = 'GET',
    body = null,
    contentType = null,
    accept = 'application/json',
    text = false,
  } = {}
) {
  const route =
    NORMA_CFG.features.relays.databases === false ? 'direct' : await resolveStringRoute()
  const target = route === 'proxy' ? `db-api/fetch?url=${encodeURIComponent(url)}` : url
  const headers = { Accept: accept }
  if (contentType) headers['Content-Type'] = contentType
  const ctl = new AbortController()
  dbState.abort[key] = ctl
  const timer = setTimeout(() => ctl.abort(), 120000)
  let response
  try {
    response = await fetch(target, { method, headers, body, signal: ctl.signal })
  } catch (err) {
    if (err.name === 'AbortError')
      throw new Error(
        dbState.cancelled && dbState.cancelled[key]
          ? 'Cancelled.'
          : `${DB_LABELS[key]} did not answer within 2 minutes.`
      )
    throw new Error(
      route === 'proxy'
        ? `The server relay could not be reached (${err.message}). Check that server.py is still running.`
        : `${DB_LABELS[key]} could not be reached (${err.message}). Browsers may block direct calls to other sites; running NORMA with server.py avoids that (see Help).`
    )
  } finally {
    clearTimeout(timer)
  }
  // download progress, except for the many small requests of dbMap (counted there)
  const task = dbState.task[key]
  const body2 = await readTextWithProgress(
    response,
    task && !dbState.mapping[key] ? (got, total) => task.bytes(got, total) : null
  )
  if (!response.ok) {
    if (route === 'proxy' && !response.headers.get('X-Norma-Relay'))
      throw new Error(
        'This server has no database relay. Start NORMA with server.py, or choose Connect: Directly under Import from STRING → STRING server.'
      )
    let detail = body2
      .slice(0, 200)
      .replace(/<[^>]+>/g, ' ')
      .replace(/\s+/g, ' ')
      .trim()
    try {
      const j = JSON.parse(body2)
      detail = j.message || j.errorMessage || j.error || detail
    } catch (e) {}
    throw new Error(
      `${DB_LABELS[key]} answered with an error (${response.status})${detail ? ': ' + detail : ''}.`
    )
  }
  if (text) return body2
  try {
    return body2.trim() ? JSON.parse(body2) : null
  } catch (e) {
    throw new Error(`${DB_LABELS[key]} sent an answer that is not JSON.`)
  }
}

// runs fn over items with a few requests at a time
async function dbMap(key, items, limit, fn, progress) {
  const out = new Array(items.length)
  let next = 0,
    done = 0
  dbState.mapping[key] = (dbState.mapping[key] || 0) + 1
  const worker = async () => {
    while (next < items.length) {
      if (dbState.cancelled[key]) throw new Error('Cancelled.')
      const i = next++
      try {
        out[i] = await fn(items[i], i)
      } catch (err) {
        if (/Cancelled/.test(err.message)) throw err
        out[i] = { error: err }
      }
      done++
      if (progress) progress(done, items.length)
      if (dbState.task[key]) dbState.task[key].sub(done / items.length)
    }
  }
  try {
    await Promise.all(Array.from({ length: Math.min(limit, items.length) }, worker))
  } finally {
    dbState.mapping[key]--
  }
  return out
}

function dbStatus(key, notes) {
  setStatus(`${key}Status`, notes)
}

// Progress of an import: dbPlan sets its weighted steps, dbStep moves on,
// dbProgress changes the text of the current step.
function dbPlan(key, weights) {
  dbState.task[key] = startProgress(`${key}Status`, weights)
  return dbState.task[key]
}

function dbStep(key, i, text) {
  if (!dbState.task[key] || !dbState.task[key].active) dbPlan(key, [1])
  dbState.task[key].step(i, text)
}

function dbProgress(key, text) {
  const task = dbState.task[key]
  if (task && task.active) task.say(text)
  else dbStep(key, 0, text)
}

function dbBusy(key, busy) {
  dbState.busy[key] = busy
  if (busy) dbState.cancelled[key] = false
  document.querySelectorAll(`[data-db-run="${key}"]`).forEach((b) => {
    b.disabled = busy
  })
  const cancel = document.getElementById(`${key}Cancel`)
  if (cancel) cancel.hidden = !busy
}

async function dbRun(key, fn) {
  if (dbState.busy[key]) return
  dbBusy(key, true)
  dbState.mapping[key] = 0
  dbStep(key, 0, `Contacting ${DB_LABELS[key]}…`)
  try {
    await fn()
  } catch (err) {
    dbStatus(key, [{ level: 'error', text: err.message }])
  } finally {
    if (dbState.task[key]) dbState.task[key].stop()
    dbState.task[key] = null
    dbBusy(key, false)
  }
}

export function dbCancel(key) {
  dbState.cancelled[key] = true
  if (dbState.abort[key]) dbState.abort[key].abort()
}

const dbSafe = (s) =>
  String(s ?? '')
    .replace(/[\t\r\n]+/g, ' ')
    .replace(/,/g, ';')
    .trim()

function dbSplitList(v) {
  return String(v || '')
    .split(/[\s,;]+/)
    .map((x) => x.trim())
    .filter(Boolean)
}

const UNIPROT_RE = /^([OPQ][0-9][A-Z0-9]{3}[0-9]|[A-NR-Z][0-9]([A-Z][A-Z0-9]{2}[0-9]){1,2})(-\d+)?$/

/* Adds a fetched network and its groupings to Files and opens them.
   edges: [{ source, target, type, weight, directed }] with node names
   nodeAttrs: { name: {...} }, groupings: [{ label, groups: [{ name, members, meta }] }] */
function dbAddImport(
  key,
  {
    name,
    edges,
    nodeAttrs = {},
    groupings = [],
    positions = null,
    notes = [],
    summary = '',
    preferGrouping = 0,
  }
) {
  if (!edges.length) throw new Error('No connections were found with these settings.')
  const weighted = edges.some((e) => typeof e.weight === 'number')
  const typed =
    new Set(edges.map((e) => e.type || '')).size > 1 ||
    edges.some((e) => e.type && e.type !== 'link')
  const anyDirected = edges.some((e) => e.directed)
  const header = [
    'Source',
    'Target',
    ...(weighted ? ['Weight'] : []),
    ...(typed ? ['Type'] : []),
    ...(anyDirected ? ['Direction'] : []),
  ]
  const lines = edges.map((e) =>
    [
      dbSafe(e.source),
      dbSafe(e.target),
      ...(weighted ? [typeof e.weight === 'number' ? Math.round(e.weight * 1e4) / 1e4 : 1] : []),
      ...(typed ? [dbSafe(e.type || 'link')] : []),
      ...(anyDirected ? [e.directed ? 'directed' : 'undirected'] : []),
    ].join('\t')
  )
  const netEntry = addNormaEntry(
    'network',
    name,
    [header.join('\t'), ...lines].join('\n') + '\n',
    '',
    null,
    {}
  )
  const cleanAttrs = {}
  Object.entries(nodeAttrs).forEach(([k, v]) => {
    cleanAttrs[dbSafe(k)] = v
  })
  netEntry.nodeAttrs = cleanAttrs
  netEntry.dbSource = key
  if (positions) {
    const pos = {}
    Object.entries(positions).forEach(([k, p]) => {
      pos[dbSafe(k)] = p
    })
    netEntry.nodePositions = pos
  }
  const nodeSet = new Set(netEntry.parsed.nodes)
  const annEntries = []
  groupings.forEach((gr) => {
    const used = new Set()
    const meta = {}
    const rows = []
    gr.groups.forEach((g) => {
      const members = [...new Set(g.members.map(dbSafe))].filter((m) => nodeSet.has(m))
      if (members.length < (gr.minSize || 1)) return
      let gname =
        String(g.name)
          .replace(/[\t\r\n]+/g, ' ')
          .trim() || 'group'
      while (used.has(gname)) gname += '′'
      used.add(gname)
      if (g.meta) meta[gname] = g.meta
      rows.push(`${gname}\t${members.join(',')}`)
    })
    if (!rows.length) return
    const entry = addNormaEntry(
      'annotation',
      `${name}: ${gr.label}`,
      rows.join('\n') + '\n',
      '',
      null,
      {}
    )
    entry.groupMeta = meta
    entry.forNetwork = netEntry.id
    annEntries.push(entry)
  })
  const chosen = annEntries[Math.min(preferGrouping, annEntries.length - 1)]
  openInNewView(name, () => {
    libSelection.networks = new Set([netEntry.id])
    libSelection.annotation = chosen ? chosen.id : ''
    libSelection.colors = ''
    renderLibraryLists()
    refreshLibraryView()
    if (anyDirected) {
      // directed databases (OmniPath, GO-CAM) open with arrows
      const dirSel = document.getElementById('edgeDirection')
      dirSel.value = 'data'
      dirSel.dispatchEvent(new Event('change', { bubbles: true }))
    }
  })
  switchTab('network')
  dbStatus(key, [
    {
      level: 'ok',
      text: `Imported "${name}": ${netEntry.parsed.summary}${summary ? `. ${summary}` : ''}.`,
    },
    ...(annEntries.length
      ? [
          {
            level: 'ok',
            text: `Added ${plural(annEntries.length, 'grouping')}: ${annEntries.map((e) => e.name.slice(name.length + 2)).join(', ')}. Switch between them with the Grouping list at the top.`,
          },
        ]
      : []),
    ...netEntry.parsed.notes.map((t) => ({ level: 'warn', text: t })),
    ...notes,
  ])
  return { netEntry, annEntries }
}

// Adds groupings for the network already shown (GO terms, ...).
function dbAddGroupingsToView(key, groupings, label) {
  const entries = []
  groupings.forEach((gr) => {
    const rows = [],
      meta = {},
      used = new Set()
    gr.groups.forEach((g) => {
      let gname = String(g.name)
        .replace(/[\t\r\n]+/g, ' ')
        .trim()
      while (used.has(gname)) gname += '′'
      used.add(gname)
      if (g.meta) meta[gname] = g.meta
      rows.push(`${gname}\t${g.members.join(',')}`)
    })
    if (!rows.length) return
    const entry = addNormaEntry(
      'annotation',
      `${label}: ${gr.label}`,
      rows.join('\n') + '\n',
      '',
      null,
      {}
    )
    entry.groupMeta = meta
    entries.push(entry)
  })
  if (!entries.length) return entries
  if (S.currentLibView) {
    libSelection.annotation = entries[0].id
    renderLibraryLists()
    refreshLibraryView()
  } else {
    // views from examples or JSON: apply the first grouping directly
    const groupsOf = {}
    entries[0].parsed.groups.forEach((g) =>
      g.members.forEach((m) => (groupsOf[m] = groupsOf[m] || []).push(g.name))
    )
    document.getElementById('nodeFillSelect').value = 'groups'
    applyGroupsAndColorsInPlace(
      groupsOf,
      new Map(),
      entries[0].parsed.groups.map((g) => g.name),
      entries[0].groupMeta
    )
    renderLibraryLists()
  }
  return entries
}

/* ---------------------------------------------------------------
   REACTOME: the proteins of a pathway's reactions, linked when they
   take part in the same reaction; groups are its sub-pathways.
   --------------------------------------------------------------- */
export const REACTOME_SPECIES = [
  'Homo sapiens',
  'Mus musculus',
  'Rattus norvegicus',
  'Danio rerio',
  'Drosophila melanogaster',
  'Caenorhabditis elegans',
  'Saccharomyces cerevisiae',
  'Gallus gallus',
  'Sus scrofa',
  'Bos taurus',
  'Canis familiaris',
  'Xenopus tropicalis',
  'Dictyostelium discoideum',
  'Plasmodium falciparum',
  'Schizosaccharomyces pombe',
]

const REACTION_CLASSES = new Set([
  'Reaction',
  'BlackBoxEvent',
  'Polymerisation',
  'Depolymerisation',
  'FailedReaction',
  'ReactionLikeEvent',
  'CellLineagePath',
  'CellDevelopmentStep',
])

const stripTags = (s) => String(s || '').replace(/<[^>]+>/g, '')

export async function reactomeSearch() {
  await dbRun('reactome', async () => {
    const q = document.getElementById('reactomeQuery').value.trim()
    const species = document.getElementById('reactomeSpecies').value
    if (!q) throw new Error('Type a pathway name or a Reactome identifier such as R-HSA-69278.')
    const sel = document.getElementById('reactomePathway')
    sel.innerHTML = ''
    if (/^R-[A-Z]{3}-\d+(\.\d+)?$/i.test(q)) {
      const p = await dbFetch('reactome', `${DB_URLS.reactome}/data/query/${encodeURIComponent(q)}`)
      sel.add(new Option(`${p.displayName} (${p.stId || q})`, p.stId || q))
    } else {
      dbProgress('reactome', 'Searching Reactome…')
      const r = await dbFetch(
        'reactome',
        `${DB_URLS.reactome}/search/query?query=${encodeURIComponent(q)}&species=${encodeURIComponent(species)}&types=Pathway&cluster=true`
      )
      const entries = ((r && r.results) || []).flatMap((g) => g.entries || [])
      sortedByName(entries.slice(0, 40), (e) => stripTags(e.name)).forEach((e) =>
        sel.add(new Option(`${stripTags(e.name)} (${e.stId || e.id})`, e.stId || e.id))
      )
    }
    document.getElementById('reactomePickRow').hidden = !sel.options.length
    dbStatus(
      'reactome',
      sel.options.length
        ? [
            {
              level: 'ok',
              text: `Found ${plural(sel.options.length, 'pathway')}. Choose one and fetch its network.`,
            },
          ]
        : [{ level: 'warn', text: 'No pathway matched. Try another name or check the species.' }]
    )
  })
}

export async function reactomeFetch() {
  await dbRun('reactome', async () => {
    const pid = document.getElementById('reactomePathway').value
    if (!pid) throw new Error('Search for a pathway and choose one first.')
    const maxReactions = Math.max(
      5,
      Math.min(500, parseInt(document.getElementById('reactomeMaxReactions').value, 10) || 150)
    )
    const maxSize = Math.max(
      2,
      parseInt(document.getElementById('reactomeMaxSize').value, 10) || 25
    )
    const smallMolecules = document.getElementById('reactomeSmall').checked
    // steps: pathway, sub-pathways, reactions, building
    dbPlan('reactome', [2, 1, 6, 1])
    dbStep('reactome', 0, 'Reading the pathway…')
    const top = await dbFetch(
      'reactome',
      `${DB_URLS.reactome}/data/query/${encodeURIComponent(pid)}`
    )
    const contained =
      (await dbFetch(
        'reactome',
        `${DB_URLS.reactome}/data/pathway/${encodeURIComponent(pid)}/containedEvents`
      )) || []
    const byDbId = new Map(
      contained.filter((e) => e && typeof e === 'object').map((e) => [e.dbId, e])
    )
    const resolve = (x) => (typeof x === 'object' ? x : byDbId.get(x))
    const reactions = [
      ...new Map(
        contained.filter((e) => e && REACTION_CLASSES.has(e.schemaClass)).map((e) => [e.stId, e])
      ).values(),
    ]
    const notes = []
    let used = reactions
    if (reactions.length > maxReactions) {
      used = reactions.slice(0, maxReactions)
      notes.push({
        level: 'warn',
        text: `The pathway has ${reactions.length} reactions; the first ${maxReactions} were read (raise "Most reactions" for more).`,
      })
    }
    if (!used.length) throw new Error('This pathway has no reactions to read.')
    // sub-pathways and the reactions each contains
    const children = (top.hasEvent || [])
      .map(resolve)
      .filter((e) => e && e.schemaClass === 'Pathway')
      .slice(0, 40)
    dbStep('reactome', 1, `Reading ${plural(children.length, 'sub-pathway')}…`)
    const childReactions = await dbMap('reactome', children, 4, async (ch) => {
      const evs =
        (await dbFetch(
          'reactome',
          `${DB_URLS.reactome}/data/pathway/${encodeURIComponent(ch.stId)}/containedEvents`
        )) || []
      return new Set(evs.filter((e) => e && typeof e === 'object').map((e) => e.stId))
    })
    // participants of each reaction
    dbStep('reactome', 2, `Reading reactions: 0 of ${used.length}…`)
    const parts = await dbMap(
      'reactome',
      used,
      4,
      (r) =>
        dbFetch('reactome', `${DB_URLS.reactome}/data/participants/${encodeURIComponent(r.stId)}`),
      (d, n) => dbProgress('reactome', `Reading reactions: ${d} of ${n}…`)
    )
    const nodeAttrs = {}
    const nameOf = new Map() // reference identifier -> node name
    const usedNames = new Map()
    const reactionMembers = new Map()
    let skippedBig = 0,
      failed = 0
    parts.forEach((p, i) => {
      if (!p || p.error) {
        failed++
        return
      }
      const members = new Set()
      ;(Array.isArray(p) ? p : []).forEach((part) =>
        (part.refEntities || []).forEach((ref) => {
          const cls = ref.schemaClass || ''
          const isProtein =
            /ReferenceGeneProduct|ReferenceIsoform|ReferenceDNASequence|ReferenceRNASequence/.test(
              cls
            ) || UNIPROT_RE.test(ref.identifier || '')
          if (!isProtein && !(smallMolecules && /ReferenceMolecule/.test(cls))) return
          const id = String(ref.identifier || ref.dbId)
          if (!nameOf.has(id)) {
            const disp = String(ref.displayName || '')
            let label = disp.includes(' ')
              ? disp.split(/\s+/).slice(1).join(' ')
              : (Array.isArray(ref.name) ? ref.name[0] : ref.name) || id
            label = label || id
            if (usedNames.has(label) && usedNames.get(label) !== id) label = `${label} (${id})`
            usedNames.set(label, id)
            nameOf.set(id, label)
            nodeAttrs[label] = {
              ...(isProtein ? { uniprot: id } : { chebi: id }),
              reactome: `https://reactome.org/content/query?q=${encodeURIComponent(id)}`,
            }
          }
          members.add(nameOf.get(id))
        })
      )
      if (members.size > maxSize) {
        skippedBig++
        return
      }
      reactionMembers.set(used[i].stId, { name: used[i].displayName, members: [...members] })
    })
    if (failed)
      notes.push({ level: 'warn', text: `${plural(failed, 'reaction')} could not be read.` })
    if (skippedBig)
      notes.push({
        level: 'ok',
        text: `Left out ${plural(skippedBig, 'reaction')} with more than ${maxSize} molecules, which would link them all to each other.`,
      })
    // edges: molecules sharing a reaction, weighted by how many they share
    const pairs = new Map()
    reactionMembers.forEach(({ members }) => {
      for (let a = 0; a < members.length; a++)
        for (let b = a + 1; b < members.length; b++) {
          const [x, y] =
            members[a] < members[b] ? [members[a], members[b]] : [members[b], members[a]]
          const k = x + '\t' + y
          pairs.set(k, (pairs.get(k) || 0) + 1)
        }
    })
    const edges = [...pairs].map(([k, w]) => {
      const [s, t] = k.split('\t')
      return { source: s, target: t, weight: w, type: 'same reaction' }
    })
    const groupings = []
    if (document.getElementById('reactomeGroupSub').checked && children.length) {
      const groups = children.map((ch, i) => {
        const set = childReactions[i] && !childReactions[i].error ? childReactions[i] : new Set()
        const members = new Set()
        reactionMembers.forEach((r, rid) => {
          if (set.has(rid)) r.members.forEach((m) => members.add(m))
        })
        return {
          name: stripTags(ch.displayName),
          members: [...members],
          meta: { description: `Reactome ${ch.stId}`, stId: ch.stId },
        }
      })
      groupings.push({ label: 'sub-pathways', groups })
    }
    if (document.getElementById('reactomeGroupReactions').checked) {
      const groups = [...reactionMembers]
        .filter(([, r]) => r.members.length >= 2)
        .sort((a, b) => b[1].members.length - a[1].members.length)
        .slice(0, 40)
        .map(([rid, r]) => ({
          name: stripTags(r.name),
          members: r.members,
          meta: { description: `Reactome reaction ${rid}`, stId: rid },
        }))
      groupings.push({ label: 'reactions', groups })
    }
    dbStep('reactome', 3, 'Building and opening the network…')
    await nextPaint()
    dbAddImport('reactome', {
      name: `Reactome ${stripTags(top.displayName)} (${pid})`,
      edges,
      nodeAttrs,
      groupings,
      notes,
      summary: `from ${plural(reactionMembers.size, 'reaction')}; edges join molecules of the same reaction, weighted by how many they share`,
    })
  })
}

/* ---------------------------------------------------------------
   OMNIPATH: signalling, TF-target and ligand-receptor interactions
   of the given proteins; groups from complexes, intercellular roles
   or pathway annotations.
   --------------------------------------------------------------- */
export async function omnipathFetch() {
  await dbRun('omnipath', async () => {
    // OmniPath's API matches gene symbols and UniProt accessions case-sensitively
    // (and both are conventionally uppercase), so normalize the query first --
    // otherwise "egfr" silently finds nothing where "EGFR" would.
    const names = dbSplitList(document.getElementById('omnipathQuery').value).map((n) =>
      n.toUpperCase()
    )
    if (!names.length)
      throw new Error('Type one or more gene symbols or UniProt accessions, for example EGFR.')
    const organism = document.getElementById('omnipathOrganism').value
    const datasets = [...document.querySelectorAll('#omnipathDatasets input:checked')].map(
      (i) => i.value
    )
    if (!datasets.length) throw new Error('Tick at least one dataset.')
    const among = document.getElementById('omnipathAmong').checked
    const maxPartners = Math.max(0, parseInt(document.getElementById('omnipathMax').value, 10) || 0)
    const channels = document.getElementById('omnipathChannels').value
    const wantedGroups = [...document.querySelectorAll('#omnipathGroups input:checked')].map(
      (i) => i.value
    )
    const annResource = document.getElementById('omnipathAnnotation').value.trim()
    // steps: interactions, complexes, intercellular roles, annotations, building
    const omniSteps = [
      6,
      wantedGroups.includes('complexes') ? 2 : 0,
      wantedGroups.includes('intercell') ? 2 : 0,
      wantedGroups.includes('annotations') && annResource ? 2 : 0,
      1,
    ]
    dbPlan('omnipath', omniSteps)
    dbStep('omnipath', 0, 'Fetching interactions from OmniPath…')
    const url = `${DB_URLS.omnipath}/interactions?partners=${encodeURIComponent(names.join(','))}&genesymbols=yes&organisms=${organism}&datasets=${datasets.join(',')}&fields=sources,references,curation_effort,type${among ? '&source_target=AND' : ''}&format=json`
    const rows = (await dbFetch('omnipath', url)) || []
    if (!Array.isArray(rows) || !rows.length)
      throw new Error('OmniPath has no interactions for these proteins with the ticked datasets.')
    const query = new Set(names.map((n) => n.toUpperCase()))
    const nodeAttrs = {}
    const nodeOf = (id, sym) => {
      const name = sym || id
      if (!nodeAttrs[name])
        nodeAttrs[name] = {
          ...(UNIPROT_RE.test(id) ? { uniprot: id } : { omnipath_id: id }),
          query: query.has(String(name).toUpperCase()) || query.has(String(id).toUpperCase()),
        }
      return name
    }
    let edges = rows
      .map((r) => {
        const s = nodeOf(r.source, r.source_genesymbol),
          t = nodeOf(r.target, r.target_genesymbol)
        const stim = +r.is_stimulation === 1 || r.is_stimulation === true,
          inh = +r.is_inhibition === 1 || r.is_inhibition === true
        const type =
          channels === 'sign'
            ? stim && inh
              ? 'stimulation and inhibition'
              : stim
                ? 'stimulation'
                : inh
                  ? 'inhibition'
                  : 'unsigned'
            : channels === 'type'
              ? String(r.type || 'interaction').replace(/_/g, ' ')
              : 'interaction'
        const sources = Array.isArray(r.sources)
          ? r.sources
          : String(r.sources || '')
              .split(';')
              .filter(Boolean)
        return {
          source: s,
          target: t,
          directed: +r.is_directed === 1 || r.is_directed === true,
          type,
          weight: Number.isFinite(+r.curation_effort) ? +r.curation_effort : sources.length || 1,
        }
      })
      .filter((e) => e.source !== e.target)
    const notes = []
    // keep the best-supported partners
    if (!among && maxPartners > 0) {
      const support = new Map()
      edges.forEach((e) =>
        [e.source, e.target].forEach((n) => {
          if (!nodeAttrs[n].query) support.set(n, (support.get(n) || 0) + e.weight)
        })
      )
      if (support.size > maxPartners) {
        const keep = new Set(
          [...support]
            .sort((a, b) => b[1] - a[1])
            .slice(0, maxPartners)
            .map((x) => x[0])
        )
        const before = support.size
        edges = edges.filter((e) =>
          [e.source, e.target].every((n) => nodeAttrs[n].query || keep.has(n))
        )
        notes.push({
          level: 'ok',
          text: `Kept the ${maxPartners} best-supported of ${before} partners (by curation effort).`,
        })
      }
    }
    // one edge per pair, channel and direction
    const merged = new Map()
    edges.forEach((e) => {
      const k = [
        e.directed ? e.source : [e.source, e.target].sort().join('\u0000'),
        e.directed ? e.target : '',
        e.type,
        e.directed,
      ].join('\t')
      const m = merged.get(k)
      if (m) m.weight = Math.max(m.weight, e.weight)
      else merged.set(k, { ...e })
    })
    edges = [...merged.values()]
    const present = new Set(edges.flatMap((e) => [e.source, e.target]))
    const uniprots = [...present].map((n) => nodeAttrs[n].uniprot).filter(Boolean)
    const symOf = new Map(
      [...present].filter((n) => nodeAttrs[n].uniprot).map((n) => [nodeAttrs[n].uniprot, n])
    )
    const groupings = []
    const wanted = [...document.querySelectorAll('#omnipathGroups input:checked')].map(
      (i) => i.value
    )
    const batches = []
    for (let i = 0; i < uniprots.length; i += 150) batches.push(uniprots.slice(i, i + 150))
    const fetchAll = async (path) =>
      (
        await dbMap('omnipath', batches, 2, (b) =>
          dbFetch(
            'omnipath',
            `${DB_URLS.omnipath}/${path}${path.includes('?') ? '&' : '?'}proteins=${b.join(',')}&format=json`
          )
        )
      ).flatMap((x) => (Array.isArray(x) ? x : []))
    if (wanted.includes('complexes') && uniprots.length) {
      dbStep('omnipath', 1, 'Fetching complexes…')
      const cx = await fetchAll('complexes')
      const groups = new Map()
      cx.forEach((c) => {
        const comps = Array.isArray(c.components)
          ? c.components
          : String(c.components || '').split('_')
        const members = comps.map((u) => symOf.get(u)).filter(Boolean)
        if (members.length < 2) return
        const name =
          c.name || String(c.components_genesymbols || '').replace(/_/g, ':') || comps.join(':')
        if (!groups.has(name))
          groups.set(name, {
            name,
            members,
            meta: {
              description: `Complex; sources: ${Array.isArray(c.sources) ? c.sources.join(', ') : c.sources || ''}`,
            },
          })
      })
      groupings.push({
        label: 'complexes',
        minSize: 2,
        groups: [...groups.values()]
          .sort((a, b) => b.members.length - a.members.length)
          .slice(0, 60),
      })
    }
    if (wanted.includes('intercell') && uniprots.length) {
      dbStep('omnipath', 2, 'Fetching intercellular roles…')
      const ic = await fetchAll('intercell?scope=generic')
      const groups = new Map()
      ic.forEach((r) => {
        const name = String(r.parent || r.category || '').replace(/_/g, ' ')
        const sym = symOf.get(r.uniprot)
        if (!name || !sym) return
        if (!groups.has(name)) groups.set(name, { name, members: [] })
        groups.get(name).members.push(sym)
      })
      groupings.push({ label: 'intercellular roles', groups: [...groups.values()] })
    }
    const resource = document.getElementById('omnipathAnnotation').value.trim()
    if (wanted.includes('annotations') && resource && uniprots.length) {
      dbStep('omnipath', 3, `Fetching ${resource} annotations…`)
      const an = await fetchAll(`annotations?resources=${encodeURIComponent(resource)}`)
      const LABELS = [
        'pathway',
        'location',
        'state',
        'function',
        'category',
        'mainclass',
        'classification',
        'family',
        'disease',
      ]
      const groups = new Map()
      an.forEach((r) => {
        const sym = symOf.get(r.uniprot)
        if (!sym || r.value === undefined || r.value === null || r.value === '') return
        if (!LABELS.includes(String(r.label).toLowerCase())) return
        const name = String(r.value)
        if (!groups.has(name))
          groups.set(name, {
            name,
            members: new Set(),
            meta: { description: `${resource}: ${r.label}` },
          })
        groups.get(name).members.add(sym)
      })
      if (!groups.size)
        notes.push({
          level: 'warn',
          text: `${resource} has no pathway-like annotations for these proteins.`,
        })
      groupings.push({
        label: resource,
        groups: [...groups.values()].map((g) => ({ ...g, members: [...g.members] })),
      })
    }
    dbStep('omnipath', omniSteps.length - 1, 'Building and opening the network…')
    await nextPaint()
    dbAddImport('omnipath', {
      name: `OmniPath ${names.slice(0, 3).join(', ')}${names.length > 3 ? ` +${names.length - 3}` : ''}`,
      edges,
      nodeAttrs,
      groupings,
      notes,
      summary: `datasets ${datasets.join(', ')}; channels by ${channels === 'sign' ? 'effect sign' : channels === 'type' ? 'interaction type' : 'nothing'}; weights are curation effort`,
    })
  })
}

/* ---------------------------------------------------------------
   NDEx: public networks by search or UUID, in CX2; node attributes
   become groupings and NDEx positions are kept.
   --------------------------------------------------------------- */
export async function ndexSearch() {
  await dbRun('ndex', async () => {
    const q = document.getElementById('ndexQuery').value.trim()
    if (!q) throw new Error('Type search words (for example "TP53 signaling") or a network UUID.')
    const sel = document.getElementById('ndexNetwork')
    sel.innerHTML = ''
    if (/^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i.test(q)) {
      sel.add(new Option(`Network ${q}`, q))
    } else {
      dbProgress('ndex', 'Searching NDEx…')
      const r = await dbFetch('ndex', `${DB_URLS.ndex}/v2/search/network?start=0&size=40`, {
        method: 'POST',
        body: JSON.stringify({ searchString: q }),
        contentType: 'application/json',
      })
      // Some sources (e.g. WikiPathways) name networks "WP4284 - Cell Cycle":
      // an id, then " - ", then the actual title. Shown and sorted by that
      // title instead, with the id moved after it ("Cell Cycle (WP4284)"),
      // matching the Reactome/GO-CAM "title (id)" pickers. Names with no
      // such prefix (no digit in the leading token) are left as they are.
      const networks = ((r && r.networks) || []).map((n) => {
        const m = /^(\S*\d\S*) - (.+)$/.exec(String(n.name || ''))
        return { ...n, ndexTitle: m ? m[2] : n.name, ndexId: m ? m[1] : null }
      })
      sortedByName(networks, (n) => n.ndexTitle).forEach((n) => {
        const big = (n.nodeCount || 0) > MAX_NETWORK_NODES ? ' — larger than NORMA shows' : ''
        const label = n.ndexId ? `${n.ndexTitle} (${n.ndexId})` : n.ndexTitle
        sel.add(
          new Option(
            `${label} · ${(n.nodeCount || 0).toLocaleString()} nodes, ${(n.edgeCount || 0).toLocaleString()} edges · ${n.owner || ''}${big}`,
            n.externalId
          )
        )
      })
    }
    document.getElementById('ndexPickRow').hidden = !sel.options.length
    dbStatus(
      'ndex',
      sel.options.length
        ? [
            {
              level: 'ok',
              text: `Found ${plural(sel.options.length, 'network')}. Choose one and fetch it.`,
            },
          ]
        : [{ level: 'warn', text: 'No public network matched.' }]
    )
  })
}

// Reads a CX2 document: attribute aliases and defaults are resolved.
function readCx2(cx) {
  const aspects = {}
  ;(Array.isArray(cx) ? cx : []).forEach((part) =>
    Object.entries(part).forEach(([k, v]) => {
      if (Array.isArray(v)) (aspects[k] = aspects[k] || []).push(...v)
    })
  )
  const decl = (aspects.attributeDeclarations || [])[0] || {}
  const resolver = (kind) => {
    const d = decl[kind] || {}
    const alias = {},
      defaults = {}
    Object.entries(d).forEach(([name, spec]) => {
      if (spec && spec.a) alias[spec.a] = name
      if (spec && spec.v !== undefined) defaults[name] = spec.v
    })
    return (v) => {
      const out = { ...defaults }
      Object.entries(v || {}).forEach(([k, val]) => {
        out[alias[k] || k] = val
      })
      return out
    }
  }
  const rn = resolver('nodes'),
    re = resolver('edges')
  const netAttrs = Object.assign({}, ...(aspects.networkAttributes || []))
  return {
    name: netAttrs.name || '',
    nodes: (aspects.nodes || []).map((n) => ({ id: n.id, v: rn(n.v), x: n.x, y: n.y })),
    edges: (aspects.edges || []).map((e) => ({ id: e.id, s: e.s, t: e.t, v: re(e.v) })),
    declarations: decl,
  }
}

export async function ndexFetch() {
  await dbRun('ndex', async () => {
    const uuid = document.getElementById('ndexNetwork').value
    if (!uuid) throw new Error('Search and choose a network first.')
    dbPlan('ndex', [8, 1])
    dbStep('ndex', 0, 'Downloading the network from NDEx…')
    const cx = await dbFetch('ndex', `${DB_URLS.ndex}/v3/networks/${encodeURIComponent(uuid)}`)
    const net = readCx2(cx)
    if (!net.nodes.length) throw new Error('This network has no nodes, or is not public.')
    const names = new Map(),
      used = new Set()
    net.nodes.forEach((n) => {
      let name = String(n.v.name ?? n.v.n ?? n.v.label ?? n.id).trim() || String(n.id)
      if (used.has(name)) name = `${name} (${n.id})`
      used.add(name)
      names.set(n.id, name)
    })
    const weightKey = ['weight', 'Weight', 'score', 'Score', 'confidence'].find((k) =>
      net.edges.some((e) => typeof e.v[k] === 'number')
    )
    const edges = net.edges
      .filter((e) => names.has(e.s) && names.has(e.t) && e.s !== e.t)
      .map((e) => ({
        source: names.get(e.s),
        target: names.get(e.t),
        type: String(e.v.interaction ?? e.v.i ?? 'interacts'),
        ...(weightKey && typeof e.v[weightKey] === 'number' ? { weight: e.v[weightKey] } : {}),
        directed: e.v.directed === true,
      }))
    // node attributes and groupings from categorical attributes
    const SKIP = /^(name|n|represents|r|alias|aliases|id|x|y|z|label|description|url|uri|link)$/i
    const keys = new Set()
    net.nodes.forEach((n) => Object.keys(n.v).forEach((k) => keys.add(k)))
    const nodeAttrs = {}
    net.nodes.forEach((n) => {
      const a = {}
      Object.entries(n.v).forEach(([k, v]) => {
        if (
          !/^(name|n)$/.test(k) &&
          (typeof v !== 'object' || v === null) &&
          Object.keys(a).length < 25
        )
          a[k] = v
      })
      const rep = String(n.v.represents || n.v.r || '')
      const m = rep.match(/uniprot(?:kb)?:([A-Z0-9-]+)/i)
      if (m) a.uniprot = m[1]
      nodeAttrs[names.get(n.id)] = a
    })
    const groupings = []
    ;[...keys]
      .filter((k) => !SKIP.test(k))
      .forEach((k) => {
        const groups = new Map()
        let covered = 0
        net.nodes.forEach((n) => {
          const v = n.v[k]
          const vals = Array.isArray(v)
            ? v
            : typeof v === 'string' || typeof v === 'boolean'
              ? [String(v)]
              : []
          const clean = vals.map((x) => String(x).trim()).filter((x) => x && x.length <= 80)
          if (clean.length) covered++
          clean.forEach((x) => {
            if (!groups.has(x)) groups.set(x, [])
            groups.get(x).push(names.get(n.id))
          })
        })
        if (groups.size < 2 || groups.size > 60 || covered < net.nodes.length * 0.3) return
        groupings.push({
          label: `groups by ${k}`,
          groups: [...groups].map(([name, members]) => ({ name, members })),
          score: covered,
        })
      })
    groupings.sort((a, b) => b.score - a.score)
    const positions = net.nodes.every((n) => Number.isFinite(n.x) && Number.isFinite(n.y))
      ? Object.fromEntries(net.nodes.map((n) => [names.get(n.id), { x: n.x, y: n.y }]))
      : null
    const notes = []
    if (!groupings.length)
      notes.push({
        level: 'ok',
        text: 'The network has no node attribute that makes useful groups (2 to 60 values covering at least 30% of the nodes); add an annotation or use communities.',
      })
    if (positions) notes.push({ level: 'ok', text: 'The layout saved in NDEx is kept.' })
    dbStep('ndex', 1, 'Building and opening the network…')
    await nextPaint()
    dbAddImport('ndex', {
      name: `NDEx ${net.name || uuid}`.slice(0, 90),
      edges,
      nodeAttrs,
      groupings: groupings.slice(0, 6),
      positions,
      notes,
      summary: `network ${uuid}`,
    })
  })
}

/* ---------------------------------------------------------------
   INTACT: molecular interactions from the IntAct PSICQUIC service
   (PSI-MITAB 2.5), filtered by the IntAct MI-score.
   --------------------------------------------------------------- */
function mitabName(idField, altField, aliasField) {
  const all = [aliasField, altField].join('|').split('|')
  const pick = (re) => {
    for (const x of all) {
      const m = x.match(re)
      if (m) return m[1]
    }
    return null
  }
  return (
    pick(/^uniprotkb:([^(]+)\(gene name\)/) ||
    pick(/^[^:]+:([^(]+)\(gene name\)/) ||
    pick(/^psi-mi:([^(]+)\(display_short\)/) ||
    String(idField)
      .split('|')[0]
      .replace(/^[^:]+:/, '')
  )
}

function mitabTerm(field) {
  const m = String(field).match(/\(([^)]+)\)/)
  return m ? m[1] : String(field).replace(/^[^:]+:/, '')
}

export async function intactFetch() {
  await dbRun('intact', async () => {
    const names = dbSplitList(document.getElementById('intactQuery').value)
    if (!names.length)
      throw new Error('Type one or more gene names or UniProt accessions, for example TP53.')
    const taxon = document.getElementById('intactTaxon').value
    const minScore = parseFloat(document.getElementById('intactScore').value) || 0
    const maxRows = Math.max(
      50,
      Math.min(5000, parseInt(document.getElementById('intactMaxRows').value, 10) || 1000)
    )
    const among = document.getElementById('intactAmong').checked
    const maxPartners = Math.max(0, parseInt(document.getElementById('intactMax').value, 10) || 0)
    const ids = names.map((n) => (/[\s():]/.test(n) ? `"${n}"` : n)).join(' OR ')
    let miql = `identifier:(${ids})`
    if (taxon) miql += ` AND taxidA:${taxon} AND taxidB:${taxon}`
    dbPlan('intact', [8, 1])
    dbStep('intact', 0, 'Fetching interactions from IntAct…')
    const text = await dbFetch(
      'intact',
      `${DB_URLS.intact}/query/${encodeURIComponent(miql)}?format=tab25&firstResult=0&maxResults=${maxRows}`,
      { accept: 'text/plain', text: true }
    )
    const lines = String(text || '')
      .split('\n')
      .filter((l) => l.trim())
    if (!lines.length)
      throw new Error('IntAct has no interactions for these names in this organism.')
    const nodeAttrs = {}
    const query = new Set(names.map((n) => n.toUpperCase()))
    const agg = new Map()
    let lowScore = 0
    lines.forEach((line) => {
      const c = line.split('\t')
      if (c.length < 15) return
      const nameA = mitabName(c[0], c[2], c[4]),
        nameB = mitabName(c[1], c[3], c[5])
      if (!nameA || !nameB || nameA === nameB) return
      const score = (String(c[14]).match(/intact-miscore:([\d.]+)/) || [])[1]
      const s = score === undefined ? NaN : parseFloat(score)
      if (Number.isFinite(s) && s < minScore) {
        lowScore++
        return
      }
      ;[
        [nameA, c[0]],
        [nameB, c[1]],
      ].forEach(([nm, idf]) => {
        if (nodeAttrs[nm]) return
        const acc = (String(idf).match(/uniprotkb:([A-Z0-9-]+)/) || [])[1]
        nodeAttrs[nm] = {
          ...(acc ? { uniprot: acc } : { intact_id: String(idf).split('|')[0] }),
          query: query.has(nm.toUpperCase()) || (acc && query.has(acc.toUpperCase())),
        }
      })
      const type = mitabTerm(String(c[11]).split('|')[0]) || 'interaction'
      const [x, y] = nameA < nameB ? [nameA, nameB] : [nameB, nameA]
      const k = `${x}\t${y}\t${type}`
      const cur = agg.get(k) || { source: x, target: y, type, weight: 0, evidence: 0 }
      cur.weight = Math.max(cur.weight, Number.isFinite(s) ? s : 0)
      cur.evidence++
      agg.set(k, cur)
    })
    let edges = [...agg.values()]
    if (among) edges = edges.filter((e) => nodeAttrs[e.source].query && nodeAttrs[e.target].query)
    const notes = []
    if (lines.length >= maxRows)
      notes.push({
        level: 'warn',
        text: `Read the first ${maxRows} interaction records; raise "Most records" for more.`,
      })
    if (lowScore)
      notes.push({
        level: 'ok',
        text: `Left out ${plural(lowScore, 'record')} with an MI-score below ${minScore}.`,
      })
    if (!among && maxPartners > 0) {
      const best = new Map()
      edges.forEach((e) =>
        [e.source, e.target].forEach((n) => {
          if (!nodeAttrs[n].query) best.set(n, Math.max(best.get(n) || 0, e.weight))
        })
      )
      if (best.size > maxPartners) {
        const keep = new Set(
          [...best]
            .sort((a, b) => b[1] - a[1])
            .slice(0, maxPartners)
            .map((x) => x[0])
        )
        notes.push({
          level: 'ok',
          text: `Kept the ${maxPartners} highest-scoring of ${best.size} partners.`,
        })
        edges = edges.filter((e) =>
          [e.source, e.target].every((n) => nodeAttrs[n].query || keep.has(n))
        )
      }
    }
    const present = new Set(edges.flatMap((e) => [e.source, e.target]))
    const groupings = [
      {
        label: 'query and partners',
        groups: [
          { name: 'Query proteins', members: [...present].filter((n) => nodeAttrs[n].query) },
          {
            name: 'Interaction partners',
            members: [...present].filter((n) => !nodeAttrs[n].query),
          },
        ],
      },
    ]
    dbStep('intact', 1, 'Building and opening the network…')
    await nextPaint()
    dbAddImport('intact', {
      name: `IntAct ${names.slice(0, 3).join(', ')}${names.length > 3 ? ` +${names.length - 3}` : ''}`,
      edges: edges.map(({ evidence, ...e }) => e),
      nodeAttrs,
      groupings,
      notes,
      summary: `MI-score at least ${minScore}; channels are interaction types, weights the best MI-score`,
    })
  })
}

/* ---------------------------------------------------------------
   GENE ONTOLOGY: a GO-CAM model as a causal gene-to-gene network,
   and GO terms (QuickGO annotations) as groupings for the view.
   --------------------------------------------------------------- */
const RO_LABELS = {
  'RO:0002629': 'directly positively regulates',
  'RO:0002630': 'directly negatively regulates',
  'RO:0002413': 'directly provides input for',
  'RO:0002211': 'regulates',
  'RO:0002213': 'positively regulates',
  'RO:0002212': 'negatively regulates',
  'RO:0002304': 'causally upstream of, positive effect',
  'RO:0002305': 'causally upstream of, negative effect',
  'RO:0002411': 'causally upstream of',
  'RO:0002418': 'causally upstream of or within',
  'RO:0012009': 'constitutively upstream of',
  'RO:0012010': 'removes input for',
  'RO:0002407': 'indirectly positively regulates',
  'RO:0002409': 'indirectly negatively regulates',
}

const termOf = (x) => (x == null ? null : typeof x === 'string' ? x : x.term || x.id || null)

// The taxon/models listing gives "gocam": "http://model.geneontology.org/<id>"
// (a full URL, not a bare id); gocam-model/<id> 404s on anything but the bare
// id, so every candidate field is reduced to its last path segment here, once,
// rather than passing whatever the API happened to send straight through.
const bareGoId = (s) => (s == null ? null : String(s).replace(/^.*\//, ''))

export async function goLoadModels() {
  await dbRun('go', async () => {
    const taxon = document.getElementById('goTaxon').value
    dbProgress('go', 'Listing GO-CAM models…')
    const r = await dbFetch('go', `${DB_URLS.goapi}/taxon/${encodeURIComponent(taxon)}/models`)
    const list = (Array.isArray(r) ? r : (r && (r.models || r.results)) || []).map((m) => {
      if (typeof m === 'string') {
        const id = bareGoId(m)
        return { id, title: id }
      }
      const id = bareGoId(m.id || m.gocam || m.model_id)
      return { id, title: m.title || m.name || id }
    })
    goState.models = list.filter((m) => m.id)
    const shown = filterGoModels()
    // The taxon listing has no titles (just ids), so the ones actually shown
    // are looked up one model at a time and the option text updated in
    // place; goState.models keeps them (title !== id marks "already known"),
    // so re-filtering to the same models within this session won't re-fetch.
    const toName = shown.filter((m) => m.title === m.id)
    if (toName.length) {
      dbProgress('go', `Reading model titles: 0 of ${toName.length}…`)
      await dbMap(
        'go',
        toName,
        6,
        async (m) => {
          try {
            const d = await dbFetch(
              'go',
              `${DB_URLS.goapi}/gocam-model/${encodeURIComponent(m.id)}`
            )
            if (d && d.title) m.title = d.title
          } catch {
            // keep the id as the label
          }
        },
        (done, total) => dbProgress('go', `Reading model titles: ${done} of ${total}…`)
      )
      // titles just came in: re-render so the list re-sorts by them instead
      // of staying in the id order it was first shown in
      renderGoOptions(shown)
    }
    dbStatus('go', [
      {
        level: goState.models.length ? 'ok' : 'warn',
        text: goState.models.length
          ? `Found ${plural(goState.models.length, 'GO-CAM model')}. Filter and choose one.`
          : 'No GO-CAM models for this organism.',
      },
    ])
  })
}

const goState = { models: [] }

// Before a title is known, m.title === m.id (see goLoadModels): show the
// bare id then, and "title (id)" once the real title comes in - the same
// "name (id)" shape as the Reactome/NDEx pickers.
const goOptionLabel = (m) => (m.title && m.title !== m.id ? `${m.title} (${m.id})` : m.id)

// Sorted by whatever each model's label is showing right now (title once
// known, the bare id until then), same as the Reactome/NDEx pickers.
function renderGoOptions(models) {
  const sel = document.getElementById('goModel')
  sel.innerHTML = ''
  sortedByName(models, (m) => m.title).forEach((m) => sel.add(new Option(goOptionLabel(m), m.id)))
}

export function filterGoModels() {
  const f = document.getElementById('goModelFilter').value.trim().toLowerCase()
  // Capped lower than Reactome/NDEx's 40: each not-yet-named model shown
  // costs its own gocam-model request (see goLoadModels), so this bounds a
  // single "List models"/filter to at most 60 of those round trips.
  const shown = goState.models
    .filter(
      (m) =>
        !f || String(m.title).toLowerCase().includes(f) || String(m.id).toLowerCase().includes(f)
    )
    .slice(0, 60)
  renderGoOptions(shown)
  document.getElementById('goPickRow').hidden = !goState.models.length
  return shown
}

export async function goFetchModel() {
  await dbRun('go', async () => {
    const typed = document.getElementById('goModelId').value.trim()
    const id = (typed || document.getElementById('goModel').value || '').replace(/^gomodel:/, '')
    if (!id) throw new Error('Choose a GO-CAM model or type its identifier.')
    dbPlan('go', [6, 1])
    dbStep('go', 0, 'Downloading the GO-CAM model…')
    const m = await dbFetch('go', `${DB_URLS.goapi}/gocam-model/${encodeURIComponent(id)}`)
    const labels = new Map((m.objects || []).map((o) => [o.id, o.label || o.id]))
    const label = (t) => {
      const l = labels.get(t) || t
      // gene product labels carry the organism, e.g. "Tp53 Mmus"
      return String(l).replace(/\s+[A-Z][a-z]{3}$/, '')
    }
    const acts = new Map((m.activities || []).map((a) => [a.id, a]))
    const enabler = (a) => termOf(a && a.enabled_by)
    const nodeAttrs = {}
    const nodeName = (t) => {
      const n = label(t)
      if (!nodeAttrs[n])
        nodeAttrs[n] = {
          go_cam_id: t,
          ...(/UniProtKB:/.test(t) ? { uniprot: t.replace('UniProtKB:', '') } : {}),
        }
      return n
    }
    const edges = []
    acts.forEach((a) => {
      const src = enabler(a)
      if (!src) return
      nodeName(src)
      ;(a.causal_associations || []).forEach((ca) => {
        const down = acts.get(termOf(ca.downstream_activity))
        const tgt = enabler(down)
        if (!tgt) return
        const pred = termOf(ca.predicate)
        edges.push({
          source: nodeName(src),
          target: nodeName(tgt),
          directed: true,
          type: labels.get(pred) || RO_LABELS[pred] || pred || 'causally related',
        })
      })
    })
    const byAspect = (field, lbl) => {
      const groups = new Map()
      acts.forEach((a) => {
        const src = enabler(a)
        const raw = a[field]
        const terms = (Array.isArray(raw) ? raw : raw ? [raw] : []).map(termOf).filter(Boolean)
        if (!src) return
        terms.forEach((t) => {
          const n = labels.get(t) || t
          if (!groups.has(n))
            groups.set(n, { name: n, members: [], meta: { description: `${lbl} ${t}`, term: t } })
          groups.get(n).members.push(label(src))
        })
      })
      return { label: lbl, groups: [...groups.values()] }
    }
    const groupings = [
      byAspect('part_of', 'biological process'),
      byAspect('occurs_in', 'cellular component'),
      byAspect('molecular_function', 'molecular function'),
    ]
    if (!edges.length) throw new Error('This model has no causal links between gene products.')
    dbStep('go', 1, 'Building and opening the network…')
    await nextPaint()
    dbAddImport('go', {
      name: `GO-CAM ${m.title || id}`.slice(0, 90),
      edges,
      nodeAttrs,
      groupings,
      summary: `model ${id}; edges are causal relations between activities, pointing downstream`,
    })
  })
}

export async function goAnnotateView() {
  await dbRun('go', async () => {
    if (!cy.nodes().length) throw new Error('Show a network first.')
    const aspects = [...document.querySelectorAll('#goAspects input:checked')].map((i) => i.value)
    if (!aspects.length) throw new Error('Tick at least one GO aspect.')
    const noIEA = document.getElementById('goNoIea').checked
    const minSize = Math.max(1, parseInt(document.getElementById('goMinSize').value, 10) || 2)
    const maxGroups = Math.max(1, parseInt(document.getElementById('goMaxGroups').value, 10) || 30)
    // UniProt accessions from node attributes or node names
    const accOf = new Map()
    cy.nodes().forEach((n) => {
      const a = n.data('attrs') || {}
      const acc = a.uniprot || (UNIPROT_RE.test(n.id()) ? n.id() : null)
      if (acc) accOf.set(String(acc).replace(/-\d+$/, ''), n.id())
    })
    if (!accOf.size)
      throw new Error(
        'No node has a UniProt accession (as its name or in a "uniprot" attribute). Networks from Reactome, OmniPath, IntAct and many NDEx networks carry them.'
      )
    const accs = [...accOf.keys()]
    const batches = []
    for (let i = 0; i < accs.length; i += 100) batches.push(accs.slice(i, i + 100))
    const groupings = []
    // steps: one per aspect, then adding the groupings
    dbPlan('go', [...aspects.map(() => 3), 1])
    for (const [ai, aspect] of aspects.entries()) {
      dbStep(
        'go',
        ai,
        `Reading ${aspect.replace('_', ' ')} annotations: 0 of ${batches.length} batches…`
      )
      const terms = new Map()
      const results = await dbMap(
        'go',
        batches,
        2,
        async (b) => {
          const all = []
          for (let page = 1; page <= 25; page++) {
            const r = await dbFetch(
              'go',
              `${DB_URLS.quickgo}/annotation/search?geneProductId=${b.join(',')}&aspect=${aspect}&limit=200&page=${page}&includeFields=goName`
            )
            all.push(...((r && r.results) || []))
            const info = r && r.pageInfo
            if (!info || page >= (info.total || 1)) break
          }
          return all
        },
        (d, n) =>
          dbProgress('go', `Reading ${aspect.replace('_', ' ')} annotations: ${d} of ${n} batches…`)
      )
      results.flat().forEach((r) => {
        if (!r || r.error) return
        if (noIEA && (r.goEvidence === 'IEA' || r.evidenceCode === 'ECO:0000501')) return
        if (r.qualifier && /^NOT/i.test(r.qualifier)) return
        const acc = String(r.geneProductId || '')
          .replace(/^UniProtKB:/, '')
          .replace(/-\d+$/, '')
        const node = accOf.get(acc)
        if (!node) return
        const name = r.goName || r.goId
        if (!terms.has(name))
          terms.set(name, {
            name,
            members: new Set(),
            meta: { description: `GO ${aspect.replace('_', ' ')}`, term: r.goId },
          })
        terms.get(name).members.add(node)
      })
      const groups = [...terms.values()]
        .map((t) => ({ ...t, members: [...t.members] }))
        .filter((t) => t.members.length >= minSize && t.members.length < accOf.size)
        .sort((a, b) => b.members.length - a.members.length)
        .slice(0, maxGroups)
      groupings.push({ label: `GO ${aspect.replace('_', ' ')}`, groups })
    }
    dbStep('go', aspects.length, 'Adding the groupings…')
    await nextPaint()
    const v = activeView()
    const entries = dbAddGroupingsToView('go', groupings, v ? v.name : 'View')
    dbStatus(
      'go',
      entries.length
        ? [
            {
              level: 'ok',
              text: `Added ${plural(entries.length, 'GO grouping')} for ${plural(accOf.size, 'node')} with a UniProt accession: ${entries.map((e) => `${e.name.split(': ').pop()} (${e.parsed.summary})`).join(', ')}.`,
            },
          ]
        : [{ level: 'warn', text: 'No GO terms with enough members were found.' }]
    )
  })
}

// page wiring, run by main.ts in the original order
export function init() {
  document.getElementById('btnGaStats').addEventListener('click', renderGroupStats)

  document.getElementById('btnGaStatsTsv').addEventListener('click', groupStatsTsv)

  document.getElementById('btnGaGroupNet').addEventListener('click', openGroupNetwork)

  document.getElementById('btnGaEnrich').addEventListener('click', runEnrichment)

  document.getElementById('btnGaEnrichTsv').addEventListener('click', enrichmentTsv)

  document.getElementById('gaSets').addEventListener('change', refreshEnrichmentChoices)

  cy.on('select unselect', () => {
    if (currentTab === 'profiler') refreshEnrichmentChoices()
  })

  document.getElementById('btnCompareNet').addEventListener('click', openComparisonNetwork)

  document.querySelectorAll('#infoExplore [data-steps]').forEach((btn) =>
    btn.addEventListener('click', () => {
      if (S.infoNodeId && cy.$id(S.infoNodeId).length)
        openNeighbourhood([S.infoNodeId], +btn.dataset.steps)
    })
  )

  document.getElementById('btnSelNeighbours').addEventListener('click', () => {
    const ids = cy.nodes(':selected').map((n) => n.id())
    if (ids.length) openNeighbourhood(ids, 1)
  })

  document.getElementById('btnSelPaths').addEventListener('click', () => {
    const ids = cy.nodes(':selected').map((n) => n.id())
    if (ids.length !== 2) return
    // paths go from the node selected first to the one selected second
    const rank = (id) => {
      const i = selectionOrder.indexOf(id)
      return i < 0 ? Infinity : i
    }
    const [a, b] = rank(ids[0]) <= rank(ids[1]) ? ids : [ids[1], ids[0]]
    openShortestPaths(a, b)
  })

  cy.on('select', 'node', (e) => {
    const id = e.target.id()
    const i = selectionOrder.indexOf(id)
    if (i >= 0) selectionOrder.splice(i, 1)
    selectionOrder.push(id)
    if (selectionOrder.length > 50) selectionOrder.shift()
  })

  cy.on('unselect', 'node', (e) => {
    const i = selectionOrder.indexOf(e.target.id())
    if (i >= 0) selectionOrder.splice(i, 1)
  })

  document
    .querySelectorAll('[data-export-format]')
    .forEach((b) => b.addEventListener('click', () => exportOtherFormat(b.dataset.exportFormat)))

  document.getElementById('btnSessionSave').addEventListener('click', saveSession)

  document
    .getElementById('btnSessionOpen')
    .addEventListener('click', () => document.getElementById('sessionFileInput').click())

  document.getElementById('sessionFileInput').addEventListener('change', (e) => {
    const f = e.target.files[0]
    if (f) openSessionFile(f)
    e.target.value = ''
  })

  ;(function setupWebglToggle() {
    const box = document.getElementById('webglToggle')
    const note = document.getElementById('webglNote')
    box.checked = WEBGL_ACTIVE
    let stored = true
    try {
      localStorage.getItem('x')
    } catch (e) {
      stored = false
    }
    const describe = () => {
      const wanted = box.checked
      if (wanted === WEBGL_ACTIVE) {
        note.textContent = WEBGL_ACTIVE ? 'WebGL drawing is on.' : ''
        document.getElementById('btnWebglReload').hidden = true
      } else {
        note.textContent = stored
          ? 'Reload the page to switch; open views are lost unless you save the session first.'
          : 'This page can\u2019t store the setting; open it with ?webgl=1 at the end of the address instead.'
        document.getElementById('btnWebglReload').hidden = !stored
      }
    }
    box.addEventListener('change', () => {
      try {
        localStorage.setItem('norma3-webgl', box.checked ? '1' : '0')
      } catch (e) {}
      describe()
    })
    document.getElementById('btnWebglReload').addEventListener('click', () => location.reload())
    describe()
  })()

  document.getElementById('btnRuntime').addEventListener('click', runRuntimeTable)

  document.getElementById('btnRuntimeTsv').addEventListener('click', runtimeTsv)

  /* frames wiring */
  document.getElementById('btnFramePrev').addEventListener('click', () => {
    stopFramePlay()
    stepFrame(-1)
  })

  document.getElementById('btnFrameNext').addEventListener('click', () => {
    stopFramePlay()
    stepFrame(1)
  })

  document.getElementById('btnFramePlay').addEventListener('click', toggleFramePlay)

  document.getElementById('frameSlider').addEventListener('input', (e) => {
    stopFramePlay()
    showFrame(parseInt(e.target.value, 10) || 0)
  })

  document.getElementById('frameSpeed').addEventListener('change', () => {
    if (frameState.playing) {
      toggleFramePlay()
      toggleFramePlay()
    }
  })

  document.getElementById('btnFramesFromFiles').addEventListener('click', addFramesFromFiles)

  document.getElementById('valueSameRange').addEventListener('change', applyValueColors)

  document
    .getElementById('btnCmpArenaOpen')
    .addEventListener('click', () => openInArena3d('compare'))

  document
    .getElementById('btnCmpArenaJson')
    .addEventListener('click', () => exportArena3d('json', 'compare'))

  document
    .getElementById('btnCmpArenaTsv')
    .addEventListener('click', () => exportArena3d('tsv', 'compare'))

  dbState.cancelled = {}
}
