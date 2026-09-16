// @ts-nocheck
// ponytail: split verbatim from the former single-file script; typed one file at a time (remove @ts-nocheck as it lands)
import { COMMUNITY_ALGORITHMS, WALKTRAP_NODE_LIMIT } from './welcome'
import {
  CONFIG_FIELDS,
  applyEdgePalette,
  applyGroupVisibility,
  applyNodePalette,
  buildGroupLegend,
  buildLegend,
  byName,
  drawGroupHulls,
  filteredGroups,
  getCurrentConfig,
  getUsedTypes,
  legendExtra,
  loadData,
  refreshAllDerivedUI,
  sortedByName,
} from './hulls'
import { EDGE_TYPES, NODE_PALETTES, ORIGINAL_EDGE_TYPES, colorAtIndex } from './palette'
import { PROFILE_DIRECTED_STATS, directedStatsTable, profileDirected } from './directed_stats'
import {
  PROFILE_PATH_LIMIT,
  PROFILE_STATS,
  formatStat,
  profileGraph,
  simpleGraph,
  switchTab,
} from './wiring'
import { S } from './state'
import {
  UNGROUPED,
  attrKind,
  attrRowsHtml,
  attrSchema,
  buildAttrSchema,
  customGroupAttrs,
  escapeHtml,
  formatAttrValue,
  getUsedGroups,
  groupDescription,
  groupLabel,
  nodeFillMode,
  refreshNodeVisual,
  renderAttrSchema,
  sanitizeColor,
} from './network_state'
import { WEBGL_ACTIVE, cy, setStyle } from './cy'
import {
  addNormaEntry,
  annotationText,
  downloadText,
  libEntry,
  libSelection,
  listSample,
  normaLibrary,
  plural,
  renderLibraryLists,
  selectedNetworks,
  setStatus,
} from './layouts/controls'
import { applyEdgeMerge, applyTypeVisibility } from './parallel_edges'
import { applyGroupsAndColorsInPlace, refreshLibraryView } from './library'
import { applyLabelColors } from './label_colors'
import {
  applyNodeSizing,
  bundleAsync,
  cancelFrJobs,
  ensureFullMetrics,
  invalidateFullMetrics,
  shownEdges,
  shownNodes,
  updateStats,
} from './metrics'
import { applyTheme, currentTheme } from './themes'
import { applyValueColors, scheduleLegend } from './clustering/mapping'
import { arrowsShown, edgeIsDirected } from './export/dialog'
import {
  bumpDataVersion,
  dataVersion,
  flushHistory,
  historyTimer,
  setHistoryBaseline,
  updateUndoButtons,
} from './demo_downloads'
import { capture3d, restore3d } from './view3d/tab'
import { fit3d } from './view3d/camera'
import { net3d } from './view3d/state'
import { refreshAfterDelete } from './api/tester'
import { runCommunityAlgorithm } from './clustering/mcl'
import { setLayoutBusy } from './layouts/run'
import { updateContextInfo } from './recording'

/* ---------- profiler UI ---------- */
export const profilerState = { results: [] }

export function renderProfilerNetworkList() {
  const el = document.getElementById('profNetList')
  if (!el) return
  const previous = new Set([...el.querySelectorAll('input:checked')].map((i) => i.value))
  const firstRender = !el.dataset.rendered
  el.dataset.rendered = '1'
  el.innerHTML = ''
  const cur = typeof activeView === 'function' && activeView()
  const options = [
    {
      value: 'view',
      label: `Current view: ${cur ? cur.name : 'untitled'} (${shownNodes().length} shown nodes)`,
    },
  ].concat(
    sortedByName(normaLibrary.network, (e) => e.name).map((e) => ({ value: e.id, label: e.name }))
  )
  options.forEach((o) => {
    const label = document.createElement('label')
    const input = document.createElement('input')
    input.type = 'checkbox'
    input.value = o.value
    input.checked = firstRender ? o.value === 'view' : previous.has(o.value)
    const span = document.createElement('span')
    span.textContent = o.label
    label.append(input, span)
    el.appendChild(label)
  })
}

function graphForSource(value) {
  if (value === 'view') {
    const ids = shownNodes().map((n) => n.id())
    const shownE = shownEdges(true)
    const pairs = shownE.map((e) => [e.data('source'), e.data('target')])
    const dirEdges = shownE.map((e) => [e.data('source'), e.data('target'), edgeIsDirected(e)])
    const viewIds = S.currentLibView ? S.currentLibView.nets.split('|') : []
    const nets = normaLibrary.network.filter((e) => viewIds.includes(e.id))
    const viewLabel = nets.length ? nets.map((e) => e.name).join(' + ') : 'Current view'
    const cur = typeof activeView === 'function' && activeView()
    return {
      name: cur ? `View: ${cur.name}` : 'Current view',
      label: viewLabel,
      graph: simpleGraph(ids, pairs),
      isView: true,
      ids,
      dirEdges,
    }
  }
  const entry = libEntry('network', value)
  if (!entry) return null
  return {
    name: entry.name,
    graph: simpleGraph(
      entry.parsed.nodes,
      entry.parsed.edges.map((e) => [e.source, e.target])
    ),
    ids: entry.parsed.nodes,
    dirEdges: entry.parsed.edges.map((e) => [e.source, e.target, !!e.directed]),
    isView: false,
    entry,
  }
}

function runProfiler() {
  const chosen = [...document.querySelectorAll('#profNetList input:checked')].map((i) => i.value)
  if (!chosen.length) {
    setStatus('profStatus', [{ level: 'error', text: 'Tick at least one network to profile.' }])
    return
  }
  if (chosen.includes('view') && !cy.nodes().length) {
    setStatus('profStatus', [
      {
        level: 'error',
        text: 'The current view is empty. Load a network first, or untick "Current view".',
      },
    ])
    return
  }
  setStatus('profStatus', [{ level: 'busy', text: 'Computing statistics…' }])
  document.getElementById('btnProfile').disabled = true
  // let the status paint before the (synchronous) computation starts
  setTimeout(() => {
    try {
      const started = performance.now()
      const useDir = document.getElementById('profDirected').checked
      const results = chosen
        .map((value) => {
          const g = graphForSource(value)
          return g && { ...g, sourceValue: value }
        })
        .filter(Boolean)
        .map((src) => ({
          ...src,
          stats: profileGraph(src.graph),
          directed:
            useDir && src.dirEdges.some((d) => d[2])
              ? profileDirected(src.ids, src.dirEdges)
              : null,
        }))
      profilerState.results = results
      renderProfilerResults(results)
      const secs = ((performance.now() - started) / 1000).toFixed(2)
      const notes = [
        { level: 'ok', text: `Profiled ${plural(results.length, 'network')} in ${secs} s.` },
      ]
      if (results.some((r) => r.stats.pathsSkipped))
        notes.push({
          level: 'warn',
          text: `Path-based statistics were skipped for networks above ${PROFILE_PATH_LIMIT.toLocaleString('en-US')} nodes.`,
        })
      setStatus('profStatus', notes)
      document.getElementById('btnProfileTsv').disabled = false
    } catch (err) {
      setStatus('profStatus', [
        { level: 'error', text: `The statistics couldn't be computed: ${err.message}` },
      ])
    } finally {
      document.getElementById('btnProfile').disabled = false
    }
  }, 30)
}

export function renderProfilerResults(results) {
  const root = document.getElementById('profResults')
  root.innerHTML = ''

  // statistics table
  const sec = profSection('Statistics', '')
  const wrap = document.createElement('div')
  wrap.className = 'table-wrap'
  const table = document.createElement('table')
  table.className = 'data'
  const head = document.createElement('tr')
  head.innerHTML =
    '<th scope="col">Statistic</th>' +
    results.map((r) => `<th scope="col">${escapeHtml(r.name)}</th>`).join('')
  const thead = document.createElement('thead')
  thead.appendChild(head)
  const tbody = document.createElement('tbody')
  PROFILE_STATS.forEach((st) => {
    const tr = document.createElement('tr')
    tr.innerHTML =
      `<td>${escapeHtml(st.label)}<span class="stat-desc">${escapeHtml(st.desc)}</span></td>` +
      results.map((r) => `<td class="num">${escapeHtml(formatStat(r.stats[st.key]))}</td>`).join('')
    tbody.appendChild(tr)
  })
  table.append(thead, tbody)
  wrap.appendChild(table)
  sec.appendChild(wrap)
  const split = results.filter((r) => r.stats.components > 1).map((r) => r.name)
  if (split.length) {
    sec.insertAdjacentHTML(
      'beforeend',
      `<p class="sub" style="margin:8px 0 0;">${escapeHtml(listSample(split, 4))} ${split.length === 1 ? 'has' : 'have'} more than one component. As in igraph, closeness is then computed within each component, so closeness centralization can exceed 1 and isn't comparable with connected networks.</p>`
    )
  }
  root.appendChild(sec)

  if (results.some((r) => r.directed)) {
    const dsec = profSection(
      'Direction',
      'Statistics that follow edge directions, for networks with directed edges. Undirected edges can be walked both ways. A dash means the network has no directed edges.'
    )
    dsec.insertAdjacentHTML('beforeend', directedStatsTable(results))
    root.appendChild(dsec)
  }

  // degree distributions
  const degSec = profSection('Degree distribution', 'Number of nodes with each degree.')
  const controls = document.createElement('div')
  controls.className = 'inline-controls'
  controls.innerHTML = '<label><input type="checkbox" id="degLogLog"> log–log scale</label>'
  degSec.appendChild(controls)
  const degCards = document.createElement('div')
  degCards.className = 'cards'
  degSec.appendChild(degCards)
  const drawDeg = () => {
    const log = document.getElementById('degLogLog').checked
    degCards.innerHTML = ''
    results.forEach((r) => {
      const card = document.createElement('div')
      card.className = 'card chart'
      card.innerHTML =
        `<h4 title="${escapeHtml(r.name)}">${escapeHtml(r.name)}</h4>` +
        degreeChartSvg(r.stats.degree, log)
      degCards.appendChild(card)
    })
  }
  controls.querySelector('input').addEventListener('change', drawDeg)
  root.appendChild(degSec)
  drawDeg()

  // central nodes
  const cSec = profSection('Most central nodes', 'Top ten nodes by the chosen measure.')
  const cControls = document.createElement('div')
  cControls.className = 'inline-controls'
  cControls.innerHTML = `<label>Rank by <select id="centralBy">
      <option value="degree">Degree</option>
      <option value="betweenness">Betweenness</option>
      <option value="closeness">Closeness</option>
      <option value="clustering">Clustering coefficient</option>
    </select></label>`
  cSec.appendChild(cControls)
  const cCards = document.createElement('div')
  cCards.className = 'cards'
  cCards.style.gridTemplateColumns = 'repeat(auto-fill, minmax(400px, 1fr))'
  cSec.appendChild(cCards)
  const drawCentral = () => {
    const by = document.getElementById('centralBy').value
    cCards.innerHTML = ''
    results.forEach((r) => {
      const s = r.stats
      const values =
        by === 'degree'
          ? s.degree
          : by === 'betweenness'
            ? s.betweenness
            : by === 'closeness'
              ? s.closeness
              : s.localClustering
      const card = document.createElement('div')
      card.className = 'card'
      card.innerHTML = `<h4 title="${escapeHtml(r.name)}">${escapeHtml(r.name)}</h4>`
      if (!values) {
        card.insertAdjacentHTML(
          'beforeend',
          '<p class="meta">Not computed for a network this large.</p>'
        )
        cCards.appendChild(card)
        return
      }
      const idx = Array.from({ length: r.graph.n }, (_, i) => i)
        .filter((i) => Number.isFinite(values[i]))
        .sort((a, b) => values[b] - values[a])
        .slice(0, 10)
      const rows = idx
        .map((i) => {
          const id = r.graph.ids[i]
          const inView = !!cy.getElementById(id).length
          const nameCell = inView
            ? `<button type="button" class="linkish" data-node="${escapeHtml(id)}" title="Show in the network">${escapeHtml(id)}</button>`
            : escapeHtml(id)
          return `<tr><td>${nameCell}</td><td class="num">${escapeHtml(formatStat(s.degree[i]))}</td><td class="num">${s.betweenness ? escapeHtml(formatStat(s.betweenness[i])) : '—'}</td><td class="num">${s.closeness ? escapeHtml(formatStat(s.closeness[i])) : '—'}</td></tr>`
        })
        .join('')
      card.insertAdjacentHTML(
        'beforeend',
        `<div class="table-wrap"><table class="data"><thead><tr><th>Node</th><th>Degree</th><th>Betweenness</th><th>Closeness</th></tr></thead><tbody>${rows}</tbody></table></div>`
      )
      cCards.appendChild(card)
    })
    cCards
      .querySelectorAll('button[data-node]')
      .forEach((b) => b.addEventListener('click', () => focusNodeInNetwork(b.dataset.node)))
  }
  cControls.querySelector('select').addEventListener('change', drawCentral)
  root.appendChild(cSec)
  drawCentral()

  // communities
  const lSec = profSection(
    'Communities',
    'Find communities with one of five algorithms and add them to Files as an annotation to view them as groups. Modularity is computed the same way for every algorithm.'
  )
  const algo = profilerState.cmAlgo || 'louvain'
  lSec.insertAdjacentHTML(
    'beforeend',
    `<div class="cm-controls">
    <label class="inline-num">Algorithm
      <select id="cmAlgo">${Object.entries(COMMUNITY_ALGORITHMS)
        .map(([k, a]) => `<option value="${k}"${k === algo ? ' selected' : ''}>${a.label}</option>`)
        .join('')}</select>
    </label>
    <label class="inline-num" id="cmParamLabel"><span id="cmParamName">Resolution</span>
      <input type="number" id="cmParam" class="num-input" step="0.1">
    </label>
    <button type="button" class="primary" id="btnCommunities">Find communities</button>
    <span class="hint-line" id="cmHint"></span>
  </div>`
  )
  const lCards = document.createElement('div')
  lCards.className = 'cards'
  lSec.appendChild(lCards)
  const drawCommunities = () => {
    lCards.innerHTML = ''
    results.forEach((r) => {
      const lv = r.cmResult || r.stats.louvain
      const label = r.cmLabel || 'Louvain'
      const card = document.createElement('div')
      card.className = 'card'
      if (r.cmError) {
        card.innerHTML = `<h4 title="${escapeHtml(r.name)}">${escapeHtml(r.name)}</h4><p class="meta">${escapeHtml(r.cmError)}</p>`
        lCards.appendChild(card)
        return
      }
      const nonSingle = lv.sizes.filter((x) => x > 1)
      const sorted = [...lv.sizes].sort((a, b) => b - a)
      const strip = sorted
        .slice(0, 40)
        .map(
          (sz, i) =>
            `<span style="width:${((100 * sz) / r.graph.n).toFixed(2)}%; background:${colorAtIndex(NODE_PALETTES.vivid, i)}" title="${sz} nodes"></span>`
        )
        .join('')
      card.innerHTML = `
        <h4 title="${escapeHtml(r.name)}">${escapeHtml(r.name)}</h4>
        <p class="meta"><b>${escapeHtml(label)}</b>: ${plural(lv.count, 'community', 'communities')} (${nonSingle.length} with more than one node), modularity ${escapeHtml(formatStat(lv.modularity))}${r.cmMs !== undefined ? `, ${Math.round(r.cmMs)} ms` : ''}. Largest: ${sorted.slice(0, 5).join(', ')} nodes.</p>
        <div class="size-strip" aria-hidden="true">${strip}</div>`
      const btn = document.createElement('button')
      btn.type = 'button'
      btn.textContent = 'Add as annotation'
      btn.disabled = !nonSingle.length
      btn.addEventListener('click', () => addCommunitiesAsAnnotation(r))
      card.appendChild(btn)
      lCards.appendChild(card)
    })
  }
  const paramFor = (a) =>
    ({
      louvain: [
        'Resolution',
        1,
        0.05,
        10,
        0.1,
        'Higher resolution gives more, smaller communities.',
      ],
      leiden: [
        'Resolution',
        1,
        0.05,
        10,
        0.1,
        'Like Louvain, but every community is guaranteed to be connected.',
      ],
      lpa: [
        null,
        null,
        0,
        0,
        0,
        'Each node takes the most common label among its neighbours until nothing changes.',
      ],
      walktrap: [
        'Walk length',
        4,
        2,
        10,
        1,
        `Short random walks tend to stay inside communities. Up to ${WALKTRAP_NODE_LIMIT.toLocaleString('en-US')} nodes.`,
      ],
      mcl: ['Inflation', 2, 1.1, 6, 0.1, 'Higher inflation gives more, smaller clusters.'],
    })[a]
  const syncParam = () => {
    const a = lSec.querySelector('#cmAlgo').value
    const [name, def, min, max, step, hint] = paramFor(a)
    const input = lSec.querySelector('#cmParam')
    lSec.querySelector('#cmParamLabel').hidden = !name
    if (name) {
      lSec.querySelector('#cmParamName').textContent = name
      Object.assign(input, { min, max, step })
      const saved = profilerState.cmParams && profilerState.cmParams[a]
      input.value = saved ?? def
    }
    lSec.querySelector('#cmHint').textContent = hint
  }
  lSec.querySelector('#cmAlgo').addEventListener('change', syncParam)
  lSec.querySelector('#btnCommunities').addEventListener('click', () => {
    const a = lSec.querySelector('#cmAlgo').value
    const value = parseFloat(lSec.querySelector('#cmParam').value)
    const [name, def, min, max] = paramFor(a)
    const v = name ? Math.min(max, Math.max(min, Number.isFinite(value) ? value : def)) : null
    profilerState.cmAlgo = a
    profilerState.cmParams = { ...(profilerState.cmParams || {}), [a]: v }
    const params =
      a === 'walktrap'
        ? { steps: Math.round(v) }
        : a === 'mcl'
          ? { inflation: v }
          : { resolution: v }
    setStatus('profStatus', [
      { level: 'busy', text: `Finding communities with ${COMMUNITY_ALGORITHMS[a].label}…` },
    ])
    setTimeout(() => {
      results.forEach((r) => {
        const t0 = performance.now()
        try {
          r.cmResult = runCommunityAlgorithm(r.graph, a, params)
          r.cmError = null
        } catch (err) {
          r.cmResult = null
          r.cmError = err.message
        }
        r.cmMs = performance.now() - t0
        r.cmAlgo = a
        r.cmLabel = `${COMMUNITY_ALGORITHMS[a].label}${name ? ` (${name.toLowerCase()} ${a === 'walktrap' ? Math.round(v) : v})` : ''}`
      })
      drawCommunities()
      setStatus('profStatus', [
        { level: 'ok', text: `Found communities with ${COMMUNITY_ALGORITHMS[a].label}.` },
      ])
    }, 30)
  })
  syncParam()
  drawCommunities()
  root.appendChild(lSec)

  // channels of the current view
  const viewResult = results.find((r) => r.isView)
  const channels = viewResult ? channelProfile() : null
  if (channels) root.appendChild(renderChannelSection(channels))
}

export function profSection(title, sub) {
  const sec = document.createElement('section')
  sec.className = 'prof-section'
  sec.innerHTML =
    `<h3>${escapeHtml(title)}</h3>` + (sub ? `<p class="sub">${escapeHtml(sub)}</p>` : '')
  return sec
}

function degreeChartSvg(deg, logScale) {
  const W = 340,
    H = 170,
    L = 40,
    R = 10,
    T = 10,
    B = 28
  const counts = new Map()
  let maxDeg = 0
  deg.forEach((d) => {
    counts.set(d, (counts.get(d) || 0) + 1)
    if (d > maxDeg) maxDeg = d
  })
  const pw = W - L - R,
    ph = H - T - B
  let body = ''
  let xLabels = '',
    yLabels = ''
  if (logScale) {
    const pts = [...counts].filter(([d]) => d > 0)
    if (!pts.length) return '<p class="meta">No connected nodes to plot.</p>'
    const maxC = Math.max(...pts.map((p) => p[1]))
    const lx = Math.log10(Math.max(maxDeg, 10)),
      ly = Math.log10(Math.max(maxC, 10))
    pts.forEach(([d, c]) => {
      const x = L + (Math.log10(d) / lx) * pw
      const y = T + ph - (Math.log10(c) / ly) * ph
      body += `<circle class="dot" cx="${x.toFixed(1)}" cy="${y.toFixed(1)}" r="3"><title>degree ${d}: ${c} nodes</title></circle>`
    })
    xLabels = `<text x="${L}" y="${H - 8}">1</text><text x="${W - R}" y="${H - 8}" text-anchor="end">${Math.round(Math.pow(10, lx))}</text>`
    yLabels = `<text x="${L - 6}" y="${T + ph}" text-anchor="end">1</text><text x="${L - 6}" y="${T + 8}" text-anchor="end">${Math.round(Math.pow(10, ly))}</text>`
  } else {
    const binWidth = Math.max(1, Math.ceil((maxDeg + 1) / 60))
    const bins = new Array(Math.floor(maxDeg / binWidth) + 1).fill(0)
    counts.forEach((c, d) => {
      bins[Math.floor(d / binWidth)] += c
    })
    const maxC = Math.max(...bins, 1)
    const bw = pw / bins.length
    bins.forEach((c, i) => {
      if (!c) return
      const h = (c / maxC) * ph
      const lo = i * binWidth,
        hi = lo + binWidth - 1
      body += `<rect class="bar" x="${(L + i * bw + 0.5).toFixed(1)}" y="${(T + ph - h).toFixed(1)}" width="${Math.max(1, bw - 1).toFixed(1)}" height="${h.toFixed(1)}"><title>degree ${binWidth > 1 ? `${lo}–${hi}` : lo}: ${c} nodes</title></rect>`
    })
    xLabels = `<text x="${L}" y="${H - 8}">0</text><text x="${W - R}" y="${H - 8}" text-anchor="end">${maxDeg}</text>`
    yLabels = `<text x="${L - 6}" y="${T + ph}" text-anchor="end">0</text><text x="${L - 6}" y="${T + 8}" text-anchor="end">${maxC}</text>`
  }
  return `<svg viewBox="0 0 ${W} ${H}" role="img" aria-label="Degree distribution">
    <line class="axis" x1="${L}" y1="${T + ph}" x2="${W - R}" y2="${T + ph}"/>
    <line class="axis" x1="${L}" y1="${T}" x2="${L}" y2="${T + ph}"/>
    ${body}${xLabels}${yLabels}
    <text x="${L + pw / 2}" y="${H - 8}" text-anchor="middle">degree</text>
  </svg>`
}

function focusNodeInNetwork(id) {
  const node = cy.getElementById(id)
  if (!node.length) return
  switchTab('network')
  if (node.hasClass('hidden-group')) {
    ;(node.data('groups') || []).forEach((g) => S.activeGroups.add(g))
    buildGroupLegend()
    applyGroupVisibility()
  }
  cy.animate(
    { center: { eles: node }, zoom: Math.max(cy.zoom(), 1.2) },
    { duration: 300, complete: clampViewport }
  )
  node.emit('tap')
}

function addCommunitiesAsAnnotation(result) {
  const lv = result.cmResult || result.stats.louvain
  const algoName = COMMUNITY_ALGORITHMS[result.cmAlgo || 'louvain'].short
  const members = new Map()
  result.graph.ids.forEach((id, i) => {
    const c = lv.membership[i]
    if (!members.has(c)) members.set(c, [])
    members.get(c).push(id)
  })
  const groups = [...members.values()]
    .filter((list) => list.length > 1)
    .sort((a, b) => b.length - a.length)
    .map((list, i) => ({ name: `Community-${i + 1}`, members: list }))
  const skippedNodes = result.graph.n - groups.reduce((s, g) => s + g.members.length, 0)
  const entry = addNormaEntry(
    'annotation',
    `${result.label || result.name} ${algoName} communities`,
    annotationText(groups),
    ''
  )
  libSelection.annotation = entry.id
  renderLibraryLists()
  const notes = [
    {
      level: 'ok',
      text: `Added the annotation "${entry.name}" with ${plural(groups.length, 'community', 'communities')} to Files and selected it.`,
    },
  ]
  if (skippedNodes)
    notes.push({
      level: 'ok',
      text: `${plural(skippedNodes, 'node')} that form a community on their own ${skippedNodes === 1 ? 'was' : 'were'} left out.`,
    })
  if (result.isView && S.currentLibView) {
    notes.push({
      level: 'ok',
      text: 'Show them in this view to see the communities as groups.',
      action: {
        label: 'Show in this view',
        run: () => {
          refreshLibraryView()
          switchTab('network')
        },
      },
    })
  } else if (result.isView) {
    notes.push({
      level: 'ok',
      text: 'The current view came from an example or JSON rather than Files, so the communities can be applied to it here.',
      action: {
        label: 'Show as groups now',
        run: () => {
          applyCommunitiesToView(groups)
          switchTab('network')
        },
      },
    })
  } else {
    notes.push({
      level: 'ok',
      text: `Tick "${result.name}" under Files and choose Show in this view to see them.`,
    })
  }
  setStatus('profStatus', notes)
}

// For views that didn't come from Files: set groups directly.
function applyCommunitiesToView(groups) {
  const groupsOf = {}
  groups.forEach((g) => g.members.forEach((id) => (groupsOf[id] = groupsOf[id] || []).push(g.name)))
  document.getElementById('nodeFillSelect').value = 'groups'
  applyGroupsAndColorsInPlace(
    groupsOf,
    new Map(),
    groups.map((g) => g.name)
  )
}

function channelProfile() {
  const types = getUsedTypes()
  if (types.length < 2) return null
  const pairsByType = new Map(types.map((t) => [t, new Set()]))
  const typesByPair = new Map()
  cy.edges().forEach((e) => {
    const s = e.data('source'),
      t = e.data('target')
    if (s === t) return
    const key = s < t ? s + '\t' + t : t + '\t' + s
    const type = e.data('type')
    pairsByType.get(type).add(key)
    if (!typesByPair.has(key)) typesByPair.set(key, new Set())
    typesByPair.get(key).add(type)
  })
  const perPair = new Map()
  typesByPair.forEach((set) => {
    perPair.set(set.size, (perPair.get(set.size) || 0) + 1)
  })
  return {
    types,
    pairsByType,
    perPair,
    totalPairs: typesByPair.size,
    totalEdges: cy.edges().length,
  }
}

function renderChannelSection(ch) {
  const sec = profSection(
    'Edge channels in the current view',
    `${plural(ch.totalEdges, 'edge')} over ${plural(ch.totalPairs, 'connected node pair')}.`
  )
  const byCount = [...ch.perPair]
    .sort((a, b) => a[0] - b[0])
    .map(
      ([k, v]) =>
        `<tr><td>${plural(k, 'channel')}</td><td class="num">${v.toLocaleString('en-US')}</td><td class="num">${((100 * v) / ch.totalPairs).toFixed(1)}%</td></tr>`
    )
    .join('')
  const shown = ch.types.slice(0, 12)
  const matrixHead =
    '<tr><th>Channel</th><th>Pairs</th>' +
    shown.map((t, i) => `<th title="${escapeHtml(t)}">${i + 1}</th>`).join('') +
    '</tr>'
  const matrixRows = shown
    .map((a, i) => {
      const A = ch.pairsByType.get(a)
      const cells = shown
        .map((b, j) => {
          if (i === j) return '<td class="num">—</td>'
          const B = ch.pairsByType.get(b)
          let shared = 0
          const [small, big] = A.size < B.size ? [A, B] : [B, A]
          small.forEach((k) => {
            if (big.has(k)) shared++
          })
          const union = A.size + B.size - shared
          const jac = union ? shared / union : 0
          return `<td class="num" title="${escapeHtml(a)} and ${escapeHtml(b)}: ${shared} shared pairs">${jac.toFixed(2)}</td>`
        })
        .join('')
      return `<tr><td>${i + 1}. ${escapeHtml(a)}</td><td class="num">${A.size.toLocaleString('en-US')}</td>${cells}</tr>`
    })
    .join('')
  sec.insertAdjacentHTML(
    'beforeend',
    `
    <div class="cards" style="grid-template-columns:minmax(260px, 340px) minmax(0, 1fr);">
      <div class="card">
        <h4>Node pairs by number of channels</h4>
        <div class="table-wrap"><table class="data"><thead><tr><th>Carried by</th><th>Pairs</th><th>Share</th></tr></thead><tbody>${byCount}</tbody></table></div>
      </div>
      <div class="card">
        <h4>Channel overlap (Jaccard index of node pairs)</h4>
        <p class="meta">1 means two channels connect exactly the same pairs; 0 means they share none.${ch.types.length > shown.length ? ` Showing the first ${shown.length} of ${ch.types.length} channels.` : ''}</p>
        <div class="table-wrap"><table class="data"><thead>${matrixHead}</thead><tbody>${matrixRows}</tbody></table></div>
      </div>
    </div>`
  )
  return sec
}

function profilerTsv() {
  const results = profilerState.results
  if (!results.length) return
  const lines = [['Statistic', ...results.map((r) => r.name)].join('\t')]
  PROFILE_STATS.forEach((st) =>
    lines.push([st.label, ...results.map((r) => formatStat(r.stats[st.key]))].join('\t'))
  )
  if (results.some((r) => r.directed)) {
    PROFILE_DIRECTED_STATS.forEach((st) =>
      lines.push(
        [st.label, ...results.map((r) => (r.directed ? formatStat(r.directed[st.key]) : ''))].join(
          '\t'
        )
      )
    )
  }
  downloadText('network-profile.tsv', lines.join('\n') + '\n')
}

/* labels */
// Node label size per node: fixed, or grown in proportion to node size.
export function applyLabelSizes() {
  const base = parseFloat(document.getElementById('nodeLabelSize').value) || 12
  const grow = document.getElementById('labelScaleWithNode').checked
  cy.batch(() => {
    cy.nodes().forEach((n) => {
      const px = grow ? Math.max(3, (base * (n.data('size') || 42)) / 42) : base
      if (n.data('labelPx') !== px) n.data('labelPx', px)
    })
  })
}

// Edge label text. A merged edge speaks for all the parallel edges it stands for.
function edgeLabelText(e, mode) {
  const merged = e.data('mergedWith')
  const group = merged && merged.length > 1 ? merged.map((id) => cy.getElementById(id)) : [e]
  const channel =
    group.length > 1
      ? `${group.length} channels`
      : (EDGE_TYPES[e.data('type')] || {}).label || e.data('type')
  const weights = group.map((x) => x.data('weight')).filter((w) => typeof w === 'number')
  const weight = weights.length ? formatAttrValue(Math.max(...weights)) : ''
  if (mode === 'weight') return weight
  if (mode === 'type+weight') return weight ? `${channel} (${weight})` : channel
  if (mode.startsWith('attr:')) {
    const key = mode.slice(5)
    const values = group
      .map((x) => (x.data('attrs') || {})[key])
      .filter((v) => attrKind(v) !== 'empty')
    return [...new Set(values.map(formatAttrValue))].join(', ')
  }
  return channel
}

export function applyEdgeLabels() {
  if (!document.getElementById('showEdgeLabels').checked) return
  const mode = document.getElementById('edgeLabelContent').value || 'type'
  cy.batch(() => {
    cy.edges().forEach((e) => {
      const text = edgeLabelText(e, mode)
      if (e.data('elabel') !== text) e.data('elabel', text)
    })
  })
}

// Keeps the "attribute" choices in the edge label list in step with the
// edge attributes of the loaded network.
export function refreshEdgeLabelOptions() {
  const sel = document.getElementById('edgeLabelContent')
  const current = sel.value
  ;[...sel.querySelectorAll('option[data-attr]')].forEach((o) => o.remove())
  Object.keys(attrSchema.edge || {}).forEach((key) => {
    const opt = new Option(`Text: attribute "${key}"`, 'attr:' + key)
    opt.dataset.attr = '1'
    sel.add(opt)
  })
  sel.value = [...sel.options].some((o) => o.value === current) ? current : 'type'
}

export function updateLabelStyle() {
  const showNode = document.getElementById('showNodeLabels').checked
  const showEdge = document.getElementById('showEdgeLabels').checked
  const pos = document.getElementById('labelPosition').value
  const nodePx = parseFloat(document.getElementById('nodeLabelSize').value) || 12
  const edgePx = parseFloat(document.getElementById('edgeLabelSize').value) || 9
  const minPx = parseFloat(document.getElementById('labelMinScreenSize').value) || 0
  document.getElementById('nodeLabelSizeValue').textContent = `${nodePx} px`
  document.getElementById('edgeLabelSizeValue').textContent = `${edgePx} px`
  document.getElementById('edgeLabelControls').hidden = !showEdge

  let valign = 'center',
    halign = 'center',
    marginY = 0,
    marginX = 0
  if (pos === 'top') {
    valign = 'top'
    marginY = -4
  } else if (pos === 'bottom') {
    valign = 'bottom'
    marginY = 4
  } else if (pos === 'left') {
    halign = 'left'
    marginX = -6
  } else if (pos === 'right') {
    halign = 'right'
    marginX = 6
  }

  applyLabelSizes()
  applyEdgeLabels()
  setStyle('node', {
    label: showNode ? 'data(id)' : '',
    'text-valign': valign,
    'text-halign': halign,
    'text-margin-y': marginY,
    'text-margin-x': marginX,
    'min-zoomed-font-size': minPx,
  })
  setStyle('edge', {
    label: showEdge ? 'data(elabel)' : '',
    'font-size': edgePx,
    'font-family': 'Inter, sans-serif',
    color: currentTheme.text,
    'text-background-color': currentTheme.bg,
    'text-background-opacity': 0.85,
    'text-background-padding': 2,
    'text-background-shape': 'roundrectangle',
    'text-rotation': document.getElementById('edgeLabelOrientation').value,
    'min-zoomed-font-size': minPx,
  })
  applyEdgeCurveStyle()
  applyLabelColors()
}

/* edge opacity */
export function applyEdgeOpacity() {
  const o = parseFloat(document.getElementById('edgeOpacity').value) || 0.85
  document.getElementById('edgeOpacityValue').textContent = o.toFixed(2)
  setStyle('edge', { opacity: o })
  setStyle('edge.dimmed', { opacity: Math.min(0.04, o) })
}

/* spread: nodes repel (right) or attract (left) each other.
   The slider sets the strength of repulsion (which acts between all nearby
   nodes) against the pull of the links, from a quarter to four times the
   current arrangement (the "base"). Each pair of nearby nodes, plus a few
   distant anchors, gets a target distance from the base: pushed apart,
   unlinked neighbours separate most and links resist, so linked groups
   stay together and move apart; drawn together, links shorten most and
   nodes stop before they overlap. Stress majorization moves the nodes
   toward those distances, keeping the arrangement. At 1x the targets are
   the base distances, and returning to 1x restores the base exactly.
   Dragging nodes, undo and layouts start a new base. */
let spreadApplied = 0

export let spreadBase = null
// { v0, ids, pos, out }
let spreadFrame = 0

function spreadFactor(v) {
  return Math.pow(2, v)
}

export function updateSpreadReadout() {
  const v = parseFloat(document.getElementById('spreadSlider').value) || 0
  document.getElementById('spreadValue').textContent = spreadFactor(v).toFixed(2) + '×'
}

// The positions the slider last produced, if the nodes are still there.
function spreadBaseValid(nodes) {
  const b = spreadBase
  if (!b || b.ids.length !== nodes.length) return false
  for (let i = 0; i < nodes.length; i++) {
    const n = nodes[i]
    if (n.id() !== b.ids[i]) return false
    const p = n.position()
    if (Math.abs(p.x - b.out[2 * i]) > 1e-6 || Math.abs(p.y - b.out[2 * i + 1]) > 1e-6) return false
  }
  return true
}

// Nodes may come as close as they were in the base, but not overlap more.
function spreadSeparate(X, B, n, radii, passes) {
  const gap = 4
  const maxR = radii.reduce((a, r) => Math.max(a, r), 0)
  if (!(maxR > 0)) return
  const size = 2 * maxR + gap
  for (let pass = 0; pass < passes; pass++) {
    const grid = new Map()
    for (let i = 0; i < n; i++) {
      const key =
        (Math.floor(X[2 * i] / size) + 50000) * 100003 + Math.floor(X[2 * i + 1] / size) + 50000
      let c = grid.get(key)
      if (!c) grid.set(key, (c = []))
      c.push(i)
    }
    let moved = false
    grid.forEach((list, key) => {
      for (let ox = -1; ox <= 1; ox++)
        for (let oy = -1; oy <= 1; oy++) {
          const other = grid.get(key + ox * 100003 + oy)
          if (!other) continue
          for (const i of list)
            for (const j of other) {
              if (i >= j) continue
              let dx = X[2 * i] - X[2 * j],
                dy = X[2 * i + 1] - X[2 * j + 1]
              const d = Math.hypot(dx, dy)
              const want = Math.min(
                radii[i] + radii[j] + gap,
                Math.hypot(B[2 * i] - B[2 * j], B[2 * i + 1] - B[2 * j + 1])
              )
              if (d >= want - 1e-6) continue
              let len = d
              if (d < 1e-9) {
                dx = (i + j) % 2 ? 1 : -1
                dy = 0
                len = 1
              }
              const push = (want - d) / 2 / len
              X[2 * i] += dx * push
              X[2 * i + 1] += dy * push
              X[2 * j] -= dx * push
              X[2 * j + 1] -= dy * push
              moved = true
            }
        }
    })
    if (!moved) break
  }
}

// Target distances from the base: repulsion acts on every pair of nearby
// nodes and attraction only along links, so pushing apart stretches unlinked
// pairs most (links resist) and drawing together shortens links most
// (unlinked pairs follow less). Crowded pairs change most, distant pairs
// least. Stress majorization then moves the nodes toward those distances.
function spreadRelax(base, n, edges, radii, rel) {
  const B = base.pos
  if (n < 2) return B.slice()
  const dist = (i, j) => Math.hypot(B[2 * i] - B[2 * j], B[2 * i + 1] - B[2 * j + 1])
  let minX = Infinity,
    minY = Infinity,
    maxX = -Infinity,
    maxY = -Infinity
  for (let i = 0; i < n; i++) {
    minX = Math.min(minX, B[2 * i])
    maxX = Math.max(maxX, B[2 * i])
    minY = Math.min(minY, B[2 * i + 1])
    maxY = Math.max(maxY, B[2 * i + 1])
  }
  const cell = Math.max(Math.sqrt(Math.max((maxX - minX) * (maxY - minY), 1) / n) * 2, 1e-6)
  const cellOf = (i) => [
    Math.floor((B[2 * i] - minX) / cell),
    Math.floor((B[2 * i + 1] - minY) / cell),
  ]
  const cells = new Map()
  for (let i = 0; i < n; i++) {
    const [gx, gy] = cellOf(i),
      key = gx * 100003 + gy
    let c = cells.get(key)
    if (!c) cells.set(key, (c = []))
    c.push(i)
  }
  const NN = 12,
    FAR = 12
  const pairs = new Map()
  const addPair = (i, j, linked) => {
    if (i === j) return
    const a = Math.min(i, j),
      b = Math.max(i, j),
      key = a * n + b
    const had = pairs.get(key)
    if (had) {
      if (linked) had[2] = true
      return
    }
    pairs.set(key, [a, b, linked])
  }
  for (let e = 0; e < edges.length; e += 2) addPair(edges[e], edges[e + 1], true)
  const nnDist = []
  for (let i = 0; i < n; i++) {
    const [gx, gy] = cellOf(i)
    let cand = []
    for (let r = 1; r <= 4 && cand.length < NN; r++) {
      cand = []
      for (let ox = -r; ox <= r; ox++)
        for (let oy = -r; oy <= r; oy++) {
          const c = cells.get((gx + ox) * 100003 + gy + oy)
          if (c) for (const j of c) if (j !== i) cand.push(j)
        }
    }
    cand.sort((a, b) => dist(i, a) - dist(i, b))
    cand.slice(0, NN).forEach((j) => addPair(i, j, false))
    if (cand.length && dist(i, cand[0]) > 1e-9) nnDist.push(dist(i, cand[0]))
    let seed = (i * 2654435761) >>> 0
    for (let f = 0; f < FAR; f++) {
      seed = (seed * 1664525 + 1013904223) >>> 0
      addPair(i, seed % n, false)
    }
  }
  nnDist.sort((a, b) => a - b)
  const k = nnDist.length ? nnDist[nnDist.length >> 1] * 2 : 50

  const lr = Math.log(rel)
  const P = []
  pairs.forEach(([i, j, linked]) => {
    const d0 = dist(i, j)
    if (d0 < 1e-9) return
    const ratio = d0 / k
    const near = ratio <= 1 ? 1 : ratio >= 12 ? 0.08 : 1 - (0.92 * Math.log(ratio)) / Math.log(12)
    const follows = rel >= 1 ? (linked ? 0.6 : 1) : linked ? 1 : 0.6
    let t = d0 * Math.exp(lr * near * follows)
    if (rel < 1) t = Math.max(t, Math.min(d0, radii[i] + radii[j] + 4))
    P.push(i, j, t)
  })
  const X = B.slice()
  const iters = n > 2500 ? 30 : n > 800 ? 45 : 70
  const nx = new Float64Array(2 * n),
    ws = new Float64Array(n)
  let cx = 0,
    cyy = 0
  for (let i = 0; i < n; i++) {
    cx += B[2 * i]
    cyy += B[2 * i + 1]
  }
  cx /= n
  cyy /= n
  for (let it = 0; it < iters; it++) {
    nx.fill(0)
    ws.fill(0)
    for (let p = 0; p < P.length; p += 3) {
      const i = P[p],
        j = P[p + 1],
        t = P[p + 2]
      let dx = X[2 * i] - X[2 * j],
        dy = X[2 * i + 1] - X[2 * j + 1]
      let d = Math.hypot(dx, dy)
      if (d < 1e-9) {
        dx = 1e-3
        dy = 0
        d = 1e-3
      }
      const w = 1 / t
      const ux = (dx / d) * t,
        uy = (dy / d) * t
      nx[2 * i] += w * (X[2 * j] + ux)
      nx[2 * i + 1] += w * (X[2 * j + 1] + uy)
      ws[i] += w
      nx[2 * j] += w * (X[2 * i] - ux)
      nx[2 * j + 1] += w * (X[2 * i + 1] - uy)
      ws[j] += w
    }
    for (let i = 0; i < n; i++) {
      if (ws[i] > 0) {
        X[2 * i] = nx[2 * i] / ws[i]
        X[2 * i + 1] = nx[2 * i + 1] / ws[i]
      }
    }
    spreadSeparate(X, B, n, radii, 6)
  }
  spreadSeparate(X, B, n, radii, 150)
  let mx = 0,
    my = 0
  for (let i = 0; i < n; i++) {
    mx += X[2 * i]
    my += X[2 * i + 1]
  }
  mx = mx / n - cx
  my = my / n - cyy
  for (let i = 0; i < n; i++) {
    X[2 * i] -= mx
    X[2 * i + 1] -= my
  }
  return X
}

function applySpread() {
  const v = parseFloat(document.getElementById('spreadSlider').value) || 0
  updateSpreadReadout()
  const nodes = cy.nodes()
  if (!nodes.length) {
    spreadApplied = v
    return
  }
  if (!spreadBaseValid(nodes)) {
    const ids = nodes.map((nd) => nd.id())
    const pos = new Float64Array(2 * nodes.length)
    nodes.forEach((nd, i) => {
      const p = nd.position()
      pos[2 * i] = p.x
      pos[2 * i + 1] = p.y
    })
    spreadBase = { v0: spreadApplied, ids, pos, out: pos }
  }
  spreadApplied = v
  const b = spreadBase
  const rel = spreadFactor(v - b.v0)
  let out
  if (Math.abs(v - b.v0) < 1e-9) out = b.pos
  else {
    const index = new Map(b.ids.map((id, i) => [id, i]))
    const seen = new Set(),
      edges = []
    cy.edges().forEach((e) => {
      const i = index.get(e.source().id()),
        j = index.get(e.target().id())
      if (i === undefined || j === undefined || i === j) return
      const key = i < j ? i * nodes.length + j : j * nodes.length + i
      if (seen.has(key)) return
      seen.add(key)
      edges.push(i, j)
    })
    const radii = nodes.map((nd) => (nd.width() || 0) / 2)
    out = spreadRelax(b, nodes.length, edges, radii, rel)
  }
  b.out = out
  cy.batch(() => {
    nodes.forEach((nd, i) => nd.position({ x: out[2 * i], y: out[2 * i + 1] }))
  })
  drawGroupHulls()
}

// Large networks take a moment per step, so they follow the slider when it
// is released rather than while it is dragged.
const SPREAD_LIVE_MAX = 1500

function queueSpread() {
  updateSpreadReadout()
  if (spreadFrame || cy.nodes().length > SPREAD_LIVE_MAX) return
  spreadFrame = requestAnimationFrame(() => {
    spreadFrame = 0
    applySpread()
  })
}

function resetSpread() {
  spreadApplied = 0
  spreadBase = null
  document.getElementById('spreadSlider').value = 0
  updateSpreadReadout()
}

/* edge curve style (straight / curved / bundled) */
export const BUNDLE_MAX_EDGES = 10000

const BUNDLED_STYLE_PROPS =
  'curve-style control-point-distances control-point-weights edge-distances'

let bundleSeq = 0

let bundleTimer = null

let bundleSignature = ''

function bundlingOn() {
  return document.getElementById('edgeCurveStyle').value === 'bundled'
}

function clearBundles() {
  bundleSignature = ''
  lastBundle = null
  cy.batch(() =>
    cy.edges('.bundled').forEach((e) => {
      e.removeStyle(BUNDLED_STYLE_PROPS)
      e.removeClass('bundled')
    })
  )
}

export function applyEdgeCurveStyle() {
  const curveStyle = document.getElementById('edgeCurveStyle').value
  const curvature = parseInt(document.getElementById('edgeCurvature').value, 10) || 40
  document.getElementById('curvatureValue').textContent = curvature
  document.getElementById('curvatureRow').style.display = curveStyle === 'bezier' ? 'block' : 'none'
  document.getElementById('bundleRow').hidden = curveStyle !== 'bundled'
  document.getElementById('bundleStrengthValue').textContent =
    document.getElementById('bundleStrength').value + '%'

  // Cytoscape can't draw labels on haystack edges; with edge labels on,
  // straight edges use the (equally straight) 'straight' style instead.
  // Bundled edges start straight and are bent individually once computed.
  const labelsOn = document.getElementById('showEdgeLabels').checked
  let effective = curveStyle
  if (curveStyle === 'bundled') effective = 'straight'
  else if (
    curveStyle === 'haystack' &&
    (labelsOn || (typeof arrowsShown === 'function' && arrowsShown()))
  )
    effective = 'straight'
  setStyle('edge', {
    'curve-style': effective,
    'control-point-step-size': curvature,
    'haystack-radius': 0.4,
  })

  if (curveStyle === 'bundled') scheduleBundling(true)
  else {
    bundleSeq++
    clearBundles()
    setStatus('bundleStatus', [])
  }
}

// Recomputes bundles shortly after the drawing changes. Repeated triggers
// within the delay collapse into one run.
export function scheduleBundling(force) {
  if (!bundlingOn()) return
  if (force) bundleSignature = ''
  clearTimeout(bundleTimer)
  bundleTimer = setTimeout(runBundling, 250)
}

// The last computed bundles, kept so they can be re-applied cheaply when
// node sizes change (which changes which bend points fall inside nodes).
var lastBundle = null

// Bends each edge through its bundled points. Points that fall inside the
// source or target node are left out: Cytoscape can't find where such an
// edge leaves its node and silently skips drawing it.
export function applyBundleResult() {
  if (!lastBundle || !bundlingOn()) return
  const { list, segs, result } = lastBundle
  const keep = new Set(list.map((e) => e.id()))
  cy.batch(() => {
    cy.edges('.bundled').forEach((e) => {
      if (!keep.has(e.id())) {
        e.removeStyle(BUNDLED_STYLE_PROPS)
        e.removeClass('bundled')
      }
    })
    list.forEach((e, k) => {
      const pts = result[k]
      if (!pts || e.removed()) return
      const s = segs[k]
      const vx = s.tx - s.sx,
        vy = s.ty - s.sy
      const L2 = vx * vx + vy * vy
      if (L2 < 1e-9) return
      const L = Math.sqrt(L2)
      const srcR = e.source().outerWidth() / 2 + 3
      const tgtR = e.target().outerWidth() / 2 + 3
      const weights = [],
        distances = []
      for (let i = 0; i < pts.length; i += 2) {
        if (Math.hypot(pts[i] - s.sx, pts[i + 1] - s.sy) <= srcR) continue
        if (Math.hypot(pts[i] - s.tx, pts[i + 1] - s.ty) <= tgtR) continue
        const px = pts[i] - s.sx,
          py = pts[i + 1] - s.sy
        // rounded: Cytoscape's style parser rejects exponent notation (1e-15)
        weights.push(
          Math.round(Math.min(0.999, Math.max(0.001, (px * vx + py * vy) / L2)) * 1000) / 1000
        )
        // Cytoscape measures positive distances to the left of source->target
        distances.push(Math.round(((py * vx - px * vy) / L) * 100) / 100)
      }
      if (!weights.length || distances.every((d) => d === 0)) {
        if (e.hasClass('bundled')) {
          e.removeStyle(BUNDLED_STYLE_PROPS)
          e.removeClass('bundled')
        }
        return
      }
      e.style({
        'curve-style': 'unbundled-bezier',
        'edge-distances': 'node-position',
        'control-point-weights': weights,
        'control-point-distances': distances,
      })
      e.addClass('bundled')
    })
  })
}

async function runBundling() {
  if (!bundlingOn()) return
  const edges = cy.edges(':visible').filter((e) => e.data('source') !== e.data('target'))
  const strength = (parseFloat(document.getElementById('bundleStrength').value) || 50) / 100
  const signature =
    strength +
    '|' +
    edges
      .map((e) => {
        const a = e.source().position(),
          b = e.target().position()
        return (
          e.id() +
          ':' +
          Math.round(a.x) +
          ',' +
          Math.round(a.y) +
          ',' +
          Math.round(b.x) +
          ',' +
          Math.round(b.y)
        )
      })
      .join(';')
  if (signature === bundleSignature) return
  if (edges.length > BUNDLE_MAX_EDGES) {
    clearBundles()
    setStatus('bundleStatus', [
      {
        level: 'warn',
        text: `Bundling handles up to ${BUNDLE_MAX_EDGES.toLocaleString()} visible edges; this view shows ${edges.length.toLocaleString()}. Untick some channels or groups, or merge parallel edges.`,
      },
    ])
    return
  }
  const run = ++bundleSeq
  setStatus('bundleStatus', edges.length > 300 ? [{ level: 'busy', text: 'Bundling edges…' }] : [])
  const list = edges.toArray()
  const segs = list.map((e) => {
    const a = e.source().position(),
      b = e.target().position()
    return { sx: a.x, sy: a.y, tx: b.x, ty: b.y }
  })
  const result = await bundleAsync(segs, {
    threshold: 0.85 - 0.5 * strength,
    iterations: Math.round(30 + 60 * strength),
  })
  if (run !== bundleSeq || !result || !bundlingOn()) return
  bundleSignature = signature
  lastBundle = { list, segs, result }
  applyBundleResult()
  setStatus('bundleStatus', [])
}

/* edge thickness */
export function applyEdgeWidth() {
  const mode = document.getElementById('edgeWidthMode').value
  document.getElementById('edgeWidthFixedRow').style.display = mode === 'fixed' ? 'block' : 'none'
  document.getElementById('edgeWidthWeightRow').style.display = mode === 'weight' ? 'block' : 'none'

  if (mode === 'fixed') {
    const w = parseFloat(document.getElementById('edgeWidthFixed').value) || 2
    document.getElementById('edgeWidthFixedValue').textContent = w
    setStyle('edge', { width: w })
    setStyle('edge.highlighted', { width: w * 1.6 })
  } else {
    const minW = parseFloat(document.getElementById('edgeWidthMin').value) || 1
    const maxW = parseFloat(document.getElementById('edgeWidthMax').value) || 8
    const knownWeights = cy
      .edges()
      .map((e) => e.data('weight'))
      .filter((w) => typeof w === 'number' && !isNaN(w))
    const lo = knownWeights.length ? Math.min(...knownWeights) : 0
    const hi = knownWeights.length ? Math.max(...knownWeights) : 1
    cy.edges().forEach((e) => {
      const w = e.data('weight')
      const v = typeof w === 'number' && !isNaN(w) ? w : (lo + hi) / 2
      const width = hi === lo ? (minW + maxW) / 2 : minW + ((v - lo) / (hi - lo)) * (maxW - minW)
      e.data('edgeWidth', width)
    })
    setStyle('edge', { width: 'data(edgeWidth)' })
    setStyle('edge.highlighted', { width: 'data(edgeWidth)' })
  }
  applyEdgeMerge()
}

/* ============================================================
   VIEWPORT: unbounded zoom, but the network never leaves the frame
   Zoom is effectively unlimited (see minZoom/maxZoom). After every pan
   or zoom the view is nudged so that:
     - a network smaller than the canvas stays entirely inside it, and
     - a network larger than the canvas always covers it, so there is
       no panning off into empty space.
   The model-space bounding box of the visible nodes is cached and only
   recomputed when nodes move, appear or disappear, so the check itself
   is cheap enough to run on every viewport change.
   ============================================================ */
const VIEW_MARGIN = 40

var viewBoxCache = null

var clampingViewport = false

export function invalidateViewBox() {
  viewBoxCache = null
}

function visibleModelBox() {
  if (!viewBoxCache) {
    const nodes = cy.nodes(':visible')
    viewBoxCache = nodes.length
      ? nodes.boundingBox({ includeLabels: false, includeOverlays: false })
      : 'empty'
  }
  return viewBoxCache === 'empty' ? null : viewBoxCache
}

function axisCorrection(lo, hi, size) {
  const m = Math.min(VIEW_MARGIN, size / 4)
  if (hi - lo <= size - 2 * m) {
    if (lo < m) return m - lo
    if (hi > size - m) return size - m - hi
    return 0
  }
  if (lo > m) return m - lo
  if (hi < size - m) return size - m - hi
  return 0
}

function clampViewport() {
  if (clampingViewport || cy.animated()) return
  const W = cy.width(),
    H = cy.height()
  if (!W || !H) return
  const bb = visibleModelBox()
  if (!bb) return
  const z = cy.zoom(),
    pan = cy.pan()
  const dx = axisCorrection(bb.x1 * z + pan.x, bb.x2 * z + pan.x, W)
  const dy = axisCorrection(bb.y1 * z + pan.y, bb.y2 * z + pan.y, H)
  if (Math.abs(dx) > 0.5 || Math.abs(dy) > 0.5) {
    clampingViewport = true
    cy.panBy({ x: dx, y: dy })
    clampingViewport = false
  }
}

export function formatZoom(z) {
  const pct = z * 100
  if (pct >= 1e5 || pct < 0.01) return pct.toExponential(0).replace('e+', 'e') + '%'
  if (pct >= 100) return Math.round(pct).toLocaleString() + '%'
  if (pct >= 1) return pct.toFixed(pct < 10 ? 1 : 0) + '%'
  return pct.toPrecision(2) + '%'
}

export function updateZoomReadout() {
  document.getElementById('zoomLevel').textContent = formatZoom(cy.zoom())
}

function zoomAroundCenter(factor) {
  const level = Math.min(cy.maxZoom(), Math.max(cy.minZoom(), cy.zoom() * factor))
  cy.zoom({ level, renderedPosition: { x: cy.width() / 2, y: cy.height() / 2 } })
}

export function fitView(eles, padding = 40) {
  if (typeof net3d !== 'undefined' && net3d.active) {
    fit3d(eles)
    return
  }
  const target = eles && eles.length ? eles : cy.elements(':visible')
  if (!target.length) return
  cy.animate(
    { fit: { eles: target, padding } },
    { duration: 400, easing: 'ease-out', complete: clampViewport }
  )
}

/* ============================================================
   NODE SEARCH
   Matches node names (and, optionally, attribute values) by substring,
   exact name, prefix, suffix or regular expression. A regular expression
   may be written plainly (^Rp[LS]\d+$) or with slashes and flags
   (/^rp[ls]/i). Enter zooms to the matches; Escape clears.
   ============================================================ */
var searchMatches = null

var searchTimer = null

function attrValueStrings(attrs) {
  const out = []
  Object.values(attrs || {}).forEach((v) => {
    if (Array.isArray(v)) v.forEach((x) => out.push(formatAttrValue(x)))
    else if (attrKind(v) !== 'empty') out.push(formatAttrValue(v))
  })
  return out
}

// Returns { test(str) -> bool } or throws with a readable message.
function buildSearchMatcher(query, mode, matchCase) {
  if (mode === 'regex') {
    let pattern = query,
      flags = ''
    const slashed = query.match(/^\/(.*)\/([a-z]*)$/s)
    if (slashed) {
      pattern = slashed[1]
      flags = slashed[2]
    }
    flags = flags.replace(/[gy]/g, '') // stateful flags would skip matches
    if (!matchCase && !flags.includes('i')) flags += 'i'
    let re
    try {
      re = new RegExp(pattern, flags)
    } catch (err) {
      // browsers phrase this as "Invalid regular expression: /pattern/flags: reason"
      const reason = err.message.split(': ').pop()
      throw new Error(
        `Not a valid regular expression (${reason.charAt(0).toLowerCase() + reason.slice(1)}). Check brackets and escape special characters with \\.`
      )
    }
    return { test: (str) => re.test(str) }
  }
  const q = matchCase ? query : query.toLowerCase()
  const norm = (str) => (matchCase ? str : str.toLowerCase())
  if (mode === 'exact') return { test: (str) => norm(str) === q }
  if (mode === 'prefix') return { test: (str) => norm(str).startsWith(q) }
  if (mode === 'suffix') return { test: (str) => norm(str).endsWith(q) }
  return { test: (str) => norm(str).includes(q) }
}

function setSearchStatus(text, isError) {
  const el = document.getElementById('searchStatus')
  el.textContent = text
  el.classList.toggle('error', !!isError)
  const badge = document.getElementById('searchBadge')
  if (badge) badge.textContent = isError ? '!' : text
  document.getElementById('search').setAttribute('aria-invalid', isError ? 'true' : 'false')
}

function clearSearchHighlight() {
  searchMatches = null
  cy.elements().removeClass('dimmed highlighted')
}

function runSearch() {
  const input = document.getElementById('search')
  const mode = document.getElementById('searchMode').value
  // keep surrounding spaces in regex and exact modes; they may be intended
  const query = mode === 'regex' || mode === 'exact' ? input.value : input.value.trim()
  if (!query) {
    clearSearchHighlight()
    setSearchStatus('', false)
    return
  }
  let matcher
  try {
    matcher = buildSearchMatcher(query, mode, document.getElementById('searchCase').checked)
  } catch (err) {
    clearSearchHighlight()
    setSearchStatus(err.message, true)
    return
  }
  const withAttrs = document.getElementById('searchAttrs').checked
  const candidates = cy.nodes().filter((n) => !n.hasClass('hidden-group'))
  const matched = candidates.filter(
    (n) =>
      matcher.test(n.id()) ||
      (withAttrs && attrValueStrings(n.data('attrs')).some((v) => matcher.test(v)))
  )
  searchMatches = matched
  cy.batch(() => {
    cy.elements().removeClass('highlighted').addClass('dimmed')
    matched.removeClass('dimmed').addClass('highlighted')
    matched.connectedEdges().removeClass('dimmed')
  })
  setSearchStatus(
    matched.length
      ? `${matched.length.toLocaleString()} of ${candidates.length.toLocaleString()}`
      : 'No matches',
    false
  )
}

function scheduleSearch() {
  clearTimeout(searchTimer)
  searchTimer = setTimeout(runSearch, cy.nodes().length > 2000 ? 200 : 60)
}

/* node click -> info panel */
export function hideInfo() {
  document.getElementById('infopanel').classList.remove('show')
}

function setInfoDesc(text) {
  const el = document.getElementById('infoDesc')
  el.textContent = text || ''
  el.style.display = text ? 'block' : 'none'
}

/* group inspector: reuses the node info panel for a whole group */
export function showGroupInfo(g) {
  hideEdgePopup()
  const members = cy.nodes().filter((n) => (n.data('groups') || []).includes(g))
  const memberIds = new Set(members.map((n) => n.id()))

  let internalEdges = 0,
    boundaryEdges = 0
  const internalPairs = new Set()
  const internal = cy.collection()
  cy.edges().forEach((e) => {
    const s = e.data('source'),
      t = e.data('target')
    const inS = memberIds.has(s),
      inT = memberIds.has(t)
    if (inS && inT) {
      internalEdges++
      internal.merge(e)
      if (s !== t) internalPairs.add(s < t ? s + '|' + t : t + '|' + s)
    } else if (inS || inT) {
      boundaryEdges++
    }
  })
  const k = members.length
  const density = k > 1 ? internalPairs.size / ((k * (k - 1)) / 2) : 0

  const overlaps = {}
  members.forEach((n) =>
    (n.data('groups') || []).forEach((o) => {
      if (o !== g) overlaps[o] = (overlaps[o] || 0) + 1
    })
  )
  const sharedCount = members.filter((n) => (n.data('groups') || []).length > 1).length

  cy.elements().removeClass('highlighted').addClass('dimmed')
  members.removeClass('dimmed').addClass('highlighted')
  internal.removeClass('dimmed')

  document.getElementById('infoDots').innerHTML =
    `<span class="nodedot" style="background:${S.nodeColorMap[g] || '#888'}"></span>`
  document.getElementById('infoName').textContent = groupLabel(g)
  document.getElementById('infoExplore').hidden = true
  document.getElementById('infoSub').textContent =
    groupLabel(g) === g ? `group · ${k} members` : `group "${g}" · ${k} members`
  setInfoDesc(groupDescription(g))

  const stat = (label, value) =>
    `<div style="display:flex; justify-content:space-between; color:var(--muted);">${label}<span style="color:var(--text);">${value}</span></div>`
  document.getElementById('infoMetrics').innerHTML =
    stat('Shared', sharedCount) +
    stat('Density', density.toFixed(3)) +
    stat('Inside', internalEdges) +
    stat('Outgoing', boundaryEdges)

  const custom = customGroupAttrs(g)
  const currentShape = S.groupShapes[g] || 'ellipse'
  document.getElementById('infoAttrs').innerHTML =
    '<div class="kvhead">Node shape for this group</div><div class="info-shapes" role="listbox" aria-label="Node shape">' +
    NODE_SHAPES.map(
      ([key, name]) =>
        `<button type="button" role="option" class="shape-opt" data-shape="${key}" aria-selected="${key === currentShape}" title="${name}" aria-label="${name}">${shapeSvg(key)}</button>`
    ).join('') +
    '</div>' +
    (Object.keys(custom).length
      ? '<div class="kvhead">Group attributes</div>' + attrRowsHtml(custom, 'kvrow')
      : '')
  document.querySelectorAll('#infoAttrs .shape-opt').forEach((btn) => {
    btn.addEventListener('click', () => {
      setGroupShape(g, btn.dataset.shape)
      document
        .querySelectorAll('#infoAttrs .shape-opt')
        .forEach((b) => b.setAttribute('aria-selected', b === btn ? 'true' : 'false'))
    })
  })

  const overlapEntries = Object.entries(overlaps).sort((a, b) =>
    byName(groupLabel(a[0]), groupLabel(b[0]))
  )
  const connEl = document.getElementById('infoConns')
  connEl.innerHTML = overlapEntries.length
    ? '<div class="kvhead" style="font-size:11px; font-family:var(--mono);">Shares members with</div>' +
      overlapEntries
        .map(
          ([o, c]) => `
        <div class="conn jump" data-group="${escapeHtml(o)}" role="button" tabindex="0">
          <span>${escapeHtml(groupLabel(o))}</span><span class="ename">${c} ${c === 1 ? 'node' : 'nodes'}</span>
        </div>`
        )
        .join('')
    : '<div class="kvhead" style="font-size:11px; font-family:var(--mono);">No members shared with other groups</div>'
  connEl.querySelectorAll('.conn.jump').forEach((row) => {
    const go = () => showGroupInfo(row.dataset.group)
    row.addEventListener('click', go)
    row.addEventListener('keydown', (ev) => {
      if (ev.key === 'Enter' || ev.key === ' ') {
        ev.preventDefault()
        go()
      }
    })
  })

  document.getElementById('infopanel').classList.add('show')
}

/* edge click -> popup */
export function hideEdgePopup() {
  document.getElementById('edgePopup').classList.remove('show')
}

/* ============================================================
   GROUP SHAPES
   Each group can have a node shape. A node takes its group's shape only
   when exactly one of its groups is currently ticked; nodes showing two
   or more active groups stay circular so their pie slices remain
   readable. Unticking groups therefore changes which shape a node shows.
   ============================================================ */
function polyPoints(n, r, rot = -Math.PI / 2, cx = 8, cy = 8) {
  return Array.from({ length: n }, (_, i) => {
    const a = rot + (2 * Math.PI * i) / n
    return `${(cx + r * Math.cos(a)).toFixed(2)},${(cy + r * Math.sin(a)).toFixed(2)}`
  }).join(' ')
}

function starPoints() {
  return Array.from({ length: 10 }, (_, i) => {
    const a = -Math.PI / 2 + (Math.PI * i) / 5
    const r = i % 2 ? 3 : 7
    return `${(8 + r * Math.cos(a)).toFixed(2)},${(8.6 + r * Math.sin(a)).toFixed(2)}`
  }).join(' ')
}

// [cytoscape shape, name, svg markup inside a 16x16 box]
var NODE_SHAPES = [
  ['ellipse', 'Circle', '<circle cx="8" cy="8" r="6.5"/>'],
  ['rectangle', 'Square', '<rect x="2" y="2" width="12" height="12"/>'],
  ['round-rectangle', 'Rounded square', '<rect x="2" y="2" width="12" height="12" rx="3.5"/>'],
  ['diamond', 'Diamond', '<polygon points="8,1 15,8 8,15 1,8"/>'],
  ['triangle', 'Triangle', '<polygon points="8,1.5 15,14 1,14"/>'],
  ['vee', 'Vee', '<polygon points="1,2 8,6.5 15,2 8,15"/>'],
  ['pentagon', 'Pentagon', `<polygon points="${polyPoints(5, 7, -Math.PI / 2, 8, 8.6)}"/>`],
  ['hexagon', 'Hexagon', `<polygon points="${polyPoints(6, 7, 0)}"/>`],
  ['octagon', 'Octagon', `<polygon points="${polyPoints(8, 7, Math.PI / 8)}"/>`],
  ['star', 'Star', `<polygon points="${starPoints()}"/>`],
  ['rhomboid', 'Rhomboid', '<polygon points="4.5,2 15,2 11.5,14 1,14"/>'],
  ['tag', 'Tag', '<polygon points="1,2 10.5,2 15,8 10.5,14 1,14"/>'],
  ['concave-hexagon', 'Concave hexagon', '<polygon points="1,2 15,2 12,8 15,14 1,14 4,8"/>'],
  ['barrel', 'Barrel', '<path d="M3,2 Q1,8 3,14 L13,14 Q15,8 13,2 Z"/>'],
]

export var SHAPE_BY_KEY = Object.fromEntries(NODE_SHAPES.map((s) => [s[0], s]))

export function shapeSvg(key) {
  const s = SHAPE_BY_KEY[key] || SHAPE_BY_KEY.ellipse
  return `<svg viewBox="0 0 16 16" aria-hidden="true" focusable="false">${s[2]}</svg>`
}

// The shape a node should show given its currently active groups.
export function nodeShapeFor(effectiveGroups) {
  return effectiveGroups.length === 1 ? S.groupShapes[effectiveGroups[0]] || 'ellipse' : 'ellipse'
}

function setGroupShape(g, shape) {
  if (!shape || shape === 'ellipse') delete S.groupShapes[g]
  else S.groupShapes[g] = shape
  cy.batch(() => cy.nodes().forEach((n) => refreshNodeVisual(n)))
  buildGroupLegend()
}

function assignDistinctShapes() {
  const choices = NODE_SHAPES.map((s) => s[0]).filter((k) => k !== 'ellipse')
  const groups = typeof filteredGroups === 'function' ? filteredGroups() : getUsedGroups()
  groups.forEach((g, i) => {
    S.groupShapes[g] = choices[i % choices.length]
  })
  cy.batch(() => cy.nodes().forEach((n) => refreshNodeVisual(n)))
  buildGroupLegend()
}

function resetShapes() {
  const groups = typeof filteredGroups === 'function' ? filteredGroups() : getUsedGroups()
  groups.forEach((g) => {
    delete S.groupShapes[g]
  })
  cy.batch(() => cy.nodes().forEach((n) => refreshNodeVisual(n)))
  buildGroupLegend()
}

/* shape chooser popover, shared by all group rows */
function positionShapePopover() {
  const pop = document.getElementById('shapePopover')
  if (pop.hidden || !shapePopoverAnchor) return
  const r = shapePopoverAnchor.getBoundingClientRect()
  const side = document.getElementById('sidebar').getBoundingClientRect()
  if (!document.body.contains(shapePopoverAnchor) || r.bottom < side.top || r.top > side.bottom) {
    closeShapePopover(false)
    return
  }
  const w = pop.offsetWidth,
    h = pop.offsetHeight
  pop.style.left = Math.min(window.innerWidth - w - 8, Math.max(8, r.left)) + 'px'
  pop.style.top =
    (r.bottom + 4 + h > window.innerHeight ? Math.max(8, r.top - h - 4) : r.bottom + 4) + 'px'
}

export let shapePopoverGroup = null

let shapePopoverAnchor = null

export function closeShapePopover(returnFocus) {
  const pop = document.getElementById('shapePopover')
  if (pop.hidden) return
  pop.hidden = true
  if (returnFocus && shapePopoverAnchor && document.body.contains(shapePopoverAnchor))
    shapePopoverAnchor.focus()
  shapePopoverGroup = null
  shapePopoverAnchor = null
}

export function openShapePopover(g, anchor) {
  const pop = document.getElementById('shapePopover')
  shapePopoverGroup = g
  shapePopoverAnchor = anchor
  const current = S.groupShapes[g] || 'ellipse'
  pop.innerHTML =
    `<div class="shape-pop-title">Shape for ${escapeHtml(groupLabel(g))}</div><div class="shape-grid" role="listbox" aria-label="Node shape">` +
    NODE_SHAPES.map(
      ([key, name]) =>
        `<button type="button" role="option" class="shape-opt" data-shape="${key}" aria-selected="${key === current}" title="${name}" aria-label="${name}">${shapeSvg(key)}</button>`
    ).join('') +
    '</div>'
  pop.hidden = false
  positionShapePopover()
  const opts = [...pop.querySelectorAll('.shape-opt')]
  opts.forEach((btn, i) => {
    btn.addEventListener('click', () => {
      setGroupShape(g, btn.dataset.shape)
      closeShapePopover(false)
    })
    btn.addEventListener('keydown', (e) => {
      const cols = 7
      let j = i
      if (e.key === 'ArrowRight') j = i + 1
      else if (e.key === 'ArrowLeft') j = i - 1
      else if (e.key === 'ArrowDown') j = i + cols
      else if (e.key === 'ArrowUp') j = i - cols
      else if (e.key === 'Escape') {
        e.preventDefault()
        closeShapePopover(true)
        return
      } else return
      e.preventDefault()
      opts[(j + opts.length) % opts.length].focus()
    })
  })
  ;(pop.querySelector('[aria-selected="true"]') || opts[0]).focus()
}

/* ============================================================
   VIEWS
   A view is one network visualization with everything that belongs to
   it: the data on the canvas, which library files it was built from,
   every Display setting, node positions, zoom and pan, which groups and
   channels are ticked, and group and channel colors. The theme and the
   search options are app-wide and are not part of a view.
   Only the active view lives on the canvas; switching saves it into its
   record and rebuilds the other one from its record.
   ============================================================ */
const APP_WIDE_SETTINGS = new Set(['themeSelect', 'searchMode', 'searchCase', 'searchAttrs'])

export const views = []

export function viewSettings() {
  const cfg = getCurrentConfig()
  APP_WIDE_SETTINGS.forEach((k) => delete cfg[k])
  return cfg
}

// Writes settings into the controls without side effects; callers refresh.
export function setControls(cfg) {
  CONFIG_FIELDS.forEach(({ id, prop }) => {
    if (!cfg || !(id in cfg) || APP_WIDE_SETTINGS.has(id)) return
    const el = document.getElementById(id)
    if (!el) return
    if (prop === 'checked') el.checked = !!cfg[id]
    else el.value = cfg[id]
  })
}

// The canvas contents in loadData's JSON shape (the same shape Save JSON writes).
export function snapshotData() {
  if (!cy.nodes().length) return null
  const nodeColors = {}
  Object.entries(S.nodeColorMap).forEach(([g, c]) => {
    if (g !== UNGROUPED) nodeColors[g] = c
  })
  const edgeColors = {}
  cy.edges().forEach((e) => {
    edgeColors[e.data('type')] = e.data('color')
  })
  return {
    nodes: cy.nodes().map((n) => ({
      id: n.id(),
      groups: (n.data('groups') || []).filter((g) => g !== UNGROUPED),
      size: n.data('baseSize'),
      ...(n.data('nodeColor') ? { color: n.data('nodeColor') } : {}),
      ...(n.data('values') ? { values: n.data('values') } : {}),
      ...(n.data('attrs') || {}),
    })),
    edges: cy.edges().map((e) => {
      const out = {
        id: e.id(),
        source: e.data('source'),
        target: e.data('target'),
        type: e.data('type'),
      }
      if (typeof e.data('weight') === 'number') out.weight = e.data('weight')
      if (e.data('directed')) out.directed = true
      return { ...out, ...(e.data('attrs') || {}) }
    }),
    nodeColors,
    edgeColors,
    groupOrder: getUsedGroups().filter((g) => g !== UNGROUPED),
    ...(Object.keys(S.groupShapes).length ? { groupShapes: { ...S.groupShapes } } : {}),
    ...(Object.keys(S.groupAttrs).length ? { groupAttrs: { ...S.groupAttrs } } : {}),
    ...(legendExtra ? { legendExtra } : {}),
  }
}

export function selectionFromKey(key) {
  return {
    networks: key && key.nets ? key.nets.split('|').filter(Boolean) : [],
    annotation: key ? key.annotation || '' : '',
    colors: key ? key.colors || '' : '',
  }
}

export function activeView() {
  return views.find((v) => v.id === S.activeViewId) || null
}

export function captureActiveView() {
  const v = activeView()
  if (!v) return
  flushHistory()
  cancelFrJobs()
  S.layoutRunSeq++
  const positions = {}
  cy.nodes().forEach((n) => {
    const p = n.position()
    positions[n.id()] = { x: p.x, y: p.y }
  })
  const typeColors = {}
  getUsedTypes().forEach((t) => {
    if (EDGE_TYPES[t]) typeColors[t] = EDGE_TYPES[t].color
  })
  v.data = snapshotData()
  v.selection = {
    networks: [...libSelection.networks],
    annotation: libSelection.annotation,
    colors: libSelection.colors,
  }
  v.state = {
    config: viewSettings(),
    positions,
    zoom: cy.zoom(),
    pan: { ...cy.pan() },
    activeGroups: [...S.activeGroups],
    activeTypes: [...S.activeTypes],
    groupColors: { ...S.nodeColorMap },
    typeColors,
    libView: S.currentLibView ? { ...S.currentLibView } : null,
    spread: parseFloat(document.getElementById('spreadSlider').value) || 0,
    groupFilter: document.getElementById('groupFilter').value,
    status: document.getElementById('normaStatus').innerHTML,
    view3d: capture3d(),
  }
}

export function clearCanvas() {
  bumpDataVersion()
  cancelFrJobs()
  S.layoutRunSeq++
  cy.elements().remove()
  document.getElementById('edgeLegend').innerHTML = ''
  document.getElementById('nodeGroupLegend').innerHTML = ''
  Object.keys(EDGE_TYPES).forEach((k) => {
    if (ORIGINAL_EDGE_TYPES[k]) EDGE_TYPES[k] = { ...ORIGINAL_EDGE_TYPES[k] }
    else delete EDGE_TYPES[k]
  })
  S.autoEdgeIdx = 0
  S.nodeColorMap = {}
  S.groupShapes = {}
  S.groupAttrs = {}
  S.activeGroups = new Set()
  invalidateFullMetrics()
  buildAttrSchema()
  renderAttrSchema()
  buildGroupLegend()
  updateStats()
  hideInfo()
  hideEdgePopup()
  S.currentLibView = null
  drawGroupHulls()
}

// With reuseData, the network already on the canvas is kept and only the
// view's look and positions are put back (used by undo/redo).
export function restoreView(v, { reuseData = false } = {}) {
  const st = v.state || {}
  setControls(st.config || S.DEFAULT_VIEW_CONFIG)
  document.getElementById('groupFilter').value = st.groupFilter || ''
  if (reuseData) {
    cancelFrJobs()
    S.layoutRunSeq++
    setLayoutBusy(false)
  } else if (v.data) {
    loadData(v.data, { positions: st.positions || null })
  } else {
    clearCanvas()
  }
  // some choices (e.g. an edge attribute as label text) only exist once
  // the view's data is loaded, so apply the settings again
  if (v.data) setControls(st.config || S.DEFAULT_VIEW_CONFIG)
  refreshAllDerivedUI()
  if (v.data) {
    if (st.groupColors) {
      S.nodeColorMap = {}
      Object.entries(st.groupColors).forEach(([g, c]) => {
        const sc = sanitizeColor(c)
        if (sc) S.nodeColorMap[g] = sc
      })
    }
    if (st.typeColors) {
      Object.entries(st.typeColors).forEach(([t, c]) => {
        const sc = sanitizeColor(c)
        if (!sc) return
        if (EDGE_TYPES[t]) EDGE_TYPES[t].color = sc
        else EDGE_TYPES[t] = { color: sc, label: t }
      })
      cy.edges().forEach((e) => {
        const m = EDGE_TYPES[e.data('type')]
        if (m) e.data('color', m.color)
      })
    }
    const used = new Set(getUsedGroups())
    if (st.activeGroups) S.activeGroups = new Set(st.activeGroups.filter((g) => used.has(g)))
    const usedTypes = getUsedTypes()
    if (st.activeTypes) S.activeTypes = new Set(st.activeTypes.filter((t) => usedTypes.includes(t)))
    buildGroupLegend()
    buildLegend(usedTypes)
    applyGroupVisibility()
    applyTypeVisibility()
    if (reuseData && st.positions) {
      cy.batch(() =>
        cy.nodes().forEach((n) => {
          const p = st.positions[n.id()]
          if (p) n.position(p)
        })
      )
      invalidateViewBox()
    }
    if (!reuseData && st.positions && typeof st.zoom === 'number') {
      cy.viewport({ zoom: st.zoom, pan: st.pan })
    }
  }
  restore3d(st.view3d)
  spreadApplied = st.spread || 0
  spreadBase = st.spreadBase ? { ...st.spreadBase } : null
  document.getElementById('spreadSlider').value = spreadApplied
  updateSpreadReadout()

  const sel = v.selection || { networks: [], annotation: '', colors: '' }
  libSelection.networks = new Set(sel.networks.filter((id) => libEntry('network', id)))
  libSelection.annotation = libEntry('annotation', sel.annotation) ? sel.annotation : ''
  libSelection.colors = libEntry('colors', sel.colors) ? sel.colors : ''
  S.currentLibView = st.libView || null
  renderLibraryLists()
  if (!reuseData) document.getElementById('normaStatus').innerHTML = st.status || ''
  updateStats()
  invalidateViewBox()
  drawGroupHulls()
  cy.elements().removeClass('dimmed highlighted')
  if (document.getElementById('search').value) runSearch()
  if (v.needsRefresh && !reuseData) refreshAfterDelete(v)
}

export function uniqueViewName(base) {
  const taken = new Set(views.map((v) => v.name))
  if (!taken.has(base)) return base
  let n = 2
  while (taken.has(`${base} ${n}`)) n++
  return `${base} ${n}`
}

export function renderViewBar() {
  const sel = document.getElementById('viewSelect')
  sel.innerHTML = ''
  views.forEach((v) => sel.add(new Option(v.name, v.id)))
  sel.value = S.activeViewId
  document.getElementById('viewCount').textContent =
    views.length > 1 ? `${views.length} views open` : ''
  if (typeof updateContextInfo === 'function') updateContextInfo()
}

// Creates a view and makes it active. With `copy`, it starts as a copy of
// the current view; otherwise it starts empty with default settings.
export function createView(name, { copy = false } = {}) {
  captureActiveView()
  const from = activeView()
  const v = {
    id: 'view' + ++S.viewSeq,
    name: uniqueViewName(name || 'Untitled view'),
    autoName: !name || name === 'Untitled view',
    data: copy && from ? from.data : null,
    selection:
      copy && from
        ? { ...from.selection, networks: [...from.selection.networks] }
        : { networks: [], annotation: '', colors: '' },
    state:
      copy && from
        ? JSON.parse(JSON.stringify(from.state))
        : { config: { ...S.DEFAULT_VIEW_CONFIG } },
  }
  views.push(v)
  S.activeViewId = v.id
  S.historySuspended++
  try {
    restoreView(v)
  } finally {
    S.historySuspended--
  }
  setHistoryBaseline()
  renderViewBar()
  return v
}

export function switchView(id) {
  if (id === S.activeViewId || !views.some((v) => v.id === id)) return
  captureActiveView()
  S.activeViewId = id
  S.historySuspended++
  try {
    restoreView(activeView())
  } finally {
    S.historySuspended--
  }
  S.dataCache = { version: dataVersion, data: activeView().data }
  setHistoryBaseline()
  renderViewBar()
  updateUndoButtons()
}

export function closeActiveView() {
  const idx = views.findIndex((v) => v.id === S.activeViewId)
  if (idx < 0) return
  views.splice(idx, 1)
  if (!views.length) {
    S.activeViewId = null
    createView('Untitled view')
    return
  }
  S.activeViewId = views[Math.max(0, idx - 1)].id
  S.historyGestureOpen = false
  clearTimeout(historyTimer)
  S.historySuspended++
  try {
    restoreView(activeView())
  } finally {
    S.historySuspended--
  }
  S.dataCache = { version: dataVersion, data: activeView().data }
  setHistoryBaseline()
  renderViewBar()
  updateUndoButtons()
}

// Runs `load` in a view of its own. An untouched empty view is reused
// rather than leaving it behind.
export function openInNewView(name, load) {
  const cur = activeView()
  if (cur && !cy.nodes().length && cur.autoName !== false && !cur.data) {
    cur.name = uniqueViewName(name)
    cur.autoName = false
    setControls(S.DEFAULT_VIEW_CONFIG)
    refreshAllDerivedUI()
  } else {
    createView(name)
    activeView().autoName = false
  }
  S.historySuspended++
  try {
    load()
  } finally {
    S.historySuspended--
  }
  setHistoryBaseline()
  renderViewBar()
  updateUndoButtons()
}

export function nameForSelection() {
  const nets = selectedNetworks()
  const ann = libEntry('annotation', libSelection.annotation)
  const base =
    nets.length === 1 ? nets[0].name : nets.length ? `${nets.length} networks` : 'Untitled view'
  return ann ? `${base}, ${ann.name}` : base
}

// page wiring, run by main.ts in the original order
export function init() {
  document.getElementById('btnProfile').addEventListener('click', runProfiler)

  document.getElementById('btnProfileTsv').addEventListener('click', profilerTsv)

  // statistic definitions in the Help tab come from the same table
  document.getElementById('helpStatsRows').innerHTML = PROFILE_STATS.map(
    (st) => `<tr><td>${escapeHtml(st.label)}</td><td>${escapeHtml(st.desc)}</td></tr>`
  ).join('')

  ;[
    'showNodeLabels',
    'showEdgeLabels',
    'labelPosition',
    'labelScaleWithNode',
    'edgeLabelContent',
    'edgeLabelOrientation',
    'labelMinScreenSize',
  ].forEach((id) => {
    document.getElementById(id).addEventListener('change', updateLabelStyle)
  })

  ;['nodeLabelSize', 'edgeLabelSize'].forEach((id) => {
    document.getElementById(id).addEventListener('input', updateLabelStyle)
  })

  document.getElementById('edgeOpacity').addEventListener('input', applyEdgeOpacity)

  document.getElementById('spreadSlider').addEventListener('input', queueSpread)

  document.getElementById('spreadSlider').addEventListener('change', () => {
    const v = parseFloat(document.getElementById('spreadSlider').value) || 0
    if (spreadFrame) {
      cancelAnimationFrame(spreadFrame)
      spreadFrame = 0
    }
    if (v !== spreadApplied || cy.nodes().length > SPREAD_LIVE_MAX) applySpread()
    scheduleBundling()
  })

  document.getElementById('spreadSlider').addEventListener('dblclick', () => {
    document.getElementById('spreadSlider').value = 0
    applySpread()
  })

  cy.on('layoutstart', resetSpread)

  document.getElementById('edgeCurveStyle').addEventListener('change', applyEdgeCurveStyle)

  document.getElementById('edgeCurvature').addEventListener('input', applyEdgeCurveStyle)

  document.getElementById('bundleStrength').addEventListener('input', () => {
    document.getElementById('bundleStrengthValue').textContent =
      document.getElementById('bundleStrength').value + '%'
  })

  document.getElementById('bundleStrength').addEventListener('change', () => scheduleBundling(true))

  cy.on('layoutstop dragfree', () => scheduleBundling())

  document.getElementById('edgeMergeMode').addEventListener('change', applyEdgeMerge)

  document.getElementById('edgeWidthMode').addEventListener('change', applyEdgeWidth)

  document.getElementById('edgeWidthFixed').addEventListener('input', applyEdgeWidth)

  document.getElementById('edgeWidthMin').addEventListener('change', applyEdgeWidth)

  document.getElementById('edgeWidthMax').addEventListener('change', applyEdgeWidth)

  /* node fill mode */
  document.getElementById('nodeFillSelect').addEventListener('change', () => {
    if (nodeFillMode() === 'values') applyValueColors()
    cy.batch(() => cy.nodes().forEach((n) => refreshNodeVisual(n)))
    scheduleLegend()
    updateContextInfo()
  })

  /* color palettes */
  document
    .getElementById('nodePaletteSelect')
    .addEventListener('change', (e) => applyNodePalette(e.target.value))

  document
    .getElementById('edgePaletteSelect')
    .addEventListener('change', (e) => applyEdgePalette(e.target.value))

  /* node size by centrality */
  ;['sizeMetric', 'sizeChannelOnly'].forEach((id) => {
    document.getElementById(id).addEventListener('change', applyNodeSizing)
  })

  document.getElementById('sizeMin').addEventListener('change', applyNodeSizing)

  document.getElementById('sizeMax').addEventListener('change', applyNodeSizing)

  document.getElementById('nodeScale').addEventListener('input', applyNodeSizing)

  cy.on('viewport', () => {
    clampViewport()
    updateZoomReadout()
  })

  cy.on('position add remove', invalidateViewBox)

  cy.on('style data', 'node', invalidateViewBox)

  cy.on('dragfree layoutstop', () => {
    invalidateViewBox()
    clampViewport()
  })

  window.addEventListener('resize', () => {
    invalidateViewBox()
    clampViewport()
  })

  document.getElementById('zoomIn').addEventListener('click', () => zoomAroundCenter(1.25))

  document.getElementById('zoomOut').addEventListener('click', () => zoomAroundCenter(0.8))

  document.getElementById('zoomFit').addEventListener('click', () => fitView())

  document.getElementById('search').addEventListener('input', scheduleSearch)

  ;['searchMode', 'searchCase', 'searchAttrs'].forEach((id) => {
    document.getElementById(id).addEventListener('change', runSearch)
  })

  document.getElementById('search').addEventListener('keydown', (e) => {
    if (e.key === 'Enter') {
      e.preventDefault()
      clearTimeout(searchTimer)
      runSearch()
      if (searchMatches && searchMatches.length) fitView(searchMatches, 80)
    } else if (e.key === 'Escape') {
      e.target.value = ''
      runSearch()
    }
  })

  document.getElementById('searchMode').addEventListener('change', (e) => {
    document.getElementById('search').placeholder =
      {
        contains: 'Find nodes',
        exact: 'Exact node name',
        prefix: 'Name starts with…',
        suffix: 'Name ends with…',
        regex: 'e.g. ^Rp[LS]\\d+  or  /kinase$/i',
      }[e.target.value] || 'Find nodes'
  })

  cy.on('tap', 'node', (evt) => {
    const node = evt.target
    hideEdgePopup()
    cy.elements().removeClass('highlighted')
    cy.elements().addClass('dimmed')
    node.removeClass('dimmed').addClass('highlighted')
    const neigh = node.closedNeighborhood()
    neigh.removeClass('dimmed')
    node.connectedEdges().addClass('highlighted')

    const panel = document.getElementById('infopanel')
    const groupsArr = node.data('groups') || [UNGROUPED]
    document.getElementById('infoDots').innerHTML = groupsArr
      .map((g) => `<span class="nodedot" style="background:${S.nodeColorMap[g] || '#888'}"></span>`)
      .join('')
    document.getElementById('infoName').textContent = node.data('id')
    const groupLinks = groupsArr
      .map(
        (g) =>
          `<button type="button" class="glink" data-group="${escapeHtml(g)}">${escapeHtml(groupLabel(g))}</button>`
      )
      .join(' + ')
    const subEl = document.getElementById('infoSub')
    subEl.innerHTML = `${groupLinks} · ${node.connectedEdges().length} edges · ${node.neighborhood('node').length} neighbors`
    subEl
      .querySelectorAll('.glink')
      .forEach((b) => b.addEventListener('click', () => showGroupInfo(b.dataset.group)))
    setInfoDesc('')

    const nid = node.id()
    const m = ensureFullMetrics()
    const nodeDirected = node.connectedEdges().some((e) => edgeIsDirected(e))
    document.getElementById('infoMetrics').innerHTML = `
    <div style="display:flex; justify-content:space-between; color:var(--muted);">Degree<span style="color:var(--text);">${m.degree[nid] || 0}</span></div>
    ${nodeDirected ? `<div style="display:flex; justify-content:space-between; color:var(--muted);">In / out<span style="color:var(--text);">${m.indegree[nid] || 0} / ${m.outdegree[nid] || 0}</span></div>` : ''}
    <div style="display:flex; justify-content:space-between; color:var(--muted);">Betweenness<span style="color:var(--text);">${(m.betweenness[nid] || 0).toFixed(3)}</span></div>
    <div style="display:flex; justify-content:space-between; color:var(--muted);">Closeness<span style="color:var(--text);">${(m.closeness[nid] || 0).toFixed(3)}</span></div>
    <div style="display:flex; justify-content:space-between; color:var(--muted);">Clustering<span style="color:var(--text);">${(m.clustering[nid] || 0).toFixed(3)}</span></div>
  `

    const attrs = {
      ...(node.data('nodeColor') ? { color: node.data('nodeColor') } : {}),
      ...(node.data('attrs') || {}),
    }
    document.getElementById('infoAttrs').innerHTML = Object.keys(attrs).length
      ? '<div class="kvhead">Node attributes</div>' + attrRowsHtml(attrs, 'kvrow')
      : ''

    const connsByNeighbor = {}
    node.connectedEdges().forEach((e) => {
      const outgoing = e.source().id() === node.id()
      const other = outgoing ? e.target().id() : e.source().id()
      connsByNeighbor[other] = connsByNeighbor[other] || []
      const meta = EDGE_TYPES[e.data('type')] || { label: e.data('type') }
      const arrow = edgeIsDirected(e) ? (outgoing ? '→ ' : '← ') : ''
      connsByNeighbor[other].push(arrow + meta.label)
    })
    const connEl = document.getElementById('infoConns')
    connEl.innerHTML =
      (nodeDirected
        ? '<div class="kvhead" style="font-size:11px;">→ outgoing · ← incoming</div>'
        : '') +
      Object.entries(connsByNeighbor)
        .sort((a, b) => byName(a[0], b[0]))
        .map(
          ([other, types]) => `
    <div class="conn"><span>${escapeHtml(other)}</span><span class="ename">${escapeHtml(types.join(', '))}</span></div>
  `
        )
        .join('')

    S.infoNodeId = node.id()
    document.getElementById('infoExplore').hidden = false
    panel.classList.add('show')
  })

  cy.on('tap', 'edge', (evt) => {
    const edge = evt.target
    hideInfo()
    cy.elements().removeClass('highlighted')
    cy.elements().addClass('dimmed')
    edge.removeClass('dimmed').addClass('highlighted')
    edge.connectedNodes().removeClass('dimmed')

    const meta = EDGE_TYPES[edge.data('type')] || {
      color: edge.data('color'),
      label: edge.data('type'),
    }
    document.getElementById('epDot').style.background = meta.color
    document.getElementById('epType').textContent = meta.label
    document.getElementById('epSource').textContent = edge.data('source')
    document.getElementById('epTarget').textContent = edge.data('target')
    document.getElementById('epId').textContent = edge.data('id')
    document.getElementById('epDirection').textContent = edgeIsDirected(edge)
      ? `${edge.data('source')} → ${edge.data('target')}`
      : 'undirected'

    const weight = edge.data('weight')
    const attrs = edge.data('attrs') || {}
    const rows = []
    if (typeof weight === 'number')
      rows.push(`<div class="eprow"><span>weight</span><span>${weight}</span></div>`)
    rows.push(attrRowsHtml(attrs, 'eprow'))
    const merged = edge.data('mergedWith')
    if (merged && merged.length > 1) {
      rows.push(
        `<div class="eprow"><span>Merged channels</span><span>${merged.length}</span></div>`
      )
      merged.forEach((id) => {
        const other = cy.getElementById(id)
        if (!other.length) return
        const m = EDGE_TYPES[other.data('type')] || { label: other.data('type') }
        const w = other.data('weight')
        rows.push(
          `<div class="eprow"><span style="display:flex; align-items:center; gap:6px;"><span class="epdot" style="width:9px; height:9px; background:${escapeHtml(other.data('color'))}"></span>${escapeHtml(m.label)}</span><span>${typeof w === 'number' ? escapeHtml(formatAttrValue(w)) : ''}</span></div>`
        )
      })
    }
    document.getElementById('epExtra').innerHTML = rows.join('')

    const canvasRect = document.getElementById('canvas').getBoundingClientRect()
    const pos = evt.renderedPosition || { x: canvasRect.width / 2, y: canvasRect.height / 2 }
    const popup = document.getElementById('edgePopup')
    const left = Math.min(pos.x + 14, canvasRect.width - 266)
    const top = Math.min(pos.y + 14, canvasRect.height - 180)
    popup.style.left = Math.max(10, left) + 'px'
    popup.style.top = Math.max(10, top) + 'px'
    popup.classList.add('show')
  })

  document.getElementById('edgePopupClose').addEventListener('click', () => {
    cy.elements().removeClass('dimmed highlighted')
    hideEdgePopup()
  })

  cy.on('tap', (evt) => {
    if (evt.target === cy) {
      cy.elements().removeClass('dimmed highlighted')
      hideInfo()
      hideEdgePopup()
    }
  })

  /* double-click to recenter */
  cy.on('dbltap', (evt) => {
    if (evt.target === cy) {
      fitView()
    }
  })

  cy.on('dbltap', 'node', (evt) => {
    cy.animate(
      { center: { eles: evt.target } },
      { duration: 300, easing: 'ease-out', complete: clampViewport }
    )
  })

  document.getElementById('infoClose').addEventListener('click', () => {
    cy.elements().removeClass('dimmed highlighted')
    hideInfo()
  })

  /* theme */
  document
    .getElementById('themeSelect')
    .addEventListener('change', (e) => applyTheme(e.target.value))

  /* group highlighting (hulls / fog clouds) */
  cy.on('render', drawGroupHulls)

  // The WebGL renderer does not emit 'render', so the shading follows the
  // changes that move or restyle nodes instead (once per frame).
  if (WEBGL_ACTIVE) {
    let hullQueued = false
    const queueHulls = () => {
      if (hullQueued) return
      hullQueued = true
      requestAnimationFrame(() => {
        hullQueued = false
        drawGroupHulls()
      })
    }
    cy.on('viewport position add remove data style class resize layoutstop', queueHulls)
  }

  document.getElementById('showGroupHulls').addEventListener('change', (e) => {
    document.getElementById('hullControls').style.display = e.target.checked ? 'block' : 'none'
    drawGroupHulls()
  })

  document.getElementById('hullStyle').addEventListener('change', drawGroupHulls)

  document.getElementById('hullOpacity').addEventListener('input', (e) => {
    document.getElementById('hullOpacityValue').textContent = e.target.value
    drawGroupHulls()
  })

  document.addEventListener('mousedown', (e) => {
    const pop = document.getElementById('shapePopover')
    if (!pop.hidden && !pop.contains(e.target) && !e.target.closest('.gshape'))
      closeShapePopover(false)
  })

  document.getElementById('sidebar').addEventListener('scroll', positionShapePopover)

  window.addEventListener('resize', positionShapePopover)

  document.getElementById('btnShapesAssign').addEventListener('click', assignDistinctShapes)

  document.getElementById('btnShapesReset').addEventListener('click', resetShapes)
}
