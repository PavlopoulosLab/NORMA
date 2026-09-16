// @ts-nocheck
// ponytail: split verbatim from the former single-file script; typed one file at a time (remove @ts-nocheck as it lands)
import { S } from './state'
import { UNGROUPED, escapeHtml } from './network_state'
import { currentTheme } from './themes'
import { edgeIsDirected } from './export/dialog'
import { libEntry, normaLibrary, plural, setStatus } from './layouts/controls'
import { profileDirected } from './directed_stats'
import { profileGraph, simpleGraph } from './wiring'
import { renderComparison } from './export/draw'
import { setStyle } from './cy'
import { shownEdges, shownNodes } from './metrics'
import { sortedByName } from './hulls'
import { updateLabelStyle, views } from './profiler'

/* ---------- label colors ---------- */
function isLightColor(hex) {
  const m = /^#?([0-9a-f]{6})$/i.exec(hex || '')
  if (!m) return false
  const n = parseInt(m[1], 16)
  const r = (n >> 16) & 255,
    g = (n >> 8) & 255,
    b = n & 255
  return (0.2126 * r + 0.7152 * g + 0.0722 * b) / 255 > 0.72
}

function chosenLabelColor(modeId, pickerId) {
  const mode = document.getElementById(modeId).value
  const picker = document.getElementById(pickerId)
  picker.hidden = mode !== 'custom'
  if (mode === 'custom') return picker.value
  return mode // 'auto', 'match' or a hex color
}

export function applyLabelColors() {
  const node = chosenLabelColor('nodeLabelColorMode', 'nodeLabelColor')
  const edge = chosenLabelColor('edgeLabelColorMode', 'edgeLabelColor')
  const theme = currentTheme
  // light text gets a dark halo so it stays readable on a light canvas
  const halo = (c) => (c !== 'auto' && c !== 'match' && isLightColor(c) ? '#1b2333' : theme.bg)
  setStyle('node', {
    color: node === 'auto' ? theme.text : node === 'match' ? 'data(labelColor)' : node,
    'text-outline-color': halo(node),
  })
  setStyle('edge', {
    color: edge === 'auto' ? theme.text : edge === 'match' ? 'data(color)' : edge,
    'text-background-color': isLightColor(edge) ? '#1b2333' : theme.bg,
  })
}

/* ============================================================
   NETWORK COMPARISON
   Up to ten networks, taken from open views or from the file library,
   compared by node and edge overlap (Venn diagrams for two or three
   networks, UpSet plots for any number), pairwise Jaccard similarity,
   degree agreement on shared nodes, degree distributions, and the
   profiler's topology statistics side by side. Nodes are matched by
   name; edges by their unordered node pair, ignoring channels.
   ============================================================ */
const COMPARE_MAX = 10

const COMPARE_COLORS = [
  '#2563eb',
  '#e0782b',
  '#16a34a',
  '#c026d3',
  '#0891b2',
  '#dc2626',
  '#7c3aed',
  '#ca8a04',
  '#db2777',
  '#475569',
]

export const UPSET_MAX_COLUMNS = 40

export const compareState = { results: null }

function compareSourceOptions() {
  const opts = []
  views.forEach((v) => {
    const n = v.id === S.activeViewId ? shownNodes().length : v.data ? v.data.nodes.length : 0
    if (n)
      opts.push({
        value: 'view:' + v.id,
        label: v.name,
        meta: `view, ticked groups and channels`,
        group: 'Open views',
      })
  })
  sortedByName(normaLibrary.network, (e) => e.name).forEach((e) => {
    opts.push({
      value: 'lib:' + e.id,
      label: e.name,
      meta: e.parsed.summary,
      group: 'Network files',
    })
  })
  return opts
}

export function renderCompareList() {
  const el = document.getElementById('cmpNetList')
  if (!el) return
  const previous = new Set([...el.querySelectorAll('input:checked')].map((i) => i.value))
  el.innerHTML = ''
  const opts = compareSourceOptions()
  if (!opts.length) {
    el.innerHTML = '<p class="list-empty">Open an example or upload networks first.</p>'
    updateCompareLimit()
    return
  }
  let lastGroup = null
  opts.forEach((o) => {
    if (o.group !== lastGroup) {
      const h = document.createElement('div')
      h.className = 'cmp-group-head'
      h.textContent = o.group
      el.appendChild(h)
      lastGroup = o.group
    }
    const label = document.createElement('label')
    label.className = 'cmp-option'
    const input = document.createElement('input')
    input.type = 'checkbox'
    input.value = o.value
    input.checked = previous.has(o.value)
    input.addEventListener('change', updateCompareLimit)
    const text = document.createElement('span')
    text.innerHTML = `<span class="cmp-name">${escapeHtml(o.label)}</span><span class="cmp-meta">${escapeHtml(o.meta)}</span>`
    label.append(input, text)
    el.appendChild(label)
  })
  updateCompareLimit()
}

function updateCompareLimit() {
  const boxes = [...document.querySelectorAll('#cmpNetList input')]
  const checked = boxes.filter((b) => b.checked)
  boxes.forEach((b) => {
    b.disabled = !b.checked && checked.length >= COMPARE_MAX
  })
  const btn = document.getElementById('btnCompare')
  btn.disabled = checked.length < 2
  document.getElementById('cmpCount').textContent =
    `${checked.length} of ${COMPARE_MAX} chosen` +
    (checked.length < 2 ? ' (choose at least 2)' : '')
}

function compareSource(value) {
  if (value.startsWith('view:')) {
    const v = views.find((x) => x.id === value.slice(5))
    if (!v) return null
    let ids, pairs
    if (v.id === S.activeViewId) {
      ids = shownNodes().map((n) => n.id())
      pairs = shownEdges(true).map((e) => [e.data('source'), e.data('target'), edgeIsDirected(e)])
    } else {
      if (!v.data) return null
      // the part that view shows: its ticked groups and channels
      const st = v.state || {}
      const groupsOn = st.activeGroups ? new Set(st.activeGroups) : null
      const typesOn = st.activeTypes ? new Set(st.activeTypes) : null
      const shown = new Set(
        v.data.nodes
          .filter((n) => {
            if (!groupsOn) return true
            const gs = n.groups && n.groups.length ? n.groups : n.group ? [n.group] : [UNGROUPED]
            return gs.some((g) => groupsOn.has(g))
          })
          .map((n) => n.id)
      )
      ids = [...shown]
      const mode = (st.config && st.config.edgeDirection) || 'data'
      pairs = v.data.edges
        .filter(
          (e) =>
            shown.has(e.source) &&
            shown.has(e.target) &&
            (!typesOn || typesOn.has(e.type || 'link'))
        )
        .map((e) => [
          e.source,
          e.target,
          mode === 'directed' ? true : mode === 'undirected' ? false : !!e.directed,
        ])
    }
    return { name: v.name, kind: 'view', ids, pairs }
  }
  const entry = libEntry('network', value.slice(4))
  if (!entry) return null
  return {
    name: entry.name,
    kind: 'file',
    ids: entry.parsed.nodes,
    pairs: entry.parsed.edges.map((e) => [e.source, e.target, !!e.directed]),
  }
}

// Edge identity: an unordered pair, or with direction on, an ordered pair
// for directed edges (so A->B, B->A and undirected A-B all differ).
function pairKey(a, b, directed) {
  if (directed) return a + '\t' + b + '\tdirected'
  return (a < b ? a + '\t' + b : b + '\t' + a) + '\tundirected'
}

// element -> bitmask of the sets containing it
export function membership(sets) {
  const mask = new Map()
  sets.forEach((set, i) => set.forEach((x) => mask.set(x, (mask.get(x) || 0) | (1 << i))))
  return mask
}

export function exclusiveCounts(mask) {
  const counts = new Map()
  mask.forEach((m) => counts.set(m, (counts.get(m) || 0) + 1))
  return counts
}

export function jaccard(a, b) {
  let inter = 0
  const [small, big] = a.size < b.size ? [a, b] : [b, a]
  small.forEach((x) => {
    if (big.has(x)) inter++
  })
  const union = a.size + b.size - inter
  return { inter, union, j: union ? inter / union : 0 }
}

export function spearman(xs, ys) {
  const rank = (arr) => {
    const idx = arr.map((v, i) => [v, i]).sort((p, q) => p[0] - q[0])
    const r = new Array(arr.length)
    for (let i = 0; i < idx.length;) {
      let j = i
      while (j + 1 < idx.length && idx[j + 1][0] === idx[i][0]) j++
      const avg = (i + j) / 2 + 1
      for (let k = i; k <= j; k++) r[idx[k][1]] = avg
      i = j + 1
    }
    return r
  }
  const n = xs.length
  if (n < 3) return NaN
  const rx = rank(xs),
    ry = rank(ys)
  const mx = (n + 1) / 2
  let num = 0,
    dx = 0,
    dy = 0
  for (let i = 0; i < n; i++) {
    num += (rx[i] - mx) * (ry[i] - mx)
    dx += (rx[i] - mx) ** 2
    dy += (ry[i] - mx) ** 2
  }
  return dx && dy ? num / Math.sqrt(dx * dy) : NaN
}

export function runComparison() {
  const chosen = [...document.querySelectorAll('#cmpNetList input:checked')]
    .map((i) => i.value)
    .slice(0, COMPARE_MAX)
  if (chosen.length < 2) {
    setStatus('cmpStatus', [{ level: 'error', text: 'Choose at least two networks to compare.' }])
    return
  }
  setStatus('cmpStatus', [{ level: 'busy', text: 'Comparing networks…' }])
  document.getElementById('btnCompare').disabled = true
  setTimeout(() => {
    try {
      const started = performance.now()
      const nets = chosen
        .map((value) => {
          const n = compareSource(value)
          if (n) n.sourceValue = value
          return n
        })
        .filter(Boolean)
      const useDir = document.getElementById('cmpDirected').checked
      nets.forEach((net, i) => {
        net.color = COMPARE_COLORS[i]
        net.nodeSet = new Set(net.ids)
        net.edgeSet = new Set()
        net.pairs.forEach(([a, b, d]) => {
          if (a !== b && net.nodeSet.has(a) && net.nodeSet.has(b))
            net.edgeSet.add(pairKey(a, b, useDir && d))
        })
        net.graph = simpleGraph(
          net.ids,
          net.pairs.map((p) => [p[0], p[1]])
        )
        net.stats = profileGraph(net.graph)
        net.directed =
          useDir && net.pairs.some((p) => p[2]) ? profileDirected(net.ids, net.pairs) : null
        net.degreeOf = new Map(net.ids.map((id, k) => [id, net.stats.degree[k]]))
      })
      compareState.results = nets
      renderComparison(nets)
      const secs = ((performance.now() - started) / 1000).toFixed(2)
      setStatus('cmpStatus', [
        { level: 'ok', text: `Compared ${plural(nets.length, 'network')} in ${secs} s.` },
      ])
      document.getElementById('btnCompareTsv').disabled = false
      document.getElementById('btnCompareNet').disabled = false
      document.getElementById('cmpArena').hidden = false
    } catch (err) {
      setStatus('cmpStatus', [
        { level: 'error', text: `The comparison couldn't be computed: ${err.message}` },
      ])
    } finally {
      updateCompareLimit()
    }
  }, 30)
}

// page wiring, run by main.ts in the original order
export function init() {
  ;['nodeLabelColorMode', 'edgeLabelColorMode'].forEach((id) => {
    document.getElementById(id).addEventListener('change', updateLabelStyle)
  })

  ;['nodeLabelColor', 'edgeLabelColor'].forEach((id) => {
    document.getElementById(id).addEventListener('input', updateLabelStyle)
  })
}
