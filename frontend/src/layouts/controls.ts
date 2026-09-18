// @ts-nocheck
// ponytail: split verbatim from the former single-file script; typed one file at a time (remove @ts-nocheck as it lands)
import { MAX_NETWORK_NODES, UNGROUPED, getUsedGroups } from '../network_state'
import { S } from '../state'
import { SAMPLE_GENERATORS } from '../sample_data'
import { STRATEGIES, groupArrangement, layoutMode, runActiveLayout } from './run'
import {
  applyConfig,
  applyGroupVisibility,
  buildGroupLegend,
  filteredGroups,
  getCurrentConfig,
  loadData,
  sortedByName,
} from '../hulls'
import { askDelete, deleteLibEntries } from '../api/tester'
import { clearCanvas, fitView, openInNewView, renderProfilerNetworkList } from '../profiler'
import { cy } from '../cy'
import { edgeIsDirected } from '../export/dialog'
import { exportEdges, exportNodes } from '../metrics'
import { loadNormaExampleSet } from '../examples'
import { numericShare, parseNumericValues } from '../clustering/mcl'

/* ---------- layout controls ---------- */
const ARRANGEMENT_CAPTIONS = {
  fr: 'Groups are placed by how strongly they connect to each other (weighted force-directed), then pushed apart so none overlap.',
  cose: 'Groups are placed by how they connect to each other (Cytoscape force-directed), then pushed apart so none overlap.',
  circle: 'Groups sit side by side on a ring, in legend order.',
  grid: 'Groups are packed in rows, in legend order.',
  breadthfirst:
    'Groups are stacked in levels: the best-connected group on top, the groups it links to below it, and so on.',
  concentric: 'The largest group sits in the middle, with the others in rings around it by size.',
  virtual:
    'Each group gets a hidden hub tied to all its members by heavy links (dashed), so the layout pulls each group together. The hubs are removed afterwards.',
  gravity:
    'Links inside a group are strengthened by the force and links between groups weakened by it. Layout-only links (dashed) join every pair in a group.',
  supernodes:
    'Each group is collapsed into one node and the collapsed network is laid out. Groups are then pushed apart by the force, and each group\u2019s nodes are arranged around its position. Nodes in several groups sit between them.',
}

export function setLayoutMode(mode) {
  document.getElementById('layoutMode').value = mode === 'groups' ? 'groups' : 'connections'
  updateStrategyUI()
}

export function updateStrategyUI() {
  const mode = layoutMode()
  const arrangement = groupArrangement()
  const isStrategy = STRATEGIES.has(arrangement)
  document.querySelectorAll('#layoutModeSwitch [role="radio"]').forEach((btn) => {
    const on = btn.dataset.mode === mode
    btn.setAttribute('aria-checked', on ? 'true' : 'false')
    btn.tabIndex = on ? 0 : -1
  })
  document.getElementById('connLayoutSet').disabled = mode !== 'connections'
  document.getElementById('groupLayoutSet').disabled = mode !== 'groups'

  const fig = document.getElementById('strategyFig')
  fig
    .querySelectorAll('svg')
    .forEach((svg) => svg.classList.toggle('on', svg.dataset.strategy === arrangement))
  document.getElementById('strategyCaption').textContent = ARRANGEMENT_CAPTIONS[arrangement] || ''
  document.getElementById('strategyAlgorithmRow').hidden = !isStrategy
  document.getElementById('strategyAlgorithmLabel').textContent =
    arrangement === 'supernodes'
      ? 'Algorithm for the collapsed network'
      : 'Algorithm the strategy runs'

  const keepOpt = document.querySelector('#localGroupLayout option[value="keep"]')
  const keepAllowed = arrangement === 'virtual' || arrangement === 'gravity'
  keepOpt.disabled = !keepAllowed
  keepOpt.hidden = !keepAllowed
  const localSel = document.getElementById('localGroupLayout')
  if (!keepAllowed && localSel.value === 'keep') localSel.value = 'circle'

  document.getElementById('forceLabelText').textContent = isStrategy
    ? 'Force strength'
    : 'Space between groups'
  document.getElementById('forceRow').hidden =
    arrangement === 'virtual' && localSel.value === 'keep'
  document.getElementById('clusterRow').hidden = localSel.value === 'keep'
  document.getElementById('groupForceValue').textContent =
    document.getElementById('groupForce').value
  document.getElementById('groupClusterRadiusValue').textContent = (
    parseFloat(document.getElementById('groupClusterRadius').value) || 1
  ).toFixed(1)
  document.getElementById('btnRunLayout').textContent =
    mode === 'groups' ? 'Run group layout' : 'Run layout'
}

/* ============================================================
   FILES: NORMA-FORMAT LIBRARY
   Reads and writes the three tab-delimited inputs used by NORMA
   (Koutrouli et al. 2022; Karatzas et al. 2022):
     network     -- header "Source<TAB>Target[<TAB>Weight][<TAB>Type]",
                    undirected; self-loops and repeats are dropped. The
                    optional Type column is this tool's extension: it keeps
                    parallel edges, one per type.
     annotation  -- no header; "group<TAB>node1,node2,..."
     expression  -- no header; "node<TAB>color" (name or hex)
   Every uploaded file is kept in an in-memory library. Any set of networks
   can be viewed together (each becomes an edge channel) with one
   annotation and one expression file. Annotation and expression entries
   for nodes missing from the chosen networks are discarded at view time
   (the job of NORMA's companion R script); the corrected annotation can be
   downloaded.
   ============================================================ */
export const NORMA_KINDS = ['network', 'annotation', 'colors']

export const NORMA_KIND_NAMES = {
  network: 'network',
  annotation: 'annotation',
  colors: 'expression file',
}

const LIB_LIST_IDS = {
  network: 'libNetworks',
  annotation: 'libAnnotations',
  colors: 'libExpressions',
}

export const normaLibrary = { network: [], annotation: [], colors: [] }

export const libSelection = { networks: new Set(), annotation: '', colors: '' }

export function plural(n, one, many) {
  return `${n} ${n === 1 ? one : many || one + 's'}`
}

export function listSample(items, max = 6) {
  const shown = items.slice(0, max).join(', ')
  return items.length > max ? `${shown} and ${items.length - max} more` : shown
}

export function articleFor(word) {
  return /^[aeiou]/i.test(word) ? 'an' : 'a'
}

export function normaLines(text) {
  return String(text)
    .replace(/^\uFEFF/, '')
    .split(/\r\n|\n|\r/)
}

// Tab-separated columns. Files saved with spaces instead of tabs (common
// after copy-pasting) fall back to splitting on whitespace.
export function normaCols(line) {
  let cols = line.split('\t')
  if (cols.length === 1) cols = line.trim().split(/\s+/)
  cols = cols.map((c) => c.trim())
  while (cols.length && cols[cols.length - 1] === '') cols.pop()
  return cols
}

const NETWORK_TYPE_HEADERS = new Set(['type', 'channel', 'layer', 'interaction'])

const NETWORK_DIRECTION_HEADERS = new Set(['direction', 'directed', 'dir', 'arrow'])

const DIRECTED_WORDS = new Set(['directed', 'yes', 'y', 'true', '1', '->', '-->', '→', 'forward'])

const UNDIRECTED_WORDS = new Set([
  'undirected',
  'no',
  'n',
  'false',
  '0',
  '-',
  '--',
  '—',
  'both',
  'none',
  '',
])

// options.directed: how to read rows without a Direction value
export function parseNormaNetwork(text, options = {}) {
  const defaultDirected = !!options.directed
  const lines = normaLines(text)
  const headerIdx = lines.findIndex((l) => l.trim() !== '')
  if (headerIdx < 0) throw new Error('The file is empty.')
  const header = normaCols(lines[headerIdx]).map((h) => h.toLowerCase())
  if (header[0] !== 'source' || header[1] !== 'target') {
    throw new Error(
      'The first line must be the header "Source, Target" (optionally followed by Weight, Type and Direction), separated by tabs.'
    )
  }
  let weightCol = -1,
    typeCol = -1,
    dirCol = -1
  for (let c = 2; c < header.length; c++) {
    if (header[c] === 'weight' && weightCol < 0) weightCol = c
    else if (NETWORK_TYPE_HEADERS.has(header[c]) && typeCol < 0) typeCol = c
    else if (NETWORK_DIRECTION_HEADERS.has(header[c]) && dirCol < 0) dirCol = c
    else
      throw new Error(
        `The header column "${header[c]}" isn't recognised. After Source and Target, only Weight, Type and Direction are allowed.`
      )
  }
  const minCols = Math.max(2, weightCol + 1, typeCol + 1)
  const badDirections = []

  const nodes = new Set()
  const seen = new Set()
  const edges = []
  const types = new Set()
  let selfLoops = 0,
    repeats = 0,
    directedCount = 0
  const shortLines = [],
    badWeights = []
  for (let i = headerIdx + 1; i < lines.length; i++) {
    if (!lines[i].trim()) continue
    const cols = normaCols(lines[i])
    if (cols.length < minCols) {
      shortLines.push(i + 1)
      continue
    }
    const [source, target] = cols
    nodes.add(source)
    nodes.add(target)
    if (source === target) {
      selfLoops++
      continue
    }
    const type = typeCol >= 0 ? cols[typeCol] : ''
    let directed = defaultDirected
    if (dirCol >= 0 && cols[dirCol] !== undefined) {
      const word = cols[dirCol].trim().toLowerCase()
      if (DIRECTED_WORDS.has(word)) directed = true
      else if (UNDIRECTED_WORDS.has(word)) directed = false
      else badDirections.push(i + 1)
    }
    // A directed edge A->B differs from B->A; an undirected A-B doesn't.
    const pair = directed
      ? 'D\t' + source + '\t' + target
      : 'U\t' + (source < target ? source + '\t' + target : target + '\t' + source)
    const key = pair + '\t' + type
    if (seen.has(key)) {
      repeats++
      continue
    }
    seen.add(key)
    const edge = { source, target }
    if (directed) {
      edge.directed = true
      directedCount++
    }
    if (type) {
      edge.type = type
      types.add(type)
    }
    if (weightCol >= 0) {
      const w = Number(cols[weightCol])
      if (Number.isFinite(w)) edge.weight = w
      else badWeights.push(i + 1)
    }
    edges.push(edge)
  }
  if (!edges.length) throw new Error('No connections were found below the header row.')

  const notes = []
  if (nodes.size > MAX_NETWORK_NODES) {
    // keep the first nodes in the order they appear, and the edges among them
    const keep = new Set([...nodes].slice(0, MAX_NETWORK_NODES))
    const total = nodes.size,
      before = edges.length
    const kept = edges.filter((e) => keep.has(e.source) && keep.has(e.target))
    edges.length = 0
    edges.push(...kept)
    nodes.clear()
    keep.forEach((n) => nodes.add(n))
    types.clear()
    directedCount = 0
    edges.forEach((e) => {
      if (e.type) types.add(e.type)
      if (e.directed) directedCount++
    })
    notes.push(
      `This network has ${total.toLocaleString('en-US')} nodes; NORMA shows up to ${MAX_NETWORK_NODES.toLocaleString('en-US')}, so only the first ${MAX_NETWORK_NODES.toLocaleString('en-US')} (in file order) and the ${plural(edges.length, 'edge')} among them were kept (${(before - edges.length).toLocaleString('en-US')} edges left out).`
    )
    if (!edges.length)
      throw new Error(
        `The first ${MAX_NETWORK_NODES.toLocaleString('en-US')} nodes of this network have no connections among them.`
      )
  }
  if (selfLoops) notes.push(`Removed ${plural(selfLoops, 'self-loop')}.`)
  if (repeats)
    notes.push(
      directedCount === edges.length
        ? `Removed ${plural(repeats, 'repeated connection')} with the same source, target${typeCol >= 0 ? ' and type' : ''}.`
        : typeCol >= 0
          ? `Removed ${plural(repeats, 'repeated connection')} with the same pair and type${directedCount ? ' (for undirected rows, A–B and B–A are the same pair)' : ''}.`
          : `Removed ${plural(repeats, 'repeated connection')} (for undirected rows, A–B and B–A count as the same connection).`
    )
  if (badDirections.length)
    notes.push(
      `Read ${plural(badDirections.length, 'Direction value')} that ${badDirections.length === 1 ? "isn't" : "aren't"} "directed" or "undirected" as ${defaultDirected ? 'directed' : 'undirected'}: line ${listSample(badDirections)}.`
    )
  if (shortLines.length)
    notes.push(
      `Skipped ${plural(shortLines.length, 'line')} with fewer than ${minCols} columns: line ${listSample(shortLines)}.`
    )
  if (badWeights.length)
    notes.push(
      `Left out ${plural(badWeights.length, 'weight')} that ${badWeights.length === 1 ? "isn't a number" : "aren't numbers"}: line ${listSample(badWeights)}.`
    )
  const commaNames = [...nodes].filter((n) => n.includes(','))
  if (commaNames.length)
    notes.push(
      `${plural(commaNames.length, 'node name contains', 'node names contain')} a comma, so annotation files can't list ${commaNames.length === 1 ? 'it' : 'them'}: ${listSample(commaNames)}.`
    )

  const summaryParts = [plural(nodes.size, 'node'), plural(edges.length, 'edge')]
  if (types.size) summaryParts.push(plural(types.size, 'type'))
  if (weightCol >= 0) summaryParts.push('weighted')
  const direction =
    directedCount === 0 ? 'undirected' : directedCount === edges.length ? 'directed' : 'mixed'
  if (direction !== 'undirected')
    summaryParts.push(
      direction === 'mixed' ? `mixed (${directedCount.toLocaleString()} directed)` : 'directed'
    )
  return {
    nodes: [...nodes],
    edges,
    weighted: weightCol >= 0,
    types: [...types],
    notes,
    direction,
    directedCount,
    summary: summaryParts.join(', '),
  }
}

const ANNOTATION_HEADER_WORDS =
  /^(groups?|annotations?|terms?|pathways?|names?|modules?|clusters?)$/i

function parseNormaAnnotation(text) {
  const lines = normaLines(text)
  const groups = new Map()
  let spacesStripped = 0,
    merged = 0,
    headerSkipped = false
  const shortLines = []
  let firstDataLine = true
  lines.forEach((raw, i) => {
    if (!raw.trim()) return
    let name, list
    const tab = raw.indexOf('\t')
    if (tab >= 0) {
      name = raw.slice(0, tab).trim()
      list = raw.slice(tab + 1).replace(/\t/g, '')
    } else {
      // no tab: only accept the unambiguous "name<spaces>list" shape
      const parts = raw.trim().split(/\s+/)
      if (parts.length === 2) {
        name = parts[0]
        list = parts[1]
      }
    }
    if (!name || !list || !list.trim()) {
      shortLines.push(i + 1)
      firstDataLine = false
      return
    }
    if (firstDataLine && ANNOTATION_HEADER_WORDS.test(name) && !list.includes(',')) {
      headerSkipped = true
      firstDataLine = false
      return
    }
    firstDataLine = false
    if (/\s/.test(list.trim())) spacesStripped++
    const members = list
      .split(',')
      .map((m) => m.trim())
      .filter(Boolean)
    if (groups.has(name)) merged++
    const set = groups.get(name) || new Set()
    members.forEach((m) => set.add(m))
    groups.set(name, set)
  })
  if (!groups.size) {
    throw new Error(
      'No groups were found. Each line should be a group name, a tab, then node names separated by commas.'
    )
  }
  const notes = []
  if (headerSkipped)
    notes.push(
      'Skipped the first line because it looks like a header. Annotation files have no header row.'
    )
  if (shortLines.length)
    notes.push(
      `Skipped ${plural(shortLines.length, 'line')} without a group name and node list: line ${listSample(shortLines)}.`
    )
  if (spacesStripped)
    notes.push(
      `Removed spaces from the node lists on ${plural(spacesStripped, 'line')}. NORMA expects commas only.`
    )
  if (merged) notes.push(`Merged ${plural(merged, 'repeated group name')} into one group each.`)
  const list = [...groups].map(([name, set]) => ({ name, members: [...set] }))
  return { groups: list, notes, summary: plural(list.length, 'group') }
}

const colorProbeCtx = document.createElement('canvas').getContext('2d')

// Returns a canvas-normalized color ("#rrggbb" or "rgba(...)"), or null
// if the browser doesn't recognise the value as a CSS color.
export function normalizeCssColor(value) {
  const probe = new Option().style
  probe.color = value
  if (!probe.color) return null
  colorProbeCtx.fillStyle = '#000000'
  colorProbeCtx.fillStyle = value
  return colorProbeCtx.fillStyle
}

function parseNormaColors(text) {
  // numbers instead of colors: a numeric expression file
  const sample = normaLines(text)
    .filter((l) => l.trim())
    .slice(0, 300)
  if (sample.length && numericShare(sample.slice(1)) >= 0.8) return parseNumericValues(text)
  const lines = normaLines(text)
  const colors = new Map()
  const invalid = [],
    shortLines = []
  let headerSkipped = false,
    firstDataLine = true
  lines.forEach((raw, i) => {
    if (!raw.trim()) return
    const cols = normaCols(raw)
    if (cols.length < 2) {
      shortLines.push(i + 1)
      firstDataLine = false
      return
    }
    const [node, value] = cols
    const color = normalizeCssColor(value)
    if (!color) {
      if (firstDataLine && /^(nodes?|genes?|names?|ids?|proteins?)$/i.test(node)) {
        headerSkipped = true
      } else invalid.push(`${node} (${value})`)
      firstDataLine = false
      return
    }
    firstDataLine = false
    colors.set(node, color)
  })
  if (!colors.size) {
    throw new Error(
      'No node colors were found. Each line should be a node name, a tab, then a color such as red or #ff0000.'
    )
  }
  const notes = []
  if (headerSkipped)
    notes.push(
      'Skipped the first line because it looks like a header. Node-color files have no header row.'
    )
  if (shortLines.length)
    notes.push(
      `Skipped ${plural(shortLines.length, 'line')} without both a node and a color: line ${listSample(shortLines)}.`
    )
  if (invalid.length)
    notes.push(
      `Ignored ${plural(invalid.length, 'unrecognised color')}: ${listSample(invalid)}. Use a color name like red or a hex code like #ff0000.`
    )
  return { colors, notes, summary: plural(colors.size, 'colored node') }
}

export const NORMA_PARSERS = {
  network: parseNormaNetwork,
  annotation: parseNormaAnnotation,
  colors: parseNormaColors,
}

// Sanity-checks upload content before it reaches the format-specific parsers
// above, which all assume tab-separated text. Without this, a JSON/XML/
// binary file doesn't fail cleanly: detectNormaKind has no header or
// numeric/color column to recognise, so it falls back to "annotation" and
// the file is silently read as a garbled (but technically valid) group
// list instead of being rejected with a useful hint.
export function checkNormaFileFormat(text) {
  const trimmed = String(text).replace(/^﻿/, '').trimStart()
  if (!trimmed) return // an empty file is reported by the parsers themselves
  const sample = trimmed.slice(0, 4000)
  if (sample.slice(0, 200).includes('�') || sample.includes(' ')) {
    throw new Error(
      "This looks like a binary file, not plain text. NORMA's Files uploader needs a tab-separated .txt/.tsv/.csv file — export the data as text first."
    )
  }
  if (sample[0] === '{' || sample[0] === '[') {
    throw new Error(
      'This looks like JSON, not a tab-separated NORMA file. If it’s a saved NORMA view or session, use "Open saved work" instead of the Files uploader.'
    )
  }
  if (sample[0] === '<') {
    throw new Error(
      "This looks like an XML file (e.g. GraphML, GEXF or SVG), which the Files uploader doesn't read. Export it as a tab-separated Source/Target network, group/member list, or node/color file instead."
    )
  }
}

// Guesses a file's kind: a Source/Target header means a network; a second
// column that is (almost) always a color means an expression file;
// anything else is read as an annotation.
export function detectNormaKind(text) {
  const lines = normaLines(text)
    .filter((l) => l.trim())
    .slice(0, 300)
  if (!lines.length) return null
  const head = normaCols(lines[0]).map((c) => c.toLowerCase())
  if (head[0] === 'source' && head[1] === 'target') return 'network'
  if (numericShare(lines.slice(1)) >= 0.8) return 'colors'
  let rows = 0,
    colorish = 0,
    commas = 0
  lines.forEach((line) => {
    const tab = line.indexOf('\t')
    const second = (tab >= 0 ? line.slice(tab + 1) : line.trim().split(/\s+/)[1] || '').trim()
    if (!second) return
    rows++
    if (second.includes(',')) commas++
    else if (normalizeCssColor(second)) colorish++
  })
  if (rows && !commas && colorish >= rows * 0.8) return 'colors'
  return 'annotation'
}

function uniqueNormaName(kind, name) {
  const taken = new Set(normaLibrary[kind].map((e) => e.name))
  if (!taken.has(name)) return name
  let n = 2
  while (taken.has(`${name} (${n})`)) n++
  return `${name} (${n})`
}

// Parses and stores a file. Throws with a readable message if the file
// doesn't match the format; returns the new library entry otherwise.
export function addNormaEntry(kind, name, text, fileName, sourceKey, options) {
  const parsed = NORMA_PARSERS[kind](text, options || {})
  const entry = {
    id: 'lib' + ++S.normaEntrySeq,
    kind,
    name: uniqueNormaName(kind, name),
    fileName: fileName || '',
    sourceKey: sourceKey || null,
    text,
    options: options || {},
    parsed,
  }
  normaLibrary[kind].push(entry)
  return entry
}

export function libEntry(kind, id) {
  return normaLibrary[kind].find((e) => e.id === id) || null
}

export function selectedNetworks() {
  return normaLibrary.network.filter((e) => libSelection.networks.has(e.id))
}

export function selectionKey() {
  return {
    nets: selectedNetworks()
      .map((e) => e.id)
      .join('|'),
    annotation: libSelection.annotation,
    colors: libSelection.colors,
  }
}

function selectionDiffersFromView() {
  if (!S.currentLibView) return libSelection.networks.size > 0
  const k = selectionKey()
  return (
    k.nets !== S.currentLibView.nets ||
    k.annotation !== S.currentLibView.annotation ||
    k.colors !== S.currentLibView.colors
  )
}

export function updateRefreshState() {
  const pending = selectionDiffersFromView() && libSelection.networks.size > 0
  document.getElementById('refreshHint').hidden = !pending
  const btn = document.getElementById('btnRefreshView')
  btn.classList.toggle('pending', pending)
  btn.disabled = libSelection.networks.size === 0
  const nb = document.getElementById('btnOpenNewView')
  if (nb) nb.disabled = libSelection.networks.size === 0
}

export function renderLibraryLists() {
  const delAll = document.getElementById('btnDeleteAllFiles')
  if (delAll) delAll.disabled = !NORMA_KINDS.some((k) => normaLibrary[k].length)
  NORMA_KINDS.forEach((kind) => {
    const el = document.getElementById(LIB_LIST_IDS[kind])
    el.innerHTML = ''
    const entries = normaLibrary[kind]
    const multi = kind === 'network'
    const groupName = 'lib-' + kind

    if (!multi) {
      el.appendChild(
        libRow({
          kind,
          inputType: 'radio',
          groupName,
          id: '',
          name: 'None',
          meta: '',
          checked: (kind === 'annotation' ? libSelection.annotation : libSelection.colors) === '',
        })
      )
    }
    if (multi && !entries.length) {
      const empty = document.createElement('div')
      empty.className = 'lib-empty'
      empty.textContent = 'Upload a network or load an example.'
      el.appendChild(empty)
    }
    sortedByName(entries, (e) => e.name).forEach((e) => {
      const checked = multi
        ? libSelection.networks.has(e.id)
        : (kind === 'annotation' ? libSelection.annotation : libSelection.colors) === e.id
      el.appendChild(
        libRow({
          kind,
          inputType: multi ? 'checkbox' : 'radio',
          groupName,
          id: e.id,
          name: e.name,
          meta: e.parsed.summary,
          checked,
          removable: true,
          title: e.fileName ? `${e.name} (${e.fileName})` : e.name,
        })
      )
    })
  })
  updateRefreshState()
  if (typeof renderProfilerNetworkList === 'function') renderProfilerNetworkList()
}

function libRow({ kind, inputType, groupName, id, name, meta, checked, removable, title }) {
  const row = document.createElement('label')
  row.className = 'lib-row'
  if (title) row.title = title
  const input = document.createElement('input')
  input.type = inputType
  input.name = groupName
  input.checked = checked
  input.value = id
  input.addEventListener('change', () => {
    if (kind === 'network') {
      if (input.checked) libSelection.networks.add(id)
      else libSelection.networks.delete(id)
    } else if (input.checked) {
      if (kind === 'annotation') libSelection.annotation = id
      else libSelection.colors = id
    }
    updateRefreshState()
  })
  const text = document.createElement('span')
  text.className = 'lib-text'
  const nameEl = document.createElement('span')
  nameEl.className = 'lib-name'
  nameEl.textContent = name
  text.appendChild(nameEl)
  if (meta) {
    const metaEl = document.createElement('span')
    metaEl.className = 'lib-meta'
    metaEl.textContent = meta
    text.appendChild(metaEl)
  }
  row.append(input, text)
  if (removable) {
    const rm = document.createElement('button')
    rm.type = 'button'
    rm.className = 'lib-remove'
    rm.textContent = '✕'
    rm.setAttribute('aria-label', `Delete ${name}`)
    rm.title = 'Delete this file from NORMA'
    rm.addEventListener('click', (ev) => {
      ev.preventDefault()
      ev.stopPropagation()
      askDelete([{ kind, id }])
    })
    row.appendChild(rm)
  }
  return row
}

// kept for scripts: deletes without asking
function removeLibEntry(kind, id) {
  deleteLibEntries([{ kind, id }])
}

// items: [{ level: 'ok' | 'warn' | 'error' | 'busy', text, action?: { label, run } }]
// opens the folded sidebar section that holds `el`
function revealSection(el) {
  const sec = el && el.closest && el.closest('.section.collapsible.collapsed')
  if (!sec) return
  sec.classList.remove('collapsed')
  const h = sec.querySelector(':scope > h3')
  if (h) h.setAttribute('aria-expanded', 'true')
}

// A busy item may carry `progress`: a fraction from 0 to 1, or null while
// the amount of work is unknown (an animated bar). Repeated progress updates
// change the bar in place, so it moves smoothly and screen readers are not
// sent every percentage.
export function setStatus(elId, items) {
  const el = document.getElementById(elId)
  if (!el) return
  const withProgress = items.length === 1 && items[0].progress !== undefined
  if (!withProgress && progressTasks && progressTasks.has(elId)) progressTasks.get(elId).stop()
  if (withProgress) {
    const note = el.children.length === 1 ? el.firstElementChild : null
    if (note && note.classList.contains('busy') && note._progress) {
      updateProgressNote(note, items[0])
      return
    }
  }
  // sections start folded: an error opens the section so it is seen
  if (items.some((i) => i.level === 'error')) revealSection(el)
  el.innerHTML = ''
  items.forEach((item) => {
    const note = document.createElement('div')
    note.className = 'note ' + (item.level || 'ok')
    if (item.progress !== undefined) {
      const label = document.createElement('span')
      const row = document.createElement('div')
      row.className = 'progress-row'
      const bar = document.createElement('div')
      bar.className = 'progress'
      bar.setAttribute('role', 'progressbar')
      bar.setAttribute('aria-valuemin', '0')
      bar.setAttribute('aria-valuemax', '100')
      bar.appendChild(document.createElement('span'))
      const pct = document.createElement('span')
      pct.className = 'progress-pct'
      pct.setAttribute('aria-hidden', 'true')
      row.append(bar, pct)
      note.append(label, row)
      note._progress = { label, bar, pct }
      updateProgressNote(note, item)
      el.appendChild(note)
      return
    }
    note.textContent = item.text
    if (item.action) {
      const btn = document.createElement('button')
      btn.type = 'button'
      btn.textContent = item.action.label
      btn.addEventListener('click', item.action.run)
      note.appendChild(btn)
    }
    el.appendChild(note)
  })
}

function updateProgressNote(note, item) {
  const { label, bar, pct } = note._progress
  if (label.textContent !== item.text) {
    label.textContent = item.text
    bar.setAttribute('aria-label', item.text)
  }
  const f = item.progress
  if (typeof f === 'number' && Number.isFinite(f)) {
    const v = Math.max(0, Math.min(1, f))
    bar.classList.remove('indeterminate')
    bar.firstElementChild.style.width = (v * 100).toFixed(1) + '%'
    bar.setAttribute('aria-valuenow', String(Math.round(v * 100)))
    pct.textContent = Math.floor(v * 100) + '%'
  } else {
    bar.classList.add('indeterminate')
    bar.firstElementChild.style.width = ''
    bar.removeAttribute('aria-valuenow')
    pct.textContent = ''
  }
}

/* ---------- progress of a task made of weighted steps ----------
   const task = startProgress('stringStatus', [1, 2, 5]);
   task.step(0, 'Checking the version…');   // moves to a step
   task.sub(0.4);                           // measured progress in the step
   task.say('Reading 3 of 8…');             // new text, same step
   task.bytes(received, total);             // download progress in the step
   Until a step reports measured progress, its bar creeps slowly toward the
   end of the step (never reaching it), so a long single request still shows
   that work goes on. Any status that is not a progress update, such as the
   result or an error, ends the task. */
var progressTasks = new Map()

function formatBytes(n) {
  if (n < 1024) return `${n} B`
  if (n < 1048576) return `${(n / 1024).toFixed(0)} kB`
  return `${(n / 1048576).toFixed(n < 10485760 ? 1 : 0)} MB`
}

export function startProgress(elId, weights) {
  const old = progressTasks.get(elId)
  if (old) old.stop()
  const w = weights.map((x) => Math.max(0, +x || 0))
  const total = w.reduce((a, b) => a + b, 0) || 1
  const starts = w.map((_, i) => w.slice(0, i).reduce((a, b) => a + b, 0))
  const task = {
    index: 0,
    text: '',
    measured: null,
    stepStart: performance.now(),
    received: 0,
    active: true,
    timer: null,
    last: 0,
    fraction() {
      const i = Math.min(this.index, w.length - 1)
      let sub = this.measured
      if (sub === null) {
        // creep: about half of the step after ~6 s, never more than 85%
        const t = (performance.now() - this.stepStart) / 1000
        sub = 0.85 * (1 - Math.exp(-t / 8))
      }
      return (starts[i] + w[i] * Math.max(0, Math.min(1, sub))) / total
    },
    render(force) {
      if (!this.active) return
      const now = performance.now()
      if (!force && now - this.last < 100) return
      this.last = now
      const extra = this.received ? ` (${formatBytes(this.received)} received)` : ''
      setStatus(elId, [{ level: 'busy', text: this.text + extra, progress: this.fraction() }])
    },
    step(i, text) {
      this.index = i
      this.text = text
      this.measured = null
      this.received = 0
      this.stepStart = performance.now()
      this.render(true)
    },
    say(text) {
      this.text = text
      this.render(true)
    },
    sub(f) {
      this.measured = Math.max(0, Math.min(1, f))
      this.render()
    },
    bytes(received, totalBytes) {
      this.received = received
      if (totalBytes > 0 && received <= totalBytes) this.measured = received / totalBytes
      this.render()
    },
    stop() {
      this.active = false
      clearInterval(this.timer)
      if (progressTasks.get(elId) === this) progressTasks.delete(elId)
    },
  }
  progressTasks.set(elId, task)
  task.timer = setInterval(() => task.render(true), 400)
  return task
}

// Resolves once the browser has had a chance to paint (for example a
// progress note) before synchronous work; does not wait in a hidden tab.
export function nextPaint() {
  return new Promise((resolve) => {
    let done = false
    const finish = () => {
      if (!done) {
        done = true
        resolve()
      }
    }
    requestAnimationFrame(() => setTimeout(finish, 0))
    setTimeout(finish, 60)
  })
}

// Reads a response body as text, reporting the bytes received so far and,
// when the server gives it, the total.
export async function readTextWithProgress(response, onBytes) {
  if (!onBytes || !response.body || typeof response.body.getReader !== 'function')
    return response.text()
  const total = Number(response.headers.get('Content-Length')) || 0
  const reader = response.body.getReader()
  const decoder = new TextDecoder()
  let received = 0,
    out = ''
  for (;;) {
    const { done, value } = await reader.read()
    if (done) break
    received += value.length
    out += decoder.decode(value, { stream: true })
    onBytes(received, total)
  }
  return out + decoder.decode()
}

export function downloadText(fileName, text) {
  const blob = new Blob([text], { type: 'text/plain' })
  const a = document.createElement('a')
  a.href = URL.createObjectURL(blob)
  a.download = fileName
  document.body.appendChild(a)
  a.click()
  a.remove()
  setTimeout(() => URL.revokeObjectURL(a.href), 1000)
}

export function annotationText(groups) {
  return groups.map((g) => `${g.name}\t${g.members.join(',')}`).join('\n') + '\n'
}

export function fileStem(name) {
  return (
    String(name)
      .replace(/\.[^.]+$/, '')
      .replace(/[^\w.-]+/g, '_')
      .replace(/^_+|_+$/g, '') || 'file'
  )
}

// page wiring, run by main.ts in the original order
export function init() {
  document.getElementById('btnRunLayout').addEventListener('click', runActiveLayout)

  document.querySelectorAll('#layoutModeSwitch [role="radio"]').forEach((btn) => {
    btn.addEventListener('click', () => setLayoutMode(btn.dataset.mode))
    btn.addEventListener('keydown', (e) => {
      if (!['ArrowLeft', 'ArrowRight', 'ArrowUp', 'ArrowDown'].includes(e.key)) return
      e.preventDefault()
      const next = btn.dataset.mode === 'groups' ? 'connections' : 'groups'
      setLayoutMode(next)
      document.querySelector(`#layoutModeSwitch [data-mode="${next}"]`).focus()
    })
  })

  ;['groupArrangement', 'localGroupLayout'].forEach((id) => {
    document.getElementById(id).addEventListener('change', updateStrategyUI)
  })

  ;['groupForce', 'groupClusterRadius'].forEach((id) => {
    document.getElementById(id).addEventListener('input', updateStrategyUI)
  })

  document.getElementById('btnFit').addEventListener('click', () => fitView())

  document.getElementById('btnSample').addEventListener('click', async () => {
    const sel = document.getElementById('sampleSelect')
    const key = sel.value
    const title = sel.options[sel.selectedIndex].text.replace(/\s*\([^)]*nodes\)\s*$/, '')
    const task = startProgress('examplesStatus', [1])
    task.step(0, `Opening "${title}"…`)
    await nextPaint()
    openInNewView(title, () => {
      if (key.startsWith('norma:')) {
        loadNormaExampleSet(key.slice(6))
        return
      }
      libSelection.networks = new Set()
      libSelection.annotation = ''
      libSelection.colors = ''
      renderLibraryLists()
      const gen = SAMPLE_GENERATORS[key] || SAMPLE_GENERATORS.trp
      loadData(gen())
      setStatus('normaStatus', [])
    })
    setStatus('examplesStatus', [{ level: 'ok', text: `Opened "${title}".` }])
  })

  document.getElementById('btnClear').addEventListener('click', () => {
    clearCanvas()
    setStatus('normaStatus', [])
    updateRefreshState()
    buildGroupLegend()
  })

  // With a filter set, these act only on the matching groups.
  document.getElementById('btnGroupsAll').addEventListener('click', () => {
    filteredGroups().forEach((g) => S.activeGroups.add(g))
    buildGroupLegend()
    applyGroupVisibility()
  })

  document.getElementById('btnGroupsNone').addEventListener('click', () => {
    filteredGroups().forEach((g) => S.activeGroups.delete(g))
    buildGroupLegend()
    applyGroupVisibility()
  })

  document.getElementById('groupFilter').addEventListener('input', buildGroupLegend)

  document.getElementById('btnImportToggle').addEventListener('click', () => {
    const box = document.getElementById('importBox')
    box.style.display = box.style.display === 'none' ? 'block' : 'none'
  })

  document.getElementById('btnImportGo').addEventListener('click', () => {
    try {
      const parsed = JSON.parse(document.getElementById('importArea').value)
      if (!parsed.nodes || !parsed.edges) throw new Error('JSON needs "nodes" and "edges" arrays.')
      openInNewView('Pasted JSON', () => loadData(parsed))
      document.getElementById('importBox').style.display = 'none'
      setStatus('savedWorkStatus', [{ level: 'ok', text: 'Opened the pasted JSON.' }])
    } catch (err) {
      setStatus('savedWorkStatus', [
        { level: 'error', text: `Could not parse JSON: ${err.message}` },
      ])
    }
  })

  document
    .getElementById('btnFileTrigger')
    .addEventListener('click', () => document.getElementById('fileInput').click())

  document.getElementById('fileInput').addEventListener('change', (e) => {
    const file = e.target.files[0]
    if (!file) return
    const task = startProgress('savedWorkStatus', [1])
    task.step(0, `Reading "${file.name}"…`)
    const reader = new FileReader()
    reader.onprogress = (ev) => task.bytes(ev.loaded, ev.lengthComputable ? ev.total : file.size)
    reader.onload = (ev) => {
      try {
        const parsed = JSON.parse(ev.target.result)
        if (!parsed.nodes || !parsed.edges)
          throw new Error('JSON needs "nodes" and "edges" arrays.')
        openInNewView(file.name.replace(/\.json$/i, ''), () => loadData(parsed))
        setStatus('savedWorkStatus', [{ level: 'ok', text: `Opened "${file.name}".` }])
      } catch (err) {
        setStatus('savedWorkStatus', [
          { level: 'error', text: `Could not read the file: ${err.message}` },
        ])
      }
    }
    reader.readAsText(file)
    e.target.value = ''
  })

  document.getElementById('btnExport').addEventListener('click', () => {
    const nodeColors = {}
    cy.nodes().forEach((n) => {
      ;(n.data('groups') || []).forEach((g) => {
        nodeColors[g] = S.nodeColorMap[g] || nodeColors[g]
      })
    })
    const edgeColors = {}
    cy.edges().forEach((e) => {
      edgeColors[e.data('type')] = e.data('color')
    })
    const data = {
      nodes: exportNodes().map((n) => ({
        id: n.data('id'),
        groups: (n.data('groups') || []).filter((g) => g !== UNGROUPED),
        size: n.data('baseSize'),
        ...(n.data('nodeColor') ? { color: n.data('nodeColor') } : {}),
        ...(n.data('values') ? { values: n.data('values') } : {}),
        ...(n.data('attrs') || {}),
      })),
      edges: exportEdges().map((e) => {
        const out = {
          id: e.data('id'),
          source: e.data('source'),
          target: e.data('target'),
          type: e.data('type'),
        }
        const w = e.data('weight')
        if (typeof w === 'number') out.weight = w
        if (edgeIsDirected(e)) out.directed = true
        return { ...out, ...(e.data('attrs') || {}) }
      }),
      nodeColors,
      edgeColors,
      groupOrder: getUsedGroups().filter((g) => g !== UNGROUPED),
      ...(Object.keys(S.groupShapes).length ? { groupShapes: S.groupShapes } : {}),
      ...(Object.keys(S.groupAttrs).length ? { groupAttrs: S.groupAttrs } : {}),
    }
    const blob = new Blob([JSON.stringify(data, null, 2)], { type: 'application/json' })
    const a = document.createElement('a')
    a.href = URL.createObjectURL(blob)
    a.download = 'network.json'
    a.click()
  })

  /* settings (config) export / import -- separate from network data */
  document.getElementById('btnExportConfig').addEventListener('click', () => {
    const blob = new Blob([JSON.stringify(getCurrentConfig(), null, 2)], {
      type: 'application/json',
    })
    const a = document.createElement('a')
    a.href = URL.createObjectURL(blob)
    a.download = 'norma-settings.json'
    a.click()
  })

  document
    .getElementById('btnImportConfigTrigger')
    .addEventListener('click', () => document.getElementById('configFileInput').click())

  document.getElementById('configFileInput').addEventListener('change', (e) => {
    const file = e.target.files[0]
    if (!file) return
    const task = startProgress('savedWorkStatus', [1])
    task.step(0, `Reading "${file.name}"…`)
    const reader = new FileReader()
    reader.onprogress = (ev) => task.bytes(ev.loaded, ev.lengthComputable ? ev.total : file.size)
    reader.onload = (ev) => {
      try {
        const parsed = JSON.parse(ev.target.result)
        applyConfig(parsed)
        setStatus('savedWorkStatus', [
          { level: 'ok', text: `Applied settings from "${file.name}".` },
        ])
      } catch (err) {
        setStatus('savedWorkStatus', [
          { level: 'error', text: `Could not read settings file: ${err.message}` },
        ])
      }
    }
    reader.readAsText(file)
    e.target.value = ''
  })
}
