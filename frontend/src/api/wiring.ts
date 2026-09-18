// @ts-nocheck
// ponytail: split verbatim from the former single-file script; typed one file at a time (remove @ts-nocheck as it lands)
import {
  REACTOME_SPECIES,
  dbCancel,
  filterGoModels,
  goAnnotateView,
  goFetchModel,
  goLoadModels,
  intactFetch,
  isSessionObject,
  loadSession,
  ndexFetch,
  ndexSearch,
  omnipathFetch,
  reactomeFetch,
  reactomeSearch,
  toast,
} from '../enrichment'
import { UNGROUPED, getUsedGroups } from '../network_state'
import {
  addNormaEntry,
  libSelection,
  plural,
  renderLibraryLists,
  setLayoutMode,
  setStatus,
} from '../layouts/controls'
import { applyConfig, loadData } from '../hulls'
import { applyTheme } from '../themes'
import { cy } from '../cy'
import { openInNewView } from '../profiler'
import { refreshLibraryView } from '../library'
import { runLayout } from '../layouts/run'
import { switchTab } from '../wiring'

/* ============================================================
   NORMA API
   Other applications can open NORMA with their own networks and groups:
     1. REST (with server.py): POST a payload to /api/external; the answer
        { token, url } holds a link that opens NORMA with the data loaded
        (norma.html?session=TOKEN).
     2. Links: norma.html?data=URL (a payload file), or
        norma.html?network=URL&annotation=URL&expression=URL (NORMA files),
        or norma.html#json=BASE64URL (a small payload inside the link).
     3. postMessage: a page that opens NORMA in a window or iframe sends
        { type: 'norma:load', payload, requestId } and gets back
        { type: 'norma:loaded', ok, summary | error, requestId }.
   Every route ends in normaApiLoad(payload).
   ============================================================ */
const NORMA_API_VERSION = '1.0'

const API_MAX_BYTES = 50 * 1024 * 1024

function apiTextOf(v) {
  if (v == null) return []
  return (Array.isArray(v) ? v : [v])
    .map((x) => (typeof x === 'string' ? { text: x } : x))
    .filter((x) => x && typeof x.text === 'string')
}

function apiSafe(s) {
  return String(s ?? '')
    .replace(/[\t\r\n]+/g, ' ')
    .trim()
}

function apiNode(s) {
  return apiSafe(s).replace(/,/g, ';')
}

// { edges: [...] } -> NORMA network text
function apiEdgesText(edges) {
  if (!Array.isArray(edges) || !edges.length)
    throw new Error('"edges" must be a non-empty list of { source, target } objects.')
  const weighted = edges.some((e) => typeof e.weight === 'number')
  const typed = edges.some((e) => e.type)
  const directed = edges.some((e) => e.directed)
  const head = [
    'Source',
    'Target',
    ...(weighted ? ['Weight'] : []),
    ...(typed ? ['Type'] : []),
    ...(directed ? ['Direction'] : []),
  ]
  const rows = edges.map((e, i) => {
    if (!e || e.source == null || e.target == null)
      throw new Error(`Edge ${i + 1} needs a source and a target.`)
    return [
      apiNode(e.source),
      apiNode(e.target),
      ...(weighted ? [typeof e.weight === 'number' ? e.weight : 1] : []),
      ...(typed ? [apiSafe(e.type || 'link')] : []),
      ...(directed ? [e.directed ? 'directed' : 'undirected'] : []),
    ].join('\t')
  })
  return [head.join('\t'), ...rows].join('\n') + '\n'
}

// groups as { name: [members] } or [{ name, members, color, description }]
function apiGroupsText(groups) {
  const list = Array.isArray(groups)
    ? groups
    : Object.entries(groups || {}).map(([name, members]) => ({ name, members }))
  const meta = {}
  const rows = list.map((g, i) => {
    if (!g || !Array.isArray(g.members)) throw new Error(`Group ${i + 1} needs a "members" list.`)
    const name = apiSafe(g.name || `Group ${i + 1}`)
    const m = {}
    if (g.color) m.color = String(g.color)
    if (g.description) m.description = String(g.description)
    if (Object.keys(m).length) meta[name] = m
    return `${name}\t${g.members.map(apiNode).join(',')}`
  })
  return { text: rows.join('\n') + '\n', meta }
}

// expression as { node: '#color' } or { node: number } or { node: { col: number } }
function apiExpressionText(expr) {
  const entries = Object.entries(expr || {})
  if (!entries.length) return null
  const first = entries[0][1]
  if (first && typeof first === 'object') {
    const cols = [...new Set(entries.flatMap(([, v]) => Object.keys(v || {})))]
    return (
      [
        'Node\t' + cols.join('\t'),
        ...entries.map(([n, v]) =>
          [apiNode(n), ...cols.map((c) => (v && v[c] != null ? v[c] : 'NA'))].join('\t')
        ),
      ].join('\n') + '\n'
    )
  }
  if (typeof first === 'number')
    return (
      ['Node\tvalue', ...entries.map(([n, v]) => `${apiNode(n)}\t${v == null ? 'NA' : v}`)].join(
        '\n'
      ) + '\n'
    )
  return entries.map(([n, v]) => `${apiNode(n)}\t${v}`).join('\n') + '\n'
}

export async function normaApiLoad(payload, origin = 'API') {
  if (typeof payload === 'string') {
    if (payload.length > API_MAX_BYTES) throw new Error('The payload is larger than 50 MB.')
    try {
      payload = JSON.parse(payload)
    } catch (e) {
      throw new Error('The payload is not valid JSON.')
    }
  }
  if (!payload || typeof payload !== 'object' || Array.isArray(payload))
    throw new Error('The payload must be a JSON object.')
  // a whole session
  if (isSessionObject(payload) || isSessionObject(payload.session)) {
    const r = loadSession(isSessionObject(payload) ? payload : payload.session)
    switchTab('network')
    return { kind: 'session', views: r.views, files: r.files }
  }
  const name = apiSafe(payload.name) || 'API network'
  const settings =
    payload.settings && typeof payload.settings === 'object' ? payload.settings : null
  const files = payload.files || {}
  const netTexts = [...apiTextOf(files.network)]
  if (payload.edges) netTexts.push({ text: apiEdgesText(payload.edges), name })
  let summary
  if (netTexts.length) {
    const directed = payload.directed === true
    const nets = netTexts.map((t, i) =>
      addNormaEntry(
        'network',
        t.name || (netTexts.length > 1 ? `${name} ${i + 1}` : name),
        t.text,
        '',
        null,
        { directed }
      )
    )
    // node attributes
    if (Array.isArray(payload.nodes)) {
      const attrs = {}
      payload.nodes.forEach((n) => {
        if (n && n.id != null) {
          const { id, ...rest } = n
          attrs[apiNode(id)] = rest
        }
      })
      nets.forEach((e) => {
        e.nodeAttrs = attrs
      })
    }
    const anns = []
    apiTextOf(files.annotation).forEach((t, i) =>
      anns.push(
        addNormaEntry('annotation', t.name || `${name}: grouping ${i + 1}`, t.text, '', null, {})
      )
    )
    if (payload.groups) {
      const g = apiGroupsText(payload.groups)
      const e = addNormaEntry('annotation', `${name}: groups`, g.text, '', null, {})
      e.groupMeta = g.meta
      anns.push(e)
    }
    ;(Array.isArray(payload.annotations) ? payload.annotations : []).forEach((a, i) => {
      const g = apiGroupsText(a.groups || {})
      const e = addNormaEntry(
        'annotation',
        `${name}: ${apiSafe(a.name) || `grouping ${i + 1}`}`,
        g.text,
        '',
        null,
        {}
      )
      e.groupMeta = g.meta
      anns.push(e)
    })
    let col = null
    const exprText =
      apiTextOf(files.expression)[0] ||
      (payload.expression ? { text: apiExpressionText(payload.expression) } : null)
    if (exprText && exprText.text)
      col = addNormaEntry(
        'colors',
        `${name}: ${payload.expression && typeof Object.values(payload.expression)[0] !== 'string' ? 'values' : 'colors'}`,
        exprText.text,
        '',
        null,
        {}
      )
    openInNewView(name, () => {
      libSelection.networks = new Set(nets.map((e) => e.id))
      libSelection.annotation = anns[0] ? anns[0].id : ''
      libSelection.colors = col ? col.id : ''
      renderLibraryLists()
      refreshLibraryView()
    })
    summary = {
      kind: 'files',
      view: name,
      networks: nets.length,
      groupings: anns.length,
      expression: !!col,
      notes: [...nets, ...anns, ...(col ? [col] : [])].flatMap((e) => e.parsed.notes || []),
    }
  } else if (payload.network || Array.isArray(payload.nodes)) {
    const data = payload.network || payload
    if (!Array.isArray(data.nodes) || !Array.isArray(data.edges))
      throw new Error('A NORMA view needs "nodes" and "edges" lists (see the JSON format in Help).')
    openInNewView(name, () => loadData(data))
    summary = { kind: 'view', view: name }
  } else {
    throw new Error(
      'Nothing to show: give "edges", "files.network", "network" (a NORMA view) or a session.'
    )
  }
  if (settings) {
    applyConfig(settings)
    if (settings.theme) applyTheme(settings.theme)
    if (settings.layout) {
      setLayoutMode('connections')
      document.getElementById('layoutSelect').value = settings.layout
      runLayout(settings.layout)
    }
  }
  switchTab(payload.tab === '3d' ? 'network3d' : 'network')
  summary.nodes = cy.nodes().length
  summary.edges = cy.edges().length
  summary.groups = getUsedGroups().filter((g) => g !== UNGROUPED).length
  setStatus('apiStatus', [
    {
      level: 'ok',
      text: `${origin}: opened "${name}" with ${plural(summary.nodes, 'node')}, ${plural(summary.edges, 'edge')} and ${plural(summary.groups, 'group')}.`,
    },
  ])
  toast(`Opened "${name}" from ${origin}.`)
  return summary
}

export function b64urlEncode(text) {
  const bytes = new TextEncoder().encode(text)
  let bin = ''
  bytes.forEach((b) => {
    bin += String.fromCharCode(b)
  })
  return btoa(bin).replace(/\+/g, '-').replace(/\//g, '_').replace(/=+$/, '')
}

function b64urlDecode(s) {
  const bin = atob(s.replace(/-/g, '+').replace(/_/g, '/') + '==='.slice((s.length + 3) % 4))
  return new TextDecoder().decode(Uint8Array.from(bin, (c) => c.charCodeAt(0)))
}

async function apiFetchText(url) {
  const r = await fetch(url)
  if (!r.ok) throw new Error(`${url} answered ${r.status}.`)
  return r.text()
}

// what the address asks for
export async function apiFromLocation() {
  const q = new URLSearchParams(location.search)
  const hash = new URLSearchParams(location.hash.replace(/^#/, ''))
  try {
    if (q.get('example')) {
      const sel = document.getElementById('sampleSelect')
      const key = q.get('example')
      if (![...sel.options].some((o) => o.value === key))
        throw new Error(`there is no example called "${key}".`)
      sel.value = key
      document.getElementById('btnSample').click()
      switchTab(q.get('tab') === '3d' ? 'network3d' : 'network')
      return { kind: 'example', example: key }
    }
    if (q.get('session')) {
      const token = q.get('session')
      const r = await fetch(`api/session/${encodeURIComponent(token)}`)
      if (!r.ok)
        throw new Error(
          r.status === 404
            ? 'This link has expired or does not exist on this server.'
            : `The server answered ${r.status}.`
        )
      return await normaApiLoad(await r.text(), 'a shared link')
    }
    if (hash.get('json')) {
      let text
      try {
        text = b64urlDecode(hash.get('json'))
      } catch (e) {
        throw new Error(
          'the data part of the link (after #json=) is damaged; it may have been cut when the link was copied.'
        )
      }
      return await normaApiLoad(text, 'the link')
    }
    if (q.get('data')) return await normaApiLoad(await apiFetchText(q.get('data')), q.get('data'))
    if (q.get('network')) {
      const files = { network: await apiFetchText(q.get('network')) }
      if (q.get('annotation')) files.annotation = await apiFetchText(q.get('annotation'))
      if (q.get('expression')) files.expression = await apiFetchText(q.get('expression'))
      const settings = {}
      if (q.get('layout')) settings.layout = q.get('layout')
      if (q.get('theme')) settings.theme = q.get('theme')
      return await normaApiLoad(
        { name: q.get('name') || 'Linked network', files, settings, tab: q.get('tab') || '' },
        'the link'
      )
    }
  } catch (err) {
    switchTab('api')
    setStatus('apiStatus', [
      { level: 'error', text: `The data in this link could not be opened: ${err.message}` },
    ])
  }
  return null
}

export function apiAnnounceReady() {
  const msg = { type: 'norma:ready', version: NORMA_API_VERSION }
  try {
    if (window.opener) window.opener.postMessage(msg, '*')
  } catch (e) {}
  try {
    if (window.parent && window.parent !== window) window.parent.postMessage(msg, '*')
  } catch (e) {}
}

// Options in a native <select> can't be text-selected by dragging, so the
// "Copy selected" buttons next to the Reactome/NDEx/GO-CAM pickers go
// through the clipboard directly. A permission prompt some browsers/contexts
// show for the async Clipboard API can sit unanswered indefinitely (e.g. an
// embedded or automated page with nobody to click it), so that path is
// capped with a short timeout and falls back to execCommand, which works
// off the same click without ever prompting.
function execCommandCopy(text) {
  const ta = document.createElement('textarea')
  ta.value = text
  ta.style.position = 'fixed'
  ta.style.opacity = '0'
  document.body.appendChild(ta)
  ta.focus()
  ta.select()
  let ok = false
  try {
    ok = document.execCommand('copy')
  } catch (e) {
    ok = false
  }
  document.body.removeChild(ta)
  return ok
}

async function copyText(text) {
  try {
    await Promise.race([
      navigator.clipboard.writeText(text),
      new Promise((_, reject) => setTimeout(() => reject(new Error('timed out')), 1200)),
    ])
    return true
  } catch (e) {
    return execCommandCopy(text)
  }
}

// The confirmation flashes on the button itself (like the API tab's "Copy"
// buttons), rather than the usual toast() - the toast lands on #canvas,
// which is hidden while the Welcome/Profiler/other tabs are the one shown,
// so a person copying a picker result before ever opening a network would
// never see it.
async function copySelectedOption(buttonId, selectId) {
  const btn = document.getElementById(buttonId)
  const sel = document.getElementById(selectId)
  const opt = sel.options[sel.selectedIndex]
  const original = btn.textContent
  const flash = (text) => {
    btn.textContent = text
    setTimeout(() => {
      btn.textContent = original
    }, 1200)
  }
  // A freshly populated multi-row <select> isn't guaranteed to start with
  // an option selected (unlike a plain dropdown), so clicking this before
  // choosing one is a real click-through, not just a shouldn't-happen case.
  if (!opt) return flash('Choose one first')
  const ok = await copyText(opt.text)
  flash(ok ? 'Copied' : "Couldn't copy")
}

// page wiring, run by main.ts in the original order
export function init() {
  /* ---------- wiring ---------- */
  ;(function setupDbImporters() {
    const rs = document.getElementById('reactomeSpecies')
    REACTOME_SPECIES.forEach((s) => rs.add(new Option(s, s)))
    const bind = (id, fn) => document.getElementById(id).addEventListener('click', fn)
    bind('btnReactomeSearch', reactomeSearch)
    bind('btnReactomeFetch', reactomeFetch)
    bind('btnReactomeCopy', () => copySelectedOption('btnReactomeCopy', 'reactomePathway'))
    bind('btnOmnipathFetch', omnipathFetch)
    bind('btnNdexSearch', ndexSearch)
    bind('btnNdexFetch', ndexFetch)
    bind('btnNdexCopy', () => copySelectedOption('btnNdexCopy', 'ndexNetwork'))
    bind('btnIntactFetch', intactFetch)
    bind('btnGoModels', goLoadModels)
    bind('btnGoModelFetch', goFetchModel)
    bind('btnGoCopy', () => copySelectedOption('btnGoCopy', 'goModel'))
    bind('btnGoAnnotate', goAnnotateView)
    document.getElementById('goModelFilter').addEventListener('input', filterGoModels)
    const sc = document.getElementById('intactScore')
    sc.addEventListener('input', () => {
      document.getElementById('intactScoreValue').textContent = parseFloat(sc.value).toFixed(2)
    })
    ;['reactome', 'omnipath', 'ndex', 'intact', 'go'].forEach((k) =>
      bind(`${k}Cancel`, () => dbCancel(k))
    )
    document.getElementById('reactomeQuery').addEventListener('keydown', (e) => {
      if (e.key === 'Enter') {
        e.preventDefault()
        reactomeSearch()
      }
    })
    document.getElementById('ndexQuery').addEventListener('keydown', (e) => {
      if (e.key === 'Enter') {
        e.preventDefault()
        ndexSearch()
      }
    })
    // collapsible importer sections
    // every sidebar section opens and closes from its title
    document.querySelectorAll('.section.collapsible > h3').forEach((h) => {
      const toggle = () => {
        h.parentElement.classList.toggle('collapsed')
        h.setAttribute('aria-expanded', String(!h.parentElement.classList.contains('collapsed')))
      }
      h.addEventListener('click', (e) => {
        if (!e.target.closest('button, a, input, select')) toggle()
      })
      h.addEventListener('keydown', (e) => {
        if (e.target !== h || (e.key !== 'Enter' && e.key !== ' ')) return
        e.preventDefault()
        toggle()
      })
    })
    document
      .querySelectorAll('.section.collapsible.collapsed > h3')
      .forEach((h) => h.setAttribute('aria-expanded', 'false'))
  })()

  // postMessage
  window.addEventListener('message', async (e) => {
    const m = e.data
    if (!m || typeof m !== 'object' || typeof m.type !== 'string' || !m.type.startsWith('norma:'))
      return
    const reply = (obj) => {
      try {
        ;(e.source || window.parent).postMessage(
          { ...obj, requestId: m.requestId },
          e.origin === 'null' ? '*' : e.origin
        )
      } catch (err) {}
    }
    if (m.type === 'norma:ping') {
      reply({ type: 'norma:ready', version: NORMA_API_VERSION })
      return
    }
    if (m.type !== 'norma:load') return
    try {
      const summary = await normaApiLoad(
        m.payload,
        e.origin && e.origin !== 'null' ? e.origin : 'another page'
      )
      reply({ type: 'norma:loaded', ok: true, summary })
    } catch (err) {
      reply({ type: 'norma:loaded', ok: false, error: err.message })
    }
  })
}
