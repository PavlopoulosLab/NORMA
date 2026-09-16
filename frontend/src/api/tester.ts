// @ts-nocheck
// ponytail: split verbatim from the former single-file script; typed one file at a time (remove @ts-nocheck as it lands)
import apiClientTemplate from '../../../norma_api_client.py?raw'
import {
  MAX_NETWORK_NODES,
  escapeHtml,
  getUsedGroups,
  refreshNodeVisual,
  sanitizeColor,
} from '../network_state'
import { NORMA_CFG } from '../config'
import { S } from '../state'
import {
  activeView,
  captureActiveView,
  closeActiveView,
  nameForSelection,
  profilerState,
  renderProfilerNetworkList,
  renderProfilerResults,
  renderViewBar,
  restoreView,
  uniqueViewName,
  views,
} from '../profiler'
import { applyGroupVisibility, buildGroupLegend, buildLegend, getUsedTypes } from '../hulls'
import { applyTypeVisibility } from '../parallel_edges'
import { b64urlEncode, normaApiLoad } from './wiring'
import { compareState, renderCompareList } from '../label_colors'
import { currentTab } from '../wiring'
import { cy } from '../cy'
import { dataVersion, setHistoryBaseline, updateUndoButtons } from '../demo_downloads'
import {
  downloadText,
  libEntry,
  libSelection,
  normaLibrary,
  plural,
  renderLibraryLists,
  setStatus,
} from '../layouts/controls'
import { refreshEnrichmentChoices, toast } from '../enrichment'
import { refreshLibraryView } from '../library'
import { renderGroupingSelect, updateContextInfo } from '../recording'
import { updateEmptyState } from '../welcome'

/* ---------- the API tab's tester ---------- */
const API_EXAMPLE = {
  name: 'API example',
  edges: [
    { source: 'TP53', target: 'MDM2', type: 'binding', weight: 0.9 },
    { source: 'MDM2', target: 'TP53', type: 'ubiquitination', directed: true },
    { source: 'TP53', target: 'CDKN1A', type: 'expression', directed: true },
    { source: 'CDKN1A', target: 'CDK2', type: 'inhibition', directed: true },
    { source: 'CDK2', target: 'CCNE1', type: 'binding' },
    { source: 'ATM', target: 'TP53', type: 'phosphorylation', directed: true },
    { source: 'ATM', target: 'CHEK2', type: 'phosphorylation', directed: true },
    { source: 'CHEK2', target: 'TP53', type: 'phosphorylation', directed: true },
  ],
  groups: [
    { name: 'DNA damage sensing', members: ['ATM', 'CHEK2'], color: '#8b5cf6' },
    {
      name: 'p53 core',
      members: ['TP53', 'MDM2'],
      color: '#f59e0b',
      description: 'p53 and its E3 ligase',
    },
    { name: 'Cell cycle', members: ['CDKN1A', 'CDK2', 'CCNE1', 'TP53'] },
  ],
  expression: { TP53: 1.8, MDM2: 1.2, CDKN1A: 2.6, CDK2: -1.1, CCNE1: -0.7, ATM: 0.3, CHEK2: 0.4 },
  settings: {
    edgeDirection: 'data',
    edgeCurveStyle: 'bezier',
    showGroupHulls: true,
    hullStyle: 'bubble',
    legendShow: true,
    layout: 'fr',
  },
}

function apiPayloadFromBox() {
  const text = document.getElementById('apiPayload').value
  try {
    return JSON.parse(text)
  } catch (e) {
    throw new Error(`The payload is not valid JSON: ${e.message}`)
  }
}

function apiBase() {
  return location.href.replace(/[?#].*$/, '')
}

/* ============================================================
   SITE INFORMATION (licence, access, privacy, contact)
   Shown on the Welcome page, in About and as an optional notice, from
   the settings (see Help: Running NORMA locally or on a server).
   ============================================================ */
function siteLink(url, text) {
  return url
    ? `<a href="${escapeHtml(url)}" target="_blank" rel="noopener">${escapeHtml(text)}</a>`
    : escapeHtml(text)
}

export function renderSiteInfo() {
  const S = NORMA_CFG.site
  const hosted = NORMA_CFG.mode === 'hosted'
  const contact = S.contactEmail
    ? `<a href="mailto:${escapeHtml(S.contactEmail)}">${escapeHtml(S.contactEmail)}</a>`
    : siteLink(S.contactUrl, S.contactName || 'contact')
  const foot = document.getElementById('siteFooter')
  if (foot) {
    foot.innerHTML = [
      `<span><b>Free and open to all users.</b> No login, registration or e-mail address is needed.</span>`,
      `<span>No cookies and no tracking; your data stays in your browser (<a href="#about-privacy">privacy</a>).</span>`,
      `<span>Licence: ${siteLink(S.licenceUrl, S.licenceName || 'MIT License')}</span>`,
      `<span>${siteLink(S.sourceUrl, 'Source code')}</span>`,
      `<span>Contact: ${contact}</span>`,
      `<span><a href="#api-rest">Programmatic access</a>: <a href="#" data-download-template>Python template</a></span>`,
      S.privacyUrl ? `<span>${siteLink(S.privacyUrl, 'Privacy policy')}</span>` : '',
      S.imprintUrl ? `<span>${siteLink(S.imprintUrl, 'Imprint')}</span>` : '',
    ]
      .filter(Boolean)
      .join('<span class="dot">·</span>')
  }
  const box = document.getElementById('aboutSiteInfo')
  if (box) {
    const browsers =
      Array.isArray(S.testedBrowsers) && S.testedBrowsers.length ? S.testedBrowsers.join(', ') : ''
    box.innerHTML = `
      <li><b>Running here:</b> ${hosted ? `a public NORMA server${NORMA_CFG.publicUrl ? ` at ${siteLink(NORMA_CFG.publicUrl, NORMA_CFG.publicUrl)}` : ''}` : NORMA_CFG.served ? 'a NORMA copy on this computer or network' : 'a NORMA file opened on this computer'}${S.institution ? `, provided by ${escapeHtml(S.institution)}` : ''}. NORMA ${escapeHtml(NORMA_CFG.version || '3.0')}.</li>
      <li><b>Contact:</b> ${contact}${S.contactEmail && S.contactUrl ? ` · ${siteLink(S.contactUrl, 'issue tracker')}` : ''}.</li>
      <li><b>Maintenance:</b> ${S.maintainedUntil ? `this server is maintained at least until ${escapeHtml(S.maintainedUntil)}.` : 'NORMA is maintained by the Pavlopoulos Lab; the source code stays available in its public repository.'}</li>
      <li><b>Browsers:</b> NORMA is made for current versions of Chrome, Edge, Firefox and Safari on Windows, macOS and Linux, and needs no plug-ins or installation.${browsers ? ` Tested on this server with ${escapeHtml(browsers)}.` : ''}</li>`
  }
  if (S.notice) {
    const bar = document.getElementById('siteNotice')
    bar.querySelector('span').textContent = S.notice
    bar.hidden = false
  }
  document.querySelectorAll('[data-download-template]').forEach((a) =>
    a.addEventListener('click', (e) => {
      e.preventDefault()
      downloadApiTemplate()
    })
  )
  const wf = document.querySelector('.welcome-foot')
  if (wf)
    wf.textContent = `NORMA ${NORMA_CFG.version || '3.0'} runs in your browser: your files stay on this computer, except what you send to online databases or Arena3D yourself. Networks of up to ${MAX_NETWORK_NODES.toLocaleString('en-US')} nodes.`
  // REST API availability
  const restBtn = document.getElementById('btnApiServer')
  const setRest = (on) => {
    restBtn.disabled = !on
    restBtn.title = on
      ? ''
      : 'The REST API needs NORMA to run with server.py (with the API turned on).'
  }
  if (NORMA_CFG.features.restApi === false) setRest(false)
  else if (!NORMA_CFG.served) setRest(false)
  else if (NORMA_CFG.features.restApi !== true) {
    fetch('api/health')
      .then((r) => (r.ok ? r.json() : null))
      .then((j) => setRest(!!(j && j.status === 'ok')))
      .catch(() => setRest(false))
  }
}

function downloadApiTemplate() {
  const el = { textContent: apiClientTemplate }
  let text = el ? el.textContent : ''
  const base =
    NORMA_CFG.publicUrl ||
    (NORMA_CFG.served
      ? location.href.replace(/[?#].*$/, '').replace(/norma\.html$|index\.html$/, '')
      : '')
  if (base)
    text = text.replace('DEFAULT_SERVER = "http://localhost:8000/"', `DEFAULT_SERVER = "${base}"`)
  downloadText('norma_api_client.py', text)
}

/* ============================================================
   DELETING FILES
   A network, annotation or expression file can be deleted completely:
   it leaves Files, every view that shows it is updated (views left with
   no network become empty), undo can't bring it back, and results of
   the Network Profiler and Network Comparison that used it are removed.
   ============================================================ */
const KIND_WORD = { network: 'network', annotation: 'annotation', colors: 'expression file' }

const SEL_FIELD = { annotation: 'annotation', colors: 'colors' }

function libViewUses(lv, kind, id) {
  if (!lv) return false
  if (kind === 'network')
    return String(lv.nets || '')
      .split('|')
      .includes(id)
  return lv[SEL_FIELD[kind]] === id
}

// views that show a file (the current one as it is now, others as saved)
function viewsShowing(kind, id) {
  return views.filter((v) =>
    libViewUses(v.id === S.activeViewId ? S.currentLibView : v.state && v.state.libView, kind, id)
  )
}

export let nextLoadPositions = null

const KIND_TITLE = { network: 'network file', annotation: 'grouping', colors: 'expression file' }

// items: [{ kind, id }] files; opts.view: also offer to remove this view
export function askDelete(items, opts = {}) {
  const files = items
    .map(({ kind, id }) => ({ kind, id, entry: libEntry(kind, id) }))
    .filter((x) => x.entry)
  const choices = [
    ...(opts.view ? [{ type: 'view', view: opts.view }] : []),
    ...files.map((f) => ({ type: 'file', ...f })),
  ]
  if (!choices.length) return
  const dlg = document.getElementById('deleteDialog')
  const box = document.getElementById('deleteChoices')
  const single = choices.length === 1
  const label = (c) =>
    c.type === 'view' ? `the view "${c.view.name}"` : `the ${KIND_TITLE[c.kind]} "${c.entry.name}"`
  document.getElementById('deleteTitle').textContent = single
    ? `Are you sure you want to completely remove ${label(choices[0])}?`
    : opts.all
      ? `Are you sure you want to completely remove all ${plural(files.length, 'file')}?`
      : 'Are you sure you want to completely remove these?'
  box.innerHTML = ''
  box.hidden = single || opts.all
  if (!box.hidden) {
    choices.forEach((c, i) => {
      const row = document.createElement('label')
      const cb = document.createElement('input')
      cb.type = 'checkbox'
      cb.checked = true
      cb.dataset.index = i
      const others =
        c.type === 'file'
          ? viewsShowing(c.kind, c.id).filter((v) => !opts.view || v.id !== opts.view.id)
          : []
      const note =
        c.type === 'view'
          ? 'closes the view; its display settings and positions are lost'
          : others.length
            ? `also shown in ${others.map((v) => `"${v.name}"`).join(', ')}, which will be updated`
            : 'removed from Files and every menu'
      const text = document.createElement('span')
      text.innerHTML = `${escapeHtml(
        label(c)
          .replace(/^the /, '')
          .replace(/^./, (ch) => ch.toUpperCase())
      )}<small>${escapeHtml(note)}</small>`
      row.append(cb, text)
      box.appendChild(row)
      cb.addEventListener('change', update)
    })
  }
  const confirm = document.getElementById('btnDeleteConfirm')
  function picked() {
    if (box.hidden) return choices
    return [...box.querySelectorAll('input')]
      .filter((x) => x.checked)
      .map((x) => choices[+x.dataset.index])
  }
  function update() {
    const p = picked()
    const pf = p.filter((c) => c.type === 'file')
    const shown = new Map()
    pf.forEach(({ kind, id }) => viewsShowing(kind, id).forEach((v) => shown.set(v.id, v)))
    if (opts.view && p.some((c) => c.type === 'view')) shown.delete(opts.view.id)
    const lines = []
    if (pf.length)
      lines.push(
        `${pf.length === 1 ? 'The file' : 'The files'} will be removed from Files, from the Grouping menu and every other list, together with everything NORMA read from ${pf.length === 1 ? 'it' : 'them'}.`
      )
    if (shown.size) {
      lines.push(
        `${shown.size === 1 ? 'This view shows' : 'These views show'} ${pf.length === 1 ? 'it' : 'them'} and will be updated: ${[...shown.values()].map((v) => `"${v.name}"`).join(', ')}. ${pf.some((c) => c.kind === 'network') ? 'A view left without networks becomes empty; other views keep their remaining networks and node positions.' : 'They keep their networks and node positions but lose these groups or colors.'}`
      )
    }
    if (p.some((c) => c.type === 'view'))
      lines.push(
        views.length === 1
          ? 'This is the only view; a new empty view takes its place.'
          : 'The view is closed and the previous view is shown.'
      )
    lines.push('This cannot be undone. Your files on disk are not touched.')
    document.getElementById('deleteText').innerHTML = lines
      .map((t) => `<p>${escapeHtml(t)}</p>`)
      .join('')
    confirm.disabled = !p.length
    confirm.textContent = p.length > 1 ? `Yes, remove ${p.length}` : 'Yes, remove'
  }
  update()
  confirm.onclick = () => {
    const p = picked()
    if (!p.length) return
    closeDeleteDialog()
    const pf = p.filter((c) => c.type === 'file').map(({ kind, id }) => ({ kind, id }))
    const closeView = p.some((c) => c.type === 'view') && opts.view
    if (pf.length) deleteLibEntries(pf)
    if (closeView) {
      if (S.activeViewId !== closeView.id && views.some((v) => v.id === closeView.id)) {
        captureActiveView()
        S.activeViewId = closeView.id
      }
      const name = closeView.name
      closeActiveView()
      renderLibraryLists()
      renderGroupingSelect()
      updateContextInfo()
      updateEmptyState()
      toast(`Removed the view "${name}"${pf.length ? ` and ${plural(pf.length, 'file')}` : ''}.`)
    }
  }
  if (typeof dlg.showModal === 'function') dlg.showModal()
  else dlg.setAttribute('open', '')
  document.getElementById('btnDeleteCancel').focus()
}

function closeDeleteDialog() {
  const dlg = document.getElementById('deleteDialog')
  if (typeof dlg.close === 'function' && dlg.open) dlg.close()
  else dlg.removeAttribute('open')
}

export function deleteLibEntries(items) {
  const entries = items
    .map(({ kind, id }) => ({ kind, id, entry: libEntry(kind, id) }))
    .filter((x) => x.entry)
  if (!entries.length) return
  captureActiveView()
  const affected = new Set()
  const lostNetwork = new Set()
  entries.forEach(({ kind, id }) => {
    views.forEach((v) => {
      const sel = v.selection || (v.selection = { networks: [], annotation: '', colors: '' })
      if (kind === 'network') sel.networks = (sel.networks || []).filter((x) => x !== id)
      else if (sel[SEL_FIELD[kind]] === id) sel[SEL_FIELD[kind]] = ''
      const lv = v.state && v.state.libView
      if (libViewUses(lv, kind, id)) {
        affected.add(v)
        if (kind === 'network') {
          lv.nets = String(lv.nets)
            .split('|')
            .filter((x) => x !== id)
            .join('|')
          lostNetwork.add(v)
        } else {
          lv[SEL_FIELD[kind]] = ''
        }
      }
    })
    normaLibrary[kind] = normaLibrary[kind].filter((e) => e.id !== id)
    // groupings fetched for a deleted network stay usable on their own
    if (kind === 'network')
      normaLibrary.annotation.forEach((e) => {
        if (e.forNetwork === id) delete e.forNetwork
      })
  })
  affected.forEach((v) => {
    v.history = { undo: [], redo: [], committed: null } // no way back to deleted data
    const lv = v.state.libView
    if (!lv.nets) {
      v.data = null
      if (v.autoName) v.name = uniqueViewName('Untitled view')
      v.state = { config: v.state.config || { ...S.DEFAULT_VIEW_CONFIG } }
      v.selection = { networks: [], annotation: '', colors: '' }
      v.needsRefresh = null
    } else {
      // the saved picture still holds the deleted data: drop it now and
      // rebuild the view from its remaining files when it is shown
      v.data = null
      v.needsRefresh = { reload: true }
    }
  })
  // drop analysis results that used deleted networks or changed views
  const deletedNets = new Set(entries.filter((e) => e.kind === 'network').map((e) => e.id))
  const staleView = (id) => [...affected].some((v) => v.id === id)
  let droppedResults = 0
  if (profilerState.results.length) {
    const keep = profilerState.results.filter(
      (r) =>
        !(
          deletedNets.has(r.sourceValue) ||
          (r.sourceValue === 'view' && affected.has(activeView()))
        )
    )
    droppedResults += profilerState.results.length - keep.length
    if (keep.length !== profilerState.results.length) {
      profilerState.results = keep
      if (keep.length) renderProfilerResults(keep)
      else {
        document.getElementById('profResults').innerHTML = ''
        document.getElementById('btnProfileTsv').disabled = true
      }
    }
  }
  if (compareState.results) {
    const used = compareState.results.some((r) =>
      (r.sourceValue || '').startsWith('lib:')
        ? deletedNets.has(r.sourceValue.slice(4))
        : staleView((r.sourceValue || '').slice(5))
    )
    if (used) {
      droppedResults += 1
      compareState.results = null
      document.getElementById('cmpResults').innerHTML = ''
      ;['btnCompareTsv', 'btnCompareNet'].forEach((b) => {
        document.getElementById(b).disabled = true
      })
      document.getElementById('cmpArena').hidden = true
    }
  }
  // show the current view again
  const act = activeView()
  S.historySuspended++
  try {
    restoreView(act)
  } finally {
    S.historySuspended--
  }
  S.dataCache = { version: dataVersion, data: act.data }
  setHistoryBaseline()
  renderViewBar()
  updateUndoButtons()
  renderLibraryLists()
  renderGroupingSelect()
  updateContextInfo()
  updateEmptyState()
  if (typeof refreshEnrichmentChoices === 'function') refreshEnrichmentChoices()
  if (currentTab === 'profiler') renderProfilerNetworkList()
  if (currentTab === 'compare') renderCompareList()
  const names = entries.map((e) => `"${e.entry.name}"`)
  const notes = [
    {
      level: 'ok',
      text: `Deleted ${entries.length === 1 ? names[0] : plural(entries.length, 'file')}${affected.size ? `; updated ${plural(affected.size, 'view')} that showed ${entries.length === 1 ? 'it' : 'them'}` : ''}.`,
    },
  ]
  if (droppedResults)
    notes.push({
      level: 'ok',
      text: 'Profiler or comparison results that used the deleted data were removed.',
    })
  setStatus('normaStatus', notes)
  toast(notes[0].text)
}

// Brings a view up to date after files it showed were deleted: rebuilds it
// from its remaining files with its saved node positions, group colors and
// ticked groups and channels.
export function refreshAfterDelete(v) {
  v.needsRefresh = null
  const st = v.state || {}
  const lv = st.libView
  if (!lv) return
  libSelection.networks = new Set(
    String(lv.nets)
      .split('|')
      .filter((id) => libEntry('network', id))
  )
  libSelection.annotation =
    lv.annotation && libEntry('annotation', lv.annotation) ? lv.annotation : ''
  libSelection.colors = lv.colors && libEntry('colors', lv.colors) ? lv.colors : ''
  if (!libSelection.networks.size) return
  nextLoadPositions = st.positions && Object.keys(st.positions).length ? st.positions : null
  S.currentLibView = null
  const status = document.getElementById('normaStatus').innerHTML
  try {
    refreshLibraryView()
  } finally {
    nextLoadPositions = null
  }
  document.getElementById('normaStatus').innerHTML = status
  // keep what the view looked like
  if (st.groupColors) {
    Object.entries(st.groupColors).forEach(([g, c]) => {
      if (S.nodeColorMap[g]) {
        const sc = sanitizeColor(c)
        if (sc) S.nodeColorMap[g] = sc
      }
    })
  }
  const used = new Set(getUsedGroups())
  if (Array.isArray(st.activeGroups)) {
    const was = new Set(st.activeGroups)
    used.forEach((g) => {
      if (g in (st.groupColors || {}) && !was.has(g)) S.activeGroups.delete(g)
    })
  }
  if (Array.isArray(st.activeTypes)) {
    const was = new Set(st.activeTypes)
    getUsedTypes().forEach((t) => {
      if (!was.has(t) && (st.typeColors || {})[t]) S.activeTypes.delete(t)
    })
  }
  cy.batch(() => cy.nodes().forEach((n) => refreshNodeVisual(n)))
  if (typeof applyGroupVisibility === 'function') applyGroupVisibility()
  if (typeof applyTypeVisibility === 'function') applyTypeVisibility()
  buildGroupLegend()
  buildLegend(getUsedTypes())
  if (st.positions && typeof st.zoom === 'number') cy.viewport({ zoom: st.zoom, pan: st.pan })
  v.selection = {
    networks: [...libSelection.networks],
    annotation: libSelection.annotation,
    colors: libSelection.colors,
  }
  if (v.autoName) {
    const base = nameForSelection()
    if (base && base !== v.name) v.name = uniqueViewName(base)
    renderViewBar()
  }
  captureActiveView()
}

// page wiring, run by main.ts in the original order
export function init() {
  ;(function setupApiTab() {
    const box = document.getElementById('apiPayload')
    box.value = JSON.stringify(API_EXAMPLE, null, 2)
    document.getElementById('btnApiReset').addEventListener('click', () => {
      box.value = JSON.stringify(API_EXAMPLE, null, 2)
    })
    document.getElementById('btnApiLoad').addEventListener('click', async () => {
      try {
        await normaApiLoad(apiPayloadFromBox(), 'the API tab')
      } catch (err) {
        setStatus('apiStatus', [{ level: 'error', text: err.message }])
      }
    })
    document.getElementById('btnApiLink').addEventListener('click', async () => {
      try {
        const url = `${apiBase()}#json=${b64urlEncode(JSON.stringify(apiPayloadFromBox()))}`
        document.getElementById('apiResultUrl').value = url
        const note =
          url.length > 8000
            ? ' This link is long; browsers and chat programs may cut it. For larger networks use the server (Send to server) or a data URL.'
            : ''
        setStatus('apiStatus', [
          {
            level: url.length > 8000 ? 'warn' : 'ok',
            text: `Made a link of ${url.length.toLocaleString()} characters that opens this payload.${note}`,
          },
        ])
        try {
          await navigator.clipboard.writeText(url)
        } catch (e) {}
      } catch (err) {
        setStatus('apiStatus', [{ level: 'error', text: err.message }])
      }
    })
    document.getElementById('btnApiServer').addEventListener('click', async () => {
      try {
        const body = JSON.stringify(apiPayloadFromBox())
        const r = await fetch('api/external', {
          method: 'POST',
          headers: { 'Content-Type': 'application/json' },
          body,
        })
        const text = await r.text()
        let j
        try {
          j = JSON.parse(text)
        } catch (e) {
          throw new Error(
            'This page is not served by server.py, so the REST API is not available here. Run NORMA with server.py, or use Make a link.'
          )
        }
        if (!r.ok) throw new Error(j.message || `The server answered ${r.status}.`)
        document.getElementById('apiResultUrl').value = j.url
        setStatus('apiStatus', [
          {
            level: 'ok',
            text: `The server stored the payload (token ${j.token}, kept for ${j.expiresInHours} hours).`,
            action: { label: 'Open link', run: () => window.open(j.url, '_blank') },
          },
        ])
      } catch (err) {
        setStatus('apiStatus', [
          {
            level: 'error',
            text: err.message.includes('fetch')
              ? 'This page is not served by server.py, so the REST API is not available here.'
              : err.message,
          },
        ])
      }
    })
    document.getElementById('btnApiCopy').addEventListener('click', async () => {
      const v = document.getElementById('apiResultUrl').value
      if (!v) return
      try {
        await navigator.clipboard.writeText(v)
        toast('Copied the link.')
      } catch (e) {
        document.getElementById('apiResultUrl').select()
      }
    })
    // show the address this copy of NORMA answers on
    const served = /^https?:$/.test(location.protocol)
    const base = served
      ? apiBase().replace(/norma\.html$|index\.html$/, '')
      : 'http://localhost:8000/'
    document.querySelectorAll('[data-api-base]').forEach((el) => {
      el.textContent = base
    })
    if (!served) {
      document.querySelector('.api-base').innerHTML =
        'This copy of NORMA was opened from a file, so the examples use <code>http://localhost:8000/</code>, where <code>server.py</code> runs by default. Links with <code>#json=</code> and postMessage work from a file too.'
    }
    document.querySelectorAll('.api-code').forEach((pre) => {
      const btn = document.createElement('button')
      btn.type = 'button'
      btn.className = 'api-copy'
      btn.textContent = 'Copy'
      btn.addEventListener('click', async () => {
        try {
          await navigator.clipboard.writeText(pre.innerText)
          btn.textContent = 'Copied'
          setTimeout(() => {
            btn.textContent = 'Copy'
          }, 1200)
        } catch (e) {}
      })
      pre.parentElement.insertBefore(btn, pre)
    })
  })()

  document.getElementById('siteNoticeClose').addEventListener('click', () => {
    document.getElementById('siteNotice').hidden = true
  })

  document.getElementById('btnDeleteCancel').addEventListener('click', closeDeleteDialog)
}
