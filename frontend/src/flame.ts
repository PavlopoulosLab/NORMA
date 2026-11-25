// @ts-nocheck
/* ---------- Export to Flame (functional enrichment of gene lists) ----------
   Flame (flame-enrich.org) runs enrichment on up to 10 gene lists of up to
   3,000 identifiers each. A dialog first asks whether to send all nodes of
   the network as one list or one list per group of its annotation (and
   whether to keep only the shown part). The lists then go:
   - through this server's relay (/flame-api/legacy), which posts the named
     lists to Flame's /api/v1/legacy and gets back a link to a new Flame
     workspace holding them (Flame's API does not allow direct browser
     calls from other sites);
   - without a server, through Flame's own link form
     (?url_genes=A,B;C,D&organism=9606), which keeps the lists but not their
     names, as long as the link stays short enough for a browser to open.
   Node names (their identifiers in the network) are sent as they are;
   Flame converts gene symbols, UniProt, Ensembl and other identifiers. */
import { cy } from './cy'
import { NORMA_CFG } from './config'
import { downloadText, fileStem, plural, setStatus } from './layouts/controls'
import { shownNodes } from './metrics'
import { effectiveGroupsFor, groupLabel, UNGROUPED } from './network_state'
import { activeView } from './profiler'
import { resolveStringRoute } from './string/requests'

export const FLAME_MAX_LISTS = 10
export const FLAME_MAX_IDS = 3000
const FLAME_BASE = 'https://flame-enrich.org'
const MAX_LINK = 7500 // characters a link can safely carry
const STORE_KEY = 'norma3-flame-organism'

// one identifier per node; separators Flame uses are removed
const idOf = (n) =>
  String(n.id())
    .replace(/[,;\s]+/g, ' ')
    .trim()

function viewName() {
  const v = typeof activeView === 'function' && activeView()
  return (v && v.name) || 'NORMA view'
}

// The gene lists to send, as [{ name, ids }], plus notes about what was cut:
// mode 'all' is every node as one list, 'groups' one list per group of the
// view's annotation; shownOnly keeps the ticked groups and channels only.
export function flameLists(mode, shownOnly = false) {
  const notes = []
  let lists = []
  const nodes = shownOnly ? shownNodes() : cy.nodes()
  if (mode === 'groups') {
    const members = new Map()
    nodes.forEach((n) => {
      const gs = shownOnly ? effectiveGroupsFor(n) : n.data('groups') || []
      gs.filter((g) => g !== UNGROUPED).forEach((g) => {
        if (!members.has(g)) members.set(g, [])
        members.get(g).push(idOf(n))
      })
    })
    lists = [...members].map(([g, ids]) => ({ name: groupLabel(g), ids }))
    lists.sort((a, b) => b.ids.length - a.ids.length)
  } else {
    lists = [{ name: viewName(), ids: nodes.map(idOf) }]
  }
  lists = lists
    .map((l) => ({ name: l.name, ids: [...new Set(l.ids.filter(Boolean))] }))
    .filter((l) => l.ids.length)
  if (lists.length > FLAME_MAX_LISTS) {
    notes.push(
      `Flame takes up to ${FLAME_MAX_LISTS} lists, so the ${FLAME_MAX_LISTS} largest groups were sent (${plural(lists.length - FLAME_MAX_LISTS, 'group')} left out).`
    )
    lists = lists.slice(0, FLAME_MAX_LISTS)
  }
  lists.forEach((l) => {
    if (l.ids.length > FLAME_MAX_IDS) {
      notes.push(`"${l.name}" was cut to its first ${FLAME_MAX_IDS.toLocaleString()} genes.`)
      l.ids = l.ids.slice(0, FLAME_MAX_IDS)
    }
  })
  // list names must be unique for Flame
  const seen = new Map()
  lists.forEach((l) => {
    const base = l.name.slice(0, 60) || 'List'
    const k = (seen.get(base) || 0) + 1
    seen.set(base, k)
    l.name = k > 1 ? `${base} (${k})` : base
  })
  return { lists, notes }
}

// Flame's link form: lists separated by ";", identifiers by ","
export function flameLink(lists, organism) {
  const genes = lists.map((l) => l.ids.map(encodeURIComponent).join(',')).join(';')
  return `${FLAME_BASE}/?url_genes=${genes}&organism=${encodeURIComponent(organism)}`
}

function organismValue() {
  const raw = String(document.getElementById('flameOrganism').value || '').trim()
  const taxon = parseInt(raw, 10)
  if (!Number.isFinite(taxon) || taxon <= 0)
    throw new Error('Give the organism as an NCBI taxon ID, for example 9606 for human.')
  try {
    localStorage.setItem(STORE_KEY, String(taxon))
  } catch {
    /* private mode */
  }
  return taxon
}

function summary(lists) {
  const n = lists.reduce((a, l) => a + l.ids.length, 0)
  return `${plural(lists.length, 'list')} with ${plural(n, 'gene')}`
}

function dialogChoice() {
  const mode = document.querySelector('input[name="flameMode"]:checked').value
  const shownOnly = document.getElementById('flameShownOnly').checked
  return { mode, shownOnly }
}

// Fills in the counts next to each choice, then asks the user
export function askFlame() {
  if (!cy.nodes().length) {
    setStatus('flameStatus', [{ level: 'error', text: 'Show a network first.' }])
    return
  }
  const refresh = () => {
    const { shownOnly } = dialogChoice()
    const all = flameLists('all', shownOnly).lists
    const groups = flameLists('groups', shownOnly)
    const n = all.length ? all[0].ids.length : 0
    document.getElementById('flameAllInfo').textContent = `${plural(n, 'gene')} in one list`
    const g = groups.lists.length
    document.getElementById('flameGroupsInfo').textContent = g
      ? `${plural(g, 'list')}, one per group of the annotation${groups.notes.length ? ' (largest 10 sent)' : ''}`
      : 'this view has no groups'
    const radio = document.querySelector('input[name="flameMode"][value="groups"]')
    radio.disabled = !g
    if (!g && radio.checked)
      document.querySelector('input[name="flameMode"][value="all"]').checked = true
  }
  refresh()
  const dlg = document.getElementById('flameDialog')
  dlg.dataset.bound ||
    (document.getElementById('flameShownOnly').addEventListener('change', refresh),
    (dlg.dataset.bound = '1'))
  if (typeof dlg.showModal === 'function') dlg.showModal()
  else dlg.setAttribute('open', '')
}

export async function openInFlame() {
  const btn = document.getElementById('btnFlameSend')
  const dlg = document.getElementById('flameDialog')
  let organism, built
  try {
    organism = organismValue()
    const { mode, shownOnly } = dialogChoice()
    built = flameLists(mode, shownOnly)
    if (!built.lists.length) throw new Error('There are no genes to send.')
  } catch (err) {
    setStatus('flameStatus', [{ level: 'error', text: err.message }])
    return
  }
  if (dlg.open) dlg.close()
  const { lists, notes } = built
  // open the tab while the click still counts as a user action
  const tab = window.open('', '_blank')
  if (tab) {
    try {
      tab.document.write(
        '<title>Opening Flame…</title><p style="font-family:sans-serif;padding:2em">Sending the gene lists to Flame…</p>'
      )
    } catch {
      /* cross-origin or blocked */
    }
  }
  const go = (url, how) => {
    if (tab && !tab.closed) tab.location.href = url
    else window.open(url, '_blank')
    setStatus('flameStatus', [
      { level: 'ok', text: `Opened in Flame ${how}: ${summary(lists)}.` },
      ...notes.map((t) => ({ level: 'warn', text: t })),
      {
        level: 'ok',
        text: 'If no new tab appeared, open the link:',
        action: { label: 'Open Flame', run: () => window.open(url, '_blank') },
      },
    ])
  }
  btn.disabled = true
  setStatus('flameStatus', [
    { level: 'busy', text: `Sending ${summary(lists)} to Flame…`, progress: null },
  ])
  try {
    const route = NORMA_CFG.features.relays.flame === false ? 'direct' : await resolveStringRoute()
    if (route === 'proxy') {
      const body = { organism }
      lists.forEach((l) => {
        body[l.name] = l.ids
      })
      const r = await fetch(`flame-api/legacy?upstream=${encodeURIComponent(FLAME_BASE)}`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(body),
      })
      const text = await r.text()
      let answer = null
      try {
        answer = JSON.parse(text)
      } catch {
        /* not JSON */
      }
      if (r.ok && answer && answer.url) return go(answer.url, 'with the list names')
      // no relay on this server, or Flame refused: fall back to the link
    }
    const link = flameLink(lists, organism)
    if (link.length > MAX_LINK)
      throw new Error(
        `These lists are too long for a link (${link.length.toLocaleString()} characters). Run NORMA with server.py to send them directly, or save the lists and paste them into Flame.`
      )
    go(link, 'through a link (list names are not kept)')
  } catch (err) {
    if (tab && !tab.closed) tab.close()
    setStatus('flameStatus', [
      { level: 'error', text: err.message },
      {
        level: 'ok',
        text: 'You can still save the lists and load them in Flame.',
        action: { label: 'Save gene lists', run: saveFlameLists },
      },
    ])
  } finally {
    btn.disabled = false
  }
}

// One list per line block: "# name" then one identifier per line
export function saveFlameLists() {
  const { mode, shownOnly } = dialogChoice()
  const { lists, notes } = flameLists(mode, shownOnly)
  const dlg = document.getElementById('flameDialog')
  if (dlg.open) dlg.close()
  if (!lists.length) {
    setStatus('flameStatus', [{ level: 'error', text: 'There are no genes to save.' }])
    return
  }
  const text = lists.map((l) => `# ${l.name}\n${l.ids.join('\n')}`).join('\n\n') + '\n'
  const name = `flame-lists-${fileStem(viewName())}.txt`
  downloadText(name, text)
  setStatus('flameStatus', [
    { level: 'ok', text: `Saved ${name}: ${summary(lists)}.` },
    ...notes.map((t) => ({ level: 'warn', text: t })),
  ])
}

export function init() {
  const org = document.getElementById('flameOrganism')
  try {
    const saved = localStorage.getItem(STORE_KEY)
    if (saved) org.value = saved
  } catch {
    /* private mode */
  }
  document.getElementById('btnFlameOpen').addEventListener('click', askFlame)
  document.getElementById('btnFlameSend').addEventListener('click', openInFlame)
  document.getElementById('btnFlameSave').addEventListener('click', saveFlameLists)
  document
    .getElementById('btnFlameCancel')
    .addEventListener('click', () => document.getElementById('flameDialog').close())
}
