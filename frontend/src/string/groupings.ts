// @ts-nocheck
// ponytail: split verbatim from the former single-file script; typed one file at a time (remove @ts-nocheck as it lands)
import { STRING_CATEGORIES, stringCategoryKey, stringCategoryName } from '../view3d/export'
import {
  addNormaEntry,
  libSelection,
  plural,
  renderLibraryLists,
  setStatus,
  startProgress,
} from '../layouts/controls'
import { refreshLibraryView } from '../library'
import { stringCall, stringState } from './requests'
import { stringNetworkInView, stringSettings, updateStringUI } from './ui_state'

/* ---------- groupings ---------- */
// task/step: the import's progress; on its own, it shows a progress of its own.
export async function fetchStringGroupings(netEntry, st, notes, task = null, step = 0) {
  const meta = netEntry.stringMeta
  const nameOf = new Map(Object.entries(meta.nameOf))
  const byPreferred = new Map([...nameOf.values()].map((n) => [n.toLowerCase(), n]))
  const ids = meta.stringIds
  const enrichment = st.annotMode === 'enrichment'
  const own = !task
  if (own) {
    task = startProgress('stringStatus', [1])
    step = 0
  }
  task.step(
    step,
    enrichment
      ? `Running functional enrichment for ${plural(ids.length, 'protein')}…`
      : `Fetching functional annotations for ${plural(ids.length, 'protein')}…`
  )
  let rows
  try {
    rows = await stringCall(
      enrichment ? 'enrichment' : 'functional_annotation',
      { identifiers: ids.join('\r'), species: meta.taxon },
      { onBytes: (got, total) => task.bytes(got, total) }
    )
  } catch (err) {
    notes.push({ level: 'warn', text: `Groupings couldn't be fetched: ${err.message}` })
    if (own) task.stop()
    return []
  }
  if (own) task.stop()
  const wanted = new Set(st.categories)
  const byCat = new Map()
  const seenCats = new Map()
  ;(rows || []).forEach((r) => {
    const cat = stringCategoryKey(r.category)
    seenCats.set(cat, (seenCats.get(cat) || 0) + 1)
    const known = STRING_CATEGORIES.some((c) => c[0] === cat)
    if (!(wanted.has(cat) || (!known && st.otherCategories))) return
    if (enrichment && !(Number(r.fdr) <= st.fdr)) return
    // members: map STRING ids (inputGenes) or names back to node names
    const inputs = Array.isArray(r.inputGenes)
      ? r.inputGenes
      : String(r.inputGenes || '').split(',')
    const names = Array.isArray(r.preferredNames)
      ? r.preferredNames
      : String(r.preferredNames || '').split(',')
    const members = new Set()
    inputs.forEach((g, i) => {
      const n =
        nameOf.get(g) ||
        byPreferred.get(String(names[i] || g).toLowerCase()) ||
        byPreferred.get(String(g).toLowerCase())
      if (n) members.add(n)
    })
    if (members.size < st.minGroup) return
    if (!byCat.has(cat)) byCat.set(cat, [])
    byCat.get(cat).push({
      term: r.term,
      description: r.description || r.term,
      members: [...members],
      fdr: Number(r.fdr),
      p: Number(r.p_value),
      background: Number(r.number_of_genes_in_background),
      ratio: Number(r.ratio_in_set),
    })
  })
  const entries = []
  const order = [
    ...STRING_CATEGORIES.map((c) => c[0]),
    ...[...byCat.keys()].filter((k) => !STRING_CATEGORIES.some((c) => c[0] === k)),
  ]
  order.forEach((cat) => {
    const list = byCat.get(cat)
    if (!list || !list.length) return
    list.sort((a, b) =>
      enrichment
        ? a.fdr - b.fdr || b.members.length - a.members.length
        : b.members.length - a.members.length
    )
    const top = list.slice(0, st.maxTerms)
    const usedNames = new Set()
    const groupMeta = {}
    const lines = top.map((t) => {
      let name = String(t.description)
        .replace(/[\t\r\n]+/g, ' ')
        .trim()
      if (usedNames.has(name)) name = `${name} (${t.term})`
      usedNames.add(name)
      const info = {
        description: `${stringCategoryName(cat)}${t.term && t.term !== name ? ', ' + t.term : ''}`,
        term: t.term,
        category: stringCategoryName(cat),
        'proteins in network': t.members.length,
      }
      if (enrichment) {
        info.FDR = t.fdr
        if (Number.isFinite(t.p)) info['p-value'] = t.p
        if (Number.isFinite(t.background)) info['proteins in genome'] = t.background
      }
      groupMeta[name] = info
      return `${name}\t${t.members.join(',')}`
    })
    const label = `${netEntry.name}: ${stringCategoryName(cat)}${enrichment ? ` (FDR ≤ ${st.fdr})` : ''}`
    const entry = addNormaEntry('annotation', label, lines.join('\n') + '\n', '', null, {})
    entry.groupMeta = groupMeta
    entry.stringCategory = cat
    entry.stringCategoryName = stringCategoryName(cat)
    entry.forNetwork = netEntry.id
    entries.push(entry)
    if (list.length > top.length)
      notes.push({
        level: 'ok',
        text: `${stringCategoryName(cat)}: kept the ${top.length} ${enrichment ? 'most significant' : 'largest'} of ${list.length} terms.`,
      })
  })
  const offered = [...seenCats.keys()].map((k) => `${stringCategoryName(k)} (${seenCats.get(k)})`)
  if (offered.length)
    notes.push({
      level: 'ok',
      text: `STRING returned ${enrichment ? 'enriched terms' : 'annotations'} in: ${offered.join(', ')}.`,
    })
  const emptyWanted = st.categories.filter((k) => !byCat.has(k) && !(k === 'KEGG' && !enrichment))
  if (emptyWanted.length)
    notes.push({
      level: 'warn',
      text: `No ${enrichment ? `terms with FDR ≤ ${st.fdr}` : 'annotations'} and at least ${st.minGroup} proteins for: ${emptyWanted.map(stringCategoryName).join(', ')}.`,
    })
  if (!enrichment && wanted.has('KEGG'))
    notes.push({
      level: 'ok',
      text: 'STRING does not share full KEGG annotations for licensing reasons; choose Terms: "Enriched in the network" to get KEGG pathways.',
    })
  return entries
}

export async function runStringGroupings() {
  const net = stringNetworkInView()
  if (!net) return
  const st = stringSettings()
  if (!st.categories.length && !st.otherCategories) {
    setStatus('stringStatus', [{ level: 'error', text: 'Tick at least one term collection.' }])
    return
  }
  stringState.busy = true
  stringState.cancelled = false
  updateStringUI()
  const notes = []
  try {
    const entries = await fetchStringGroupings(net, st, notes)
    if (entries.length) {
      libSelection.annotation = entries[0].id
      renderLibraryLists()
      refreshLibraryView()
    }
    setStatus('stringStatus', [
      {
        level: entries.length ? 'ok' : 'warn',
        text: entries.length
          ? `Added ${plural(entries.length, 'grouping')} for "${net.name}": ${entries.map((e) => e.stringCategoryName).join(', ')}.`
          : `No new groupings for "${net.name}".`,
      },
      ...notes,
    ])
  } finally {
    stringState.busy = false
    updateStringUI()
  }
}
