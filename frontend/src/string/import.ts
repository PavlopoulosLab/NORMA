// @ts-nocheck
// ponytail: split verbatim from the former single-file script; typed one file at a time (remove @ts-nocheck as it lands)
import { STRING_CHANNELS, STRING_PREFERRED_GROUPING } from '../view3d/export'
import {
  addNormaEntry,
  libSelection,
  listSample,
  plural,
  renderLibraryLists,
  setStatus,
  startProgress,
} from '../layouts/controls'
import {
  combineStringScores,
  scoreValue,
  stringBase,
  stringCall,
  stringState,
  stringVersion,
} from './requests'
import { fetchStringGroupings } from './groupings'
import { openInNewView } from '../profiler'
import { refreshLibraryView } from '../library'
import { stringSettings, updateStringUI } from './ui_state'

/* ---------- import ---------- */

export async function runStringImport() {
  const st = stringSettings()
  if (!st.names.length) {
    setStatus('stringStatus', [
      { level: 'error', text: 'Type at least one protein or gene name, for example TP53.' },
    ])
    return
  }
  if (!/^(\d+|STRG\w+)$/.test(st.taxon)) {
    setStatus('stringStatus', [
      {
        level: 'error',
        text: 'Enter the organism as an NCBI taxon number, for example 9606 for human.',
      },
    ])
    return
  }
  if (!st.channels.length) {
    setStatus('stringStatus', [{ level: 'error', text: 'Tick at least one evidence channel.' }])
    return
  }
  if (st.names.length > 2000) {
    setStatus('stringStatus', [
      { level: 'error', text: 'STRING accepts at most 2,000 names per query.' },
    ])
    return
  }
  stringState.busy = true
  stringState.cancelled = false
  updateStringUI()
  const notes = []
  const withGroupings = !!(st.categories.length || st.otherCategories)
  // steps: version, names, network, descriptions, groupings, opening
  const task = startProgress('stringStatus', [
    1,
    2,
    6,
    st.descriptions ? 2 : 0,
    withGroupings ? 4 : 0,
    1,
  ])
  const onBytes = (got, total) => task.bytes(got, total)
  try {
    task.step(0, 'Checking the STRING version…')
    const ver = await stringVersion()

    task.step(1, `Looking up ${plural(st.names.length, 'name')} in ${st.speciesLabel}…`)
    const mapped = await stringCall(
      'get_string_ids',
      { identifiers: st.names.join('\r'), species: st.taxon, echo_query: 1 },
      { onBytes }
    )
    const byQuery = new Map()
    ;(mapped || []).forEach((r) => {
      const q = r.queryItem ?? st.names[r.queryIndex]
      if (q !== undefined && !byQuery.has(q)) byQuery.set(q, r)
    })
    const found = st.names.filter((n) => byQuery.has(n))
    const missing = st.names.filter((n) => !byQuery.has(n))
    if (!found.length)
      throw new Error(
        `STRING found none of the names in ${st.speciesLabel}. Check the spelling and the organism.`
      )
    if (missing.length)
      notes.push({
        level: 'warn',
        text: `Not found in ${st.speciesLabel}: ${listSample(missing)}.`,
      })
    const renamed = found.filter(
      (n) =>
        byQuery.get(n).preferredName &&
        byQuery.get(n).preferredName.toLowerCase() !== n.toLowerCase()
    )
    if (renamed.length)
      notes.push({
        level: 'ok',
        text: `Matched ${listSample(renamed.map((n) => `${n} as ${byQuery.get(n).preferredName}`))}.`,
      })
    const queryIds = [...new Set(found.map((n) => byQuery.get(n).stringId))]
    const taxonName = byQuery.get(found[0]).taxonName || st.speciesLabel

    task.step(
      2,
      st.addNodes
        ? `Fetching the network with up to ${st.addNodes} interactors…`
        : 'Fetching the network…'
    )
    const params = {
      identifiers: queryIds.join('\r'),
      species: st.taxon,
      required_score: Math.round(st.threshold * 1000),
      network_type: st.networkType,
    }
    if (st.addNodes > 0) params.add_nodes = st.addNodes
    const rows = await stringCall('network', params, { onBytes })
    const built = buildStringNetwork(rows || [], queryIds, byQuery, st)
    if (!built.edges.length)
      throw new Error(
        `No ${st.networkType} connections pass a confidence of ${st.threshold.toFixed(2)} with the ticked channels. Lower the confidence, tick more channels or add interactors.`
      )
    notes.push(...built.notes)

    // descriptions for every protein
    if (st.descriptions) {
      task.step(3, `Fetching descriptions for ${plural(built.nodeIds.length, 'protein')}…`)
      try {
        const info = await stringCall(
          'get_string_ids',
          { identifiers: built.stringIds.join('\r'), species: st.taxon },
          { onBytes }
        )
        ;(info || []).forEach((r) => {
          const name = built.nameOf.get(r.stringId)
          if (name && r.annotation) built.nodeAttrs[name].description = r.annotation
        })
      } catch (err) {
        notes.push({
          level: 'warn',
          text: `Protein descriptions couldn't be fetched: ${err.message}`,
        })
      }
    }

    const netName = `STRING ${found
      .slice(0, 3)
      .map((n) => byQuery.get(n).preferredName || n)
      .join(', ')}${found.length > 3 ? ` +${found.length - 3}` : ''} (${shortSpecies(taxonName)})`
    const netEntry = addNormaEntry('network', netName, built.text, '', null, {})
    netEntry.nodeAttrs = built.nodeAttrs
    netEntry.stringMeta = {
      taxon: st.taxon,
      taxonName,
      version: ver.version,
      address: ver.address || stringBase(),
      query: found,
      stringIds: built.stringIds,
      nameOf: Object.fromEntries(built.nameOf),
      threshold: st.threshold,
      networkType: st.networkType,
      channels: st.channels.map((c) => c[1]),
    }

    // groupings
    let annEntries = []
    if (withGroupings) {
      annEntries = await fetchStringGroupings(netEntry, st, notes, task, 4)
    }
    task.step(5, 'Opening the network…')

    // show it
    const preferred =
      STRING_PREFERRED_GROUPING.map((k) => annEntries.find((e) => e.stringCategory === k)).find(
        Boolean
      ) || annEntries[0]
    openInNewView(netName, () => {
      // a new view starts with its own (empty) file selection
      libSelection.networks = new Set([netEntry.id])
      libSelection.annotation = preferred ? preferred.id : ''
      libSelection.colors = ''
      renderLibraryLists()
      refreshLibraryView()
    })
    const summary = `Imported ${netEntry.parsed.summary} from STRING${ver.version ? ' ' + ver.version : ''} (${taxonName}, ${st.networkType}, confidence ≥ ${st.threshold.toFixed(2)}).`
    const groupText = annEntries.length
      ? `Added ${plural(annEntries.length, 'grouping')}: ${annEntries.map((e) => e.stringCategoryName).join(', ')}. Switch between them with the Grouping list at the top.`
      : 'No groupings were added.'
    setStatus('stringStatus', [
      { level: 'ok', text: summary },
      { level: 'ok', text: groupText },
      ...notes,
    ])
  } catch (err) {
    setStatus('stringStatus', [{ level: 'error', text: err.message }, ...notes])
  } finally {
    task.stop()
    stringState.busy = false
    stringState.abort = null
    updateStringUI()
  }
}

function shortSpecies(name) {
  const parts = String(name || '').split(/\s+/)
  return parts.length >= 2 ? `${parts[0][0]}. ${parts[1]}` : name
}

function buildStringNetwork(rows, queryIds, byQuery, st) {
  const notes = []
  const nameOf = new Map() // stringId -> node name
  const used = new Set()
  const nodeAttrs = {}
  const querySet = new Set(queryIds)
  const addNode = (sid, preferred) => {
    if (nameOf.has(sid)) return nameOf.get(sid)
    // node names must be unique and can't contain commas (annotation files)
    let name =
      String(preferred || sid)
        .replace(/,/g, ';')
        .trim() || sid
    if (used.has(name)) name = `${name} (${sid})`
    used.add(name)
    nameOf.set(sid, name)
    nodeAttrs[name] = { stringId: sid, query: querySet.has(sid) }
    return name
  }
  queryIds.forEach((sid) => {
    const q = [...byQuery.values()].find((r) => r.stringId === sid)
    addNode(sid, q && q.preferredName)
  })
  const seen = new Set()
  const lines = []
  let dropped = 0,
    pairs = 0
  rows.forEach((r) => {
    const a = r.stringId_A,
      b = r.stringId_B
    if (!a || !b || a === b) return
    const key = a < b ? a + '\t' + b : b + '\t' + a
    if (seen.has(key)) return // STRING may list a pair in both directions
    seen.add(key)
    const scores = st.channels.map((c) => scoreValue(r[c[0]]))
    const combined =
      st.channels.length === STRING_CHANNELS.length
        ? scoreValue(r.score)
        : combineStringScores(scores)
    if (combined < st.threshold - 1e-9) {
      dropped++
      return
    }
    const na = addNode(a, r.preferredName_A),
      nb = addNode(b, r.preferredName_B)
    pairs++
    if (st.edgeMode === 'combined') {
      lines.push([na, nb, combined.toFixed(3), 'combined'])
    } else {
      st.channels.forEach((c, i) => {
        if (scores[i] > 0) lines.push([na, nb, scores[i].toFixed(3), c[1]])
      })
    }
  })
  if (dropped)
    notes.push({
      level: 'ok',
      text: `Left out ${plural(dropped, 'connection')} whose score from the ticked channels alone is below ${st.threshold.toFixed(2)}.`,
    })
  const connected = new Set(lines.flatMap((l) => [l[0], l[1]]))
  const lonely = [...nameOf.values()].filter((n) => !connected.has(n))
  if (lonely.length)
    notes.push({
      level: 'warn',
      text: `${plural(lonely.length, 'protein has', 'proteins have')} no connection that passes the filters and ${lonely.length === 1 ? 'is' : 'are'} not shown: ${listSample(lonely)}.`,
    })
  const text = ['Source\tTarget\tWeight\tType', ...lines.map((l) => l.join('\t'))].join('\n') + '\n'
  const kept = [...nameOf.entries()].filter(([, n]) => connected.has(n))
  return {
    text,
    edges: lines,
    pairs,
    nodeIds: kept.map(([, n]) => n),
    stringIds: kept.map(([sid]) => sid),
    nameOf: new Map(kept),
    nodeAttrs: Object.fromEntries(kept.map(([, n]) => [n, nodeAttrs[n]])),
    notes,
  }
}
