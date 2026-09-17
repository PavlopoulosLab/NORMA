// @ts-nocheck
// ponytail: split verbatim from the former single-file script; typed one file at a time (remove @ts-nocheck as it lands)
import { S } from '../state'
import { STRING_CATEGORIES, STRING_CHANNELS, STRING_SPECIES } from '../view3d/export'
import { libEntry } from '../layouts/controls'
import { stringState } from './requests'

/* ---------- UI state ---------- */
export function stringSettings() {
  const speciesSel = document.getElementById('stringSpecies').value
  const taxon =
    speciesSel === 'other' ? document.getElementById('stringTaxon').value.trim() : speciesSel
  const preset = document.getElementById('stringScorePreset').value
  const threshold =
    preset === 'custom'
      ? parseFloat(document.getElementById('stringScore').value)
      : parseFloat(preset)
  return {
    names: document
      .getElementById('stringQuery')
      .value.split(/[\s,;]+/)
      .map((s) => s.trim())
      .filter(Boolean),
    taxon,
    speciesLabel:
      speciesSel === 'other'
        ? `taxon ${taxon}`
        : (STRING_SPECIES.find((s) => String(s[0]) === speciesSel) || [0, speciesSel])[1],
    networkType: document.getElementById('stringNetType').value,
    addNodes: parseInt(document.getElementById('stringAddNodes').value, 10) || 0,
    threshold: Number.isFinite(threshold) ? threshold : 0.4,
    channels: STRING_CHANNELS.filter((c) => document.getElementById('stringCh_' + c[1]).checked),
    edgeMode: document.getElementById('stringEdgeMode').value,
    annotMode: document.getElementById('stringAnnotMode').value,
    fdr: parseFloat(document.getElementById('stringFdr').value) || 0.05,
    categories: STRING_CATEGORIES.map((c) => c[0]).filter(
      (k) => document.getElementById('stringCat_' + k).checked
    ),
    otherCategories: document.getElementById('stringCatOther').checked,
    maxTerms: Math.max(1, parseInt(document.getElementById('stringMaxTerms').value, 10) || 30),
    minGroup: Math.max(1, parseInt(document.getElementById('stringMinGroup').value, 10) || 2),
    descriptions: document.getElementById('stringDescriptions').checked,
  }
}

export function updateStringUI() {
  const st = stringSettings()
  document.getElementById('stringTaxonRow').hidden =
    document.getElementById('stringSpecies').value !== 'other'
  document.getElementById('stringAddNodesValue').textContent = st.addNodes
  const preset = document.getElementById('stringScorePreset').value
  document.getElementById('stringScoreRow').hidden = preset !== 'custom'
  document.getElementById('stringScoreValue').textContent = parseFloat(
    document.getElementById('stringScore').value
  ).toFixed(2)
  document.getElementById('stringFdrRow').hidden = st.annotMode !== 'enrichment'
  document.getElementById('stringKeggNote').hidden = st.annotMode !== 'annotation'
  const busy = stringState.busy
  document.getElementById('btnStringFetch').disabled = busy
  document.getElementById('btnStringCancel').hidden = !busy
  const view = stringNetworkInView()
  document.getElementById('btnStringGroupings').disabled = busy || !view
  document.getElementById('btnStringGroupings').title = view
    ? `Fetch groupings for "${view.name}" with the settings above`
    : 'Show a network imported from STRING in the current view first'
  let hint = ''
  if (st.names.length === 1 && st.addNodes === 0)
    hint = 'With a single protein STRING always adds its 10 best interactors.'
  else if (st.names.length > 1 && st.addNodes === 0)
    hint = 'Only connections among the given proteins are fetched.'
  document.getElementById('stringNodesHint').textContent = hint
}

// The STRING network shown in the current view, if any.
export function stringNetworkInView() {
  if (!S.currentLibView) return null
  const ids = S.currentLibView.nets.split('|').filter(Boolean)
  const nets = ids.map((id) => libEntry('network', id)).filter((e) => e && e.stringMeta)
  return nets.length === 1 ? nets[0] : null
}
