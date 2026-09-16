// @ts-nocheck
// ponytail: split verbatim from the former single-file script; typed one file at a time (remove @ts-nocheck as it lands)
import { NORMA_EXAMPLE_SETS } from './wiring'
import {
  addNormaEntry,
  libSelection,
  normaLibrary,
  plural,
  renderLibraryLists,
  setLayoutMode,
} from './layouts/controls'
import { applyEdgeCurveStyle } from './profiler'
import { applyEdgeDirection } from './export/dialog'
import { applyValueColors, scheduleLegend } from './clustering/mapping'
import { refreshLibraryView } from './library'

/* ---------- NORMA example sets ---------- */
// A layout to use the next time a network loads (set by showcase examples).
let nextLoadLayout = null

export function takeNextLoadLayout() {
  const name = nextLoadLayout
  nextLoadLayout = null
  if (name) {
    setLayoutMode('connections')
    document.getElementById('layoutSelect').value = name
  }
  return name
}

export function loadNormaExampleSet(key) {
  const set = NORMA_EXAMPLE_SETS[key]
  if (!set) return
  const entries = set.files.map((f, i) => {
    if (!normaLibrary[f.kind]) return null // e.g. original-format copies, offered only as downloads
    const sourceKey = `${key}/${i}`
    const existing = normaLibrary[f.kind].find((e) => e.sourceKey === sourceKey)
    return (
      existing ||
      addNormaEntry(f.kind, f.name, f.text, f.fileName, sourceKey, { directed: !!f.directed })
    )
  })
  const real = entries.filter(Boolean)
  const net = real.find((e) => e.kind === 'network')
  const ann = real.find((e) => e.kind === 'annotation')
  const col = real.find((e) => e.kind === 'colors')
  libSelection.networks = new Set(
    set.showAll ? real.filter((e) => e.kind === 'network').map((e) => e.id) : [net.id]
  )
  libSelection.annotation = ann ? ann.id : ''
  libSelection.colors = col ? col.id : ''
  renderLibraryLists()
  nextLoadLayout = (set.display && set.display.layoutSelect) || null
  refreshLibraryView()
  nextLoadLayout = null
  // showcase examples switch on the features they are meant to show
  if (set.display) {
    Object.entries(set.display).forEach(([id, value]) => {
      const el = document.getElementById(id)
      if (!el) return
      if (el.type === 'checkbox') el.checked = !!value
      else el.value = value
      if (id !== 'layoutSelect') el.dispatchEvent(new Event('change', { bubbles: true }))
    })
    applyEdgeDirection()
    applyEdgeCurveStyle()
    applyValueColors()
    scheduleLegend()
  }
  const others = real.filter((e) => e.kind === 'annotation').length - 1
  if (others > 0) {
    const el = document.getElementById('normaStatus')
    const note = document.createElement('div')
    note.className = 'note ok'
    note.textContent = `This example has ${plural(others, 'more annotation')} in the Annotations list. Tick one and choose Show in this view to switch; node positions are kept.`
    el.insertBefore(note, el.children[1] || null)
  }
}
