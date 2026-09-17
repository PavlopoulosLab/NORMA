// @ts-nocheck
// ponytail: split verbatim from the former single-file script; typed one file at a time (remove @ts-nocheck as it lands)
import { EDGE_TYPES } from '../palette'
import { STRING_CATEGORIES, STRING_CHANNELS, STRING_SPECIES } from '../view3d/export'
import { escapeHtml } from '../network_state'
import { runStringGroupings } from './groupings'
import { runStringImport } from './import'
import { stringRoute, stringState, updateStringConnectState } from './requests'
import { updateStringUI } from './ui_state'

/* ============================================================
   ICONS AND WELCOME CARD
   Small line icons (16 x 16, drawn in the current text color) for the
   page tabs, sidebar tabs and sidebar sections, and the welcome card
   shown while the current view is empty.
   ============================================================ */
const ICON_PATHS = {
  code: '<path d="M5.5 4.5 2 8l3.5 3.5M10.5 4.5 14 8l-3.5 3.5M9.2 3 6.8 13"/>',
  legend:
    '<rect x="2.5" y="2.5" width="11" height="11" rx="1.5"/><circle cx="5.2" cy="5.8" r="0.9"/><circle cx="5.2" cy="10.2" r="0.9"/><path d="M7.5 5.8h4M7.5 10.2h4"/>',
  gauge:
    '<path d="M2.6 11.5a5.6 5.6 0 1 1 10.8 0"/><path d="M8 10.2 10.6 6.4"/><circle cx="8" cy="10.4" r="0.9"/>',
  info: '<circle cx="8" cy="8" r="6.2"/><path d="M8 7.2v4.2M8 4.8v.1"/>',
  home: '<path d="M2.5 7.4 8 2.8l5.5 4.6"/><path d="M4 6.3V13.2h3V9.6h2v3.6h3V6.3"/>',
  graph:
    '<circle cx="4" cy="4" r="2"/><circle cx="12" cy="5" r="2"/><circle cx="7" cy="12" r="2"/><path d="M5.8 4.3 10 4.8M5 5.6l1.3 4.6M10.8 6.6 8.4 10.5"/>',
  cube: '<path d="M8 1.8 14 5v6L8 14.2 2 11V5z"/><path d="M2 5l6 3.2L14 5M8 8.2v6"/>',
  chart:
    '<path d="M2 14h12"/><rect x="3" y="8" width="2.4" height="5"/><rect x="6.8" y="4.5" width="2.4" height="8.5"/><rect x="10.6" y="2" width="2.4" height="11"/>',
  venn: '<circle cx="6" cy="8" r="4.2"/><circle cx="10" cy="8" r="4.2"/>',
  help: '<circle cx="8" cy="8" r="6.2"/><path d="M6.2 6.2a1.9 1.9 0 1 1 2.6 1.8c-.6.3-.8.7-.8 1.3v.4"/><circle cx="8" cy="11.6" r=".4" fill="currentColor"/>',
  database:
    '<ellipse cx="8" cy="3.8" rx="5.2" ry="1.9"/><path d="M2.8 3.8v8.4c0 1 2.3 1.9 5.2 1.9s5.2-.9 5.2-1.9V3.8M2.8 8c0 1 2.3 1.9 5.2 1.9s5.2-.9 5.2-1.9"/>',
  sliders:
    '<path d="M2 4h7M12 4h2M2 12h2M7 12h7M2 8h3M8 8h6"/><circle cx="10.5" cy="4" r="1.5"/><circle cx="5.5" cy="12" r="1.5"/><circle cx="6.5" cy="8" r="1.5"/>',
  sparkle: '<path d="M8 1.5 9.3 6.7 14.5 8 9.3 9.3 8 14.5 6.7 9.3 1.5 8 6.7 6.7z"/>',
  cloud:
    '<path d="M4.5 12.5h7.2a3 3 0 0 0 .3-6 4.2 4.2 0 0 0-8.1.9A2.6 2.6 0 0 0 4.5 12.5z"/><path d="M8 7.2v4.3M6.3 9.9 8 11.6l1.7-1.7"/>',
  folder:
    '<path d="M1.8 4.2c0-.7.5-1.2 1.2-1.2h3.2l1.5 1.6h5.3c.7 0 1.2.5 1.2 1.2v6.4c0 .7-.5 1.2-1.2 1.2H3c-.7 0-1.2-.5-1.2-1.2z"/>',
  swap: '<path d="M4.5 2v10M2.3 9.8l2.2 2.2 2.2-2.2M11.5 14V4M9.3 6.2l2.2-2.2 2.2 2.2"/>',
  layout:
    '<rect x="2" y="2" width="5" height="5" rx="1"/><rect x="9" y="2" width="5" height="5" rx="1"/><rect x="2" y="9" width="5" height="5" rx="1"/><circle cx="11.5" cy="11.5" r="2.5"/>',
  groups:
    '<circle cx="5.5" cy="6" r="3.5"/><circle cx="10.5" cy="6" r="3.5"/><circle cx="8" cy="10.5" r="3.5"/>',
  fog: '<path d="M3.5 9.5c-1.4-2 0-5 2.6-5.2C7 2.3 10 2 11.4 4c2.2.1 3.2 2.8 1.9 4.6.6 2-1.3 3.8-3.2 3.1-1.1 1.4-3.6 1.4-4.6 0-1.7.3-3-1-2-2.2z"/>',
  palette:
    '<path d="M8 1.8a6.2 6.2 0 1 0 0 12.4c1 0 1.4-.8 1-1.6-.5-1 .1-2 1.2-2h1.5a2.6 2.6 0 0 0 2.5-2.6A6.2 6.2 0 0 0 8 1.8z"/><circle cx="5" cy="7" r=".9" fill="currentColor"/><circle cx="7.6" cy="4.6" r=".9" fill="currentColor"/><circle cx="10.6" cy="5.6" r=".9" fill="currentColor"/>',
  node: '<circle cx="8" cy="8" r="5.2"/><circle cx="8" cy="8" r="1.6" fill="currentColor"/>',
  tag: '<path d="M2 2.8V7l6.8 6.8a1 1 0 0 0 1.4 0l3.6-3.6a1 1 0 0 0 0-1.4L7 2H2.8a.8.8 0 0 0-.8.8z"/><circle cx="5" cy="5" r="1"/>',
  edge: '<circle cx="3.5" cy="12.5" r="1.8"/><circle cx="12.5" cy="3.5" r="1.8"/><path d="M4.8 11.2 11.2 4.8"/><path d="M8.6 3.6 12 4l.4 3.4" />',
  layers: '<path d="M8 2 14 5 8 8 2 5z"/><path d="M2 8l6 3 6-3M2 11l6 3 6-3"/>',
  list: '<path d="M6 4h8M6 8h8M6 12h8"/><circle cx="3" cy="4" r=".8" fill="currentColor"/><circle cx="3" cy="8" r=".8" fill="currentColor"/><circle cx="3" cy="12" r=".8" fill="currentColor"/>',
  play: '<path d="M4.5 2.8v10.4L13 8z"/>',
  upload: '<path d="M8 11V2.8M5 5.6 8 2.6l3 3M2.5 10.5v2.2c0 .6.5 1 1 1h9c.6 0 1-.4 1-1v-2.2"/>',
}

function iconSvg(name) {
  const body = ICON_PATHS[name]
  if (!body) return ''
  return `<svg viewBox="0 0 16 16" fill="none" stroke="currentColor" stroke-width="1.4" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true" focusable="false">${body}</svg>`
}

function iconSpan(name) {
  const span = document.createElement('span')
  span.className = 'ico'
  span.innerHTML = iconSvg(name)
  return span
}

// page wiring, run by main.ts in the original order
export function init() {
  /* ---------- wiring ---------- */
  ;(function setupStringImporter() {
    const sel = document.getElementById('stringSpecies')
    STRING_SPECIES.forEach(([id, name]) => sel.add(new Option(name, String(id))))
    sel.add(new Option('Other organism (enter the taxon ID)…', 'other'))
    const chBox = document.getElementById('stringChannels')
    STRING_CHANNELS.forEach(([, key, label]) => {
      const row = document.createElement('label')
      row.className = 'string-check'
      row.innerHTML = `<input type="checkbox" id="stringCh_${key}" checked><span class="swatch" style="background:${(EDGE_TYPES[key] || {}).color || '#888'}"></span><span>${escapeHtml(label)}</span>`
      chBox.appendChild(row)
    })
    const catBox = document.getElementById('stringCategories')
    STRING_CATEGORIES.forEach(([key, label, on]) => {
      const row = document.createElement('label')
      row.className = 'string-check'
      row.innerHTML = `<input type="checkbox" id="stringCat_${key}"${on ? ' checked' : ''}><span>${escapeHtml(label)}</span>`
      catBox.appendChild(row)
    })
    const other = document.createElement('label')
    other.className = 'string-check'
    other.innerHTML =
      '<input type="checkbox" id="stringCatOther"><span>Any other collection STRING offers</span>'
    catBox.appendChild(other)
    document.getElementById('stringSection').addEventListener('input', updateStringUI)
    document.getElementById('stringSection').addEventListener('change', updateStringUI)
    document.getElementById('btnStringFetch').addEventListener('click', runStringImport)
    document.getElementById('btnStringGroupings').addEventListener('click', runStringGroupings)
    document.getElementById('btnStringCancel').addEventListener('click', () => {
      stringState.cancelled = true
      if (stringState.abort) stringState.abort.abort()
    })
    document.getElementById('stringQuery').addEventListener('keydown', (ev) => {
      if (ev.key === 'Enter' && (ev.ctrlKey || ev.metaKey)) {
        ev.preventDefault()
        runStringImport()
      }
    })
    document.getElementById('stringConnect').addEventListener('change', () => {
      stringRoute.checked = false
      stringRoute.mode = null
      updateStringConnectState()
    })
    updateStringConnectState()
    document.getElementById('btnStringCatAll').addEventListener('click', () => {
      STRING_CATEGORIES.forEach((c) => {
        document.getElementById('stringCat_' + c[0]).checked = true
      })
      updateStringUI()
    })
    document.getElementById('btnStringCatNone').addEventListener('click', () => {
      STRING_CATEGORIES.forEach((c) => {
        document.getElementById('stringCat_' + c[0]).checked = false
      })
      document.getElementById('stringCatOther').checked = false
      updateStringUI()
    })
    updateStringUI()
  })()

  ;(function addIcons() {
    const tabIcons = {
      welcome: 'home',
      about: 'info',
      api: 'code',
      network: 'graph',
      network3d: 'cube',
      profiler: 'chart',
      compare: 'venn',
      help: 'help',
    }
    document.querySelectorAll('#tabs .tab').forEach((t) => {
      if (tabIcons[t.dataset.tab]) t.prepend(iconSpan(tabIcons[t.dataset.tab]))
    })
    const sideIcons = { data: 'upload', db: 'database', display: 'sliders', export: 'swap' }
    document
      .querySelectorAll('#sideTabs [data-side]')
      .forEach((t) => t.prepend(iconSpan(sideIcons[t.dataset.side])))
    const sectionIcons = {
      Examples: 'sparkle',
      STRING: 'cloud',
      Files: 'folder',
      'Open saved work': 'upload',
      Reactome: 'cloud',
      OmniPath: 'cloud',
      NDEx: 'cloud',
      IntAct: 'cloud',
      'Gene Ontology': 'cloud',
      'NORMA files': 'folder',
      Image: 'palette',
      Arena3D: 'cube',
      'Other tools': 'swap',
      'Save your work': 'upload',
      '3D layout': 'cube',
      Layout: 'layout',
      'Node groups': 'groups',
      'Group highlighting': 'fog',
      Colors: 'palette',
      Nodes: 'node',
      Labels: 'tag',
      Edges: 'edge',
      'Edge channels': 'layers',
      Attributes: 'list',
      Legend: 'legend',
      Performance: 'gauge',
    }
    document.querySelectorAll('.section > h3').forEach((h) => {
      const title = [...h.childNodes]
        .filter((n) => n.nodeType === 3)
        .map((n) => n.textContent)
        .join('')
        .trim()
      if (sectionIcons[title]) h.prepend(iconSpan(sectionIcons[title]))
    })
    document.querySelectorAll('.ico[data-icon]').forEach((s) => {
      s.innerHTML = iconSvg(s.dataset.icon)
    })
  })()
}
