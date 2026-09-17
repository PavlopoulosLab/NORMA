// @ts-nocheck
// ponytail: split verbatim from the former single-file script; typed one file at a time (remove @ts-nocheck as it lands)
import cytoscape from 'cytoscape'
import { webglPreference } from './enrichment'

/* ---------- cytoscape setup ---------- */
/* ---------- stylesheet ----------
   BASE_STYLE is the fixed rule list. Settings change it only through
   setStyle(selector, props), which merges into the rule for that selector.
   The whole sheet is then rebuilt once per tick, so it never grows no
   matter how often sliders move, and rule order (and therefore which rule
   wins) stays fixed: node, node.dimmed, ..., edge, ..., edge.highlighted. */
export const SELECTION_YELLOW = '#facc15'

const BASE_STYLE = [
  {
    selector: 'node',
    style: {
      'background-color': 'data(color)',
      'pie-size': '100%',
      // Cytoscape supports up to 16 pie slices per node
      ...Object.fromEntries(
        Array.from({ length: 16 }, (_, i) => [
          [`pie-${i + 1}-background-color`, `data(pieColor${i + 1})`],
          [`pie-${i + 1}-background-size`, `data(pieSize${i + 1})`],
        ]).flat()
      ),
      width: 'data(size)',
      height: 'data(size)',
      shape: 'data(shape)',
      label: 'data(id)',
      color: '#e7ebf5',
      'font-size': 'data(labelPx)',
      'font-family': 'Inter, sans-serif',
      'text-valign': 'center',
      'text-margin-y': 0,
      'text-outline-width': 3,
      'text-outline-color': '#0f1420',
      'border-width': 2,
      'border-color': 'rgba(255,255,255,0.35)',
      'overlay-opacity': 0,
    },
  },
  {
    selector: 'node.dimmed',
    style: { opacity: 0.12 },
  },
  {
    selector: 'node.highlighted',
    style: { 'border-color': '#5fd3c4', 'border-width': 3 },
  },
  {
    // several nodes can be selected (Shift/Ctrl/Cmd-click or Shift-drag);
    // a thick border plus a soft halo keeps them visible at any zoom
    selector: 'node:selected',
    style: {
      'border-width': 5,
      'border-color': '#facc15',
      'underlay-color': '#facc15',
      'underlay-opacity': 0.35,
      'underlay-padding': 7,
      'underlay-shape': 'ellipse',
    },
  },
  {
    selector: 'edge',
    style: {
      'curve-style': 'haystack',
      'haystack-radius': 0.4,
      width: 2,
      'line-color': 'data(color)',
      opacity: 0.85,
    },
  },
  {
    selector: 'edge.hidden-type',
    style: { display: 'none' },
  },
  {
    selector: 'edge.merged-hidden',
    style: { display: 'none' },
  },
  {
    selector: 'node.hidden-group',
    style: { display: 'none' },
  },
  {
    selector: 'edge.dimmed',
    style: { opacity: 0.04 },
  },
  {
    selector: 'edge.highlighted',
    style: { opacity: 1, width: 3.2 },
  },
  {
    selector: 'edge:selected',
    style: {
      'underlay-color': '#facc15',
      'underlay-opacity': 0.45,
      'underlay-padding': 4,
      opacity: 1,
    },
  },
  {
    selector: 'core',
    style: {
      'selection-box-color': '#0f766e',
      'selection-box-opacity': 0.12,
      'selection-box-border-color': '#0f766e',
      'selection-box-border-width': 1,
      'active-bg-opacity': 0,
    },
  },
]

const DYNAMIC_STYLE = {}

let styleCommitQueued = false

function buildStylesheet() {
  const seen = new Set()
  const sheet = BASE_STYLE.map((rule) => {
    seen.add(rule.selector)
    return {
      selector: rule.selector,
      style: { ...rule.style, ...(DYNAMIC_STYLE[rule.selector] || {}) },
    }
  })
  Object.keys(DYNAMIC_STYLE).forEach((sel) => {
    if (!seen.has(sel)) sheet.push({ selector: sel, style: { ...DYNAMIC_STYLE[sel] } })
  })
  return sheet
}

// other modules (3D view, legend) react after every style commit
const styleHooks = []

export function onCommitStyle(fn) {
  styleHooks.push(fn)
}

function commitStyle() {
  styleCommitQueued = false
  cy.style().fromJson(buildStylesheet()).update()
  styleHooks.forEach((fn) => fn())
}

export function setStyle(selector, props) {
  DYNAMIC_STYLE[selector] = { ...(DYNAMIC_STYLE[selector] || {}), ...props }
  if (!styleCommitQueued) {
    styleCommitQueued = true
    queueMicrotask(commitStyle)
  }
}

// WebGL drawing is chosen once, when the canvas is created (see Display → Performance).
export const WEBGL_ACTIVE = (() => {
  try {
    return webglPreference()
  } catch (e) {
    return false
  }
})()

export const cy = cytoscape({
  container: document.getElementById('cy'),
  ...(WEBGL_ACTIVE ? { renderer: { name: 'canvas', webgl: true } } : {}),
  elements: [],
  // Effectively unlimited zoom. Cytoscape's own limits are 1e-50..1e50,
  // but canvas drawing breaks down long before that; a millionfold either
  // way is far past anything useful. Panning is kept in bounds by
  // clampViewport() below, so the network can't be lost off-screen.
  minZoom: 1e-6,
  maxZoom: 1e6,
  // with panning on, box selection needs Shift (or Ctrl/Cmd/Alt) while dragging
  boxSelectionEnabled: true,
  selectionType: 'single',
  wheelSensitivity: 0.25,
  style: BASE_STYLE,
  layout: { name: 'grid' },
})
