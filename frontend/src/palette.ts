// @ts-nocheck
// ponytail: split verbatim from the former single-file script; typed one file at a time (remove @ts-nocheck as it lands)
/* ---------- edge type palette (STRING-style evidence channels) ---------- */
export const EDGE_TYPES = {
  neighborhood: { color: '#59c46b', label: 'Gene neighborhood' },
  fusion: { color: '#d9534f', label: 'Gene fusion' },
  cooccurrence: { color: '#3a6cd6', label: 'Co-occurrence' },
  coexpression: { color: '#111827', label: 'Co-expression' },
  experiments: { color: '#b464c9', label: 'Experiments' },
  database: { color: '#8fc7e8', label: 'Curated database' },
  textmining: { color: '#c9c14a', label: 'Text mining' },
  homology: { color: '#caa6ea', label: 'Homology' },
}

export const ORIGINAL_EDGE_TYPES = JSON.parse(JSON.stringify(EDGE_TYPES))

// Colorblind-safe palettes, shared between node and edge use.
// Okabe-Ito: Okabe,M. and Ito,K. (2008) Color Universal Design (CUD) - a widely
// used 8-color palette designed to stay distinguishable under the common forms
// of color vision deficiency (protanopia, deuteranopia, tritanopia).
const CB_OKABE_ITO = [
  '#E69F00',
  '#56B4E9',
  '#009E73',
  '#F0E442',
  '#0072B2',
  '#D55E00',
  '#CC79A7',
  '#000000',
]

// IBM Design Language's colorblind-safe categorical set.
const CB_IBM = ['#648FFF', '#785EF0', '#DC267F', '#FE6100', '#FFB000']

// Viridis-inspired perceptually-uniform sequence (also colorblind-safe by design).
const CB_VIRIDIS = [
  '#440154',
  '#482878',
  '#3E4A89',
  '#31688E',
  '#26828E',
  '#1F9E89',
  '#35B779',
  '#FDE725',
]

// Once a category count exceeds a curated palette's length (all palettes here
// have 5-8 hand-picked colors), further colors are generated on the fly using
// a golden-angle hue rotation. Successive hues stay well-spread around the
// color wheel however many categories are added -- this is what lets the
// legends stay usable up to ~100 distinct edge channels or node groups
// instead of the palette repeating every 6-8 entries.
function hslToHex(h, s, l) {
  s /= 100
  l /= 100
  const k = (n) => (n + h / 30) % 12
  const a = s * Math.min(l, 1 - l)
  const f = (n) => l - a * Math.max(-1, Math.min(k(n) - 3, Math.min(9 - k(n), 1)))
  const toHex = (x) =>
    Math.round(255 * x)
      .toString(16)
      .padStart(2, '0')
  return `#${toHex(f(0))}${toHex(f(8))}${toHex(f(4))}`
}

function goldenAngleColor(index) {
  const hue = (index * 137.508) % 360 // golden angle, avoids hue clustering
  const s = 62,
    l = index % 2 === 0 ? 52 : 62 // alternate lightness for extra separation
  return hslToHex(hue, s, l)
}

export function colorAtIndex(palette, index) {
  return index < palette.length ? palette[index] : goldenAngleColor(index)
}

export const NODE_PALETTES = {
  vivid: ['#e8a15f', '#5fd3c4', '#e07a7a', '#8fb8e8', '#c9c14a', '#a97fd6', '#7fd68a', '#d68fc0'],
  sunset: ['#f4a261', '#e76f51', '#e9c46a', '#f2a65a', '#bc6c25', '#dda15e', '#ffb703', '#fb8500'],
  ocean: ['#264653', '#2a9d8f', '#8ecae6', '#219ebc', '#023047', '#48cae4', '#90e0ef', '#00b4d8'],
  forest: ['#606c38', '#283618', '#a3b18a', '#588157', '#3a5a40', '#344e41', '#6a994e', '#a7c957'],
  pastel: ['#ffd6ff', '#c8b6ff', '#bde0fe', '#a2d2ff', '#ffafcc', '#caffbf', '#fdffb6', '#ffc6ff'],
  mono: ['#3a4a6b', '#4c5f8a', '#6478a8', '#7d91c0', '#96a9d3', '#b0bfe0', '#c9d3ea', '#e2e7f4'],
  cbOkabeIto: CB_OKABE_ITO,
  cbIBM: CB_IBM,
  viridis: CB_VIRIDIS,
}

export const EDGE_PALETTES = {
  categorical: [
    '#e07a7a',
    '#5fd3c4',
    '#e8a15f',
    '#8fb8e8',
    '#c9c14a',
    '#a97fd6',
    '#7fd68a',
    '#d68fc0',
  ],
  warm: ['#e63946', '#f4a261', '#e9c46a', '#f77f00', '#d62828', '#fb8500', '#c1121f', '#ee9b00'],
  cool: ['#264653', '#2a9d8f', '#457b9d', '#1d3557', '#48cae4', '#219ebc', '#023047', '#90e0ef'],
  mono: ['#3a4a6b', '#4c5f8a', '#6478a8', '#7d91c0', '#96a9d3', '#b0bfe0', '#c9d3ea', '#e2e7f4'],
  cbOkabeIto: CB_OKABE_ITO,
  cbIBM: CB_IBM,
  viridis: CB_VIRIDIS,
}
