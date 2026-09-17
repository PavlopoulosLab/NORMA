// @ts-nocheck
// ponytail: split verbatim from the former single-file script; typed one file at a time (remove @ts-nocheck as it lands)
import { SELECTION_YELLOW, setStyle } from './cy'
import { applyEdgeMerge } from './parallel_edges'
import { updateLabelStyle } from './profiler'

/* ---------- UI themes (chrome + canvas; independent of node/edge data palettes) ---------- */
const THEMES = {
  dark: {
    bg: '#0f1420',
    panel: '#161d2e',
    panel2: '#1d2740',
    panel2Hover: '#223050',
    line: '#2a3554',
    text: '#e7ebf5',
    muted: '#8993ad',
    accent: '#5fd3c4',
    accentDim: '#3a7d74',
    accentHover: '#7fe0d3',
    accentText: '#08221e',
    warn: '#e7a15f',
    canvasDot: '#1c2540',
    nodeBorder: 'rgba(255,255,255,0.35)',
  },
  midnight: {
    bg: '#050608',
    panel: '#0c0e12',
    panel2: '#131620',
    panel2Hover: '#1a1e2b',
    line: '#1f2430',
    text: '#dfe3ee',
    muted: '#767c8c',
    accent: '#7c5cff',
    accentDim: '#5540b3',
    accentHover: '#9b82ff',
    accentText: '#0a0612',
    warn: '#e7a15f',
    canvasDot: '#111420',
    nodeBorder: 'rgba(255,255,255,0.3)',
  },
  light: {
    bg: '#f4f5f8',
    panel: '#ffffff',
    panel2: '#eef1f6',
    panel2Hover: '#e2e7ef',
    line: '#d7dce6',
    text: '#1b2333',
    muted: '#6b7387',
    accent: '#2a9d8f',
    accentDim: '#1f7a6f',
    accentHover: '#39b7a7',
    accentText: '#ffffff',
    warn: '#c76b1f',
    canvasDot: '#e4e8f0',
    nodeBorder: 'rgba(20,25,40,0.25)',
  },
  white: {
    bg: '#ffffff',
    panel: '#ffffff',
    panel2: '#f5f5f7',
    panel2Hover: '#e9e9ec',
    line: '#e0e0e5',
    text: '#111114',
    muted: '#6e6e76',
    accent: '#0f766e',
    accentDim: '#0b5c56',
    accentHover: '#14958a',
    accentText: '#ffffff',
    warn: '#b45309',
    canvasDot: '#eeeeee',
    nodeBorder: 'rgba(0,0,0,0.2)',
  },
  gray: {
    bg: '#2b2e33',
    panel: '#34383f',
    panel2: '#3d424a',
    panel2Hover: '#464b54',
    line: '#4a4f58',
    text: '#e7e8ea',
    muted: '#9a9fa8',
    accent: '#8fb8e8',
    accentDim: '#5f83ad',
    accentHover: '#a9cdf5',
    accentText: '#12161c',
    warn: '#e0a458',
    canvasDot: '#3a3f47',
    nodeBorder: 'rgba(255,255,255,0.2)',
  },
  paper: {
    bg: '#f3ecdd',
    panel: '#fbf6ea',
    panel2: '#efe6d2',
    panel2Hover: '#e6dac0',
    line: '#ddd0b0',
    text: '#3a2f1f',
    muted: '#8a7a5c',
    accent: '#b3541e',
    accentDim: '#8a4017',
    accentHover: '#d06a2c',
    accentText: '#fff7ec',
    warn: '#7a5a1e',
    canvasDot: '#e9ddc2',
    nodeBorder: 'rgba(58,47,31,0.3)',
  },
}

export let currentTheme = THEMES.dark

export function applyTheme(name) {
  const t = THEMES[name] || THEMES.dark
  currentTheme = t

  const root = document.documentElement.style
  root.setProperty('--bg', t.bg)
  root.setProperty('--panel', t.panel)
  root.setProperty('--panel-2', t.panel2)
  root.setProperty('--panel-2-hover', t.panel2Hover)
  root.setProperty('--line', t.line)
  root.setProperty('--text', t.text)
  root.setProperty('--muted', t.muted)
  root.setProperty('--accent', t.accent)
  root.setProperty('--accent-dim', t.accentDim)
  root.setProperty('--accent-hover', t.accentHover)
  root.setProperty('--accent-text', t.accentText)
  root.setProperty('--warn', t.warn)
  root.setProperty('--canvas-dot', t.canvasDot)

  // cytoscape doesn't read CSS variables, so the canvas-drawn parts
  // (node label color/halo, node border, highlight color) are set directly
  setStyle('node', { color: t.text, 'text-outline-color': t.bg, 'border-color': t.nodeBorder })
  setStyle('node.highlighted', { 'border-color': t.accent })
  // selected nodes and edges are marked in yellow on every theme
  setStyle('node:selected', {
    'border-color': SELECTION_YELLOW,
    'underlay-color': SELECTION_YELLOW,
  })
  setStyle('edge:selected', { 'underlay-color': SELECTION_YELLOW })
  setStyle('core', { 'selection-box-color': t.accent, 'selection-box-border-color': t.accent })

  updateLabelStyle() // edge label color/halo also depend on the theme
  applyEdgeMerge() // merged edges use the theme's muted color
}
