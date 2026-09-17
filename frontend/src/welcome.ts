// @ts-nocheck
// ponytail: split verbatim from the former single-file script; typed one file at a time (remove @ts-nocheck as it lands)
import { cy } from './cy'
import { switchSideTab } from './side_tabs'
import { switchTab } from './wiring'

/* ---------- welcome card ---------- */
export function updateEmptyState() {
  const el = document.getElementById('emptyState')
  if (!el) return
  const empty = !cy.nodes().length
  el.hidden = !empty
  document.getElementById('canvas').classList.toggle('is-empty', empty)
}

function openWelcomeExample() {
  switchSideTab('data')
  const sel = document.getElementById('sampleSelect')
  sel.value = 'norma:cascade'
  document.getElementById('btnSample').click()
  switchTab('network')
}

/* ============================================================
   COMMUNITY DETECTION
   All algorithms work on the profiler's simple undirected graph
   ({ ids, n, m, adj }) and return { membership, count, sizes, modularity }.
   Randomized steps use a fixed seed, so results are repeatable.
     louvain            Blondel et al. 2008 (resolution parameter)
     leidenCommunities  Traag, Waltman & van Eck 2019
     labelPropagation   Raghavan, Albert & Kumara 2007
     walktrap           Pons & Latapy 2005 (random walks of length t)
     markovClustering   van Dongen 2000 (MCL, inflation parameter)
   ============================================================ */
export const COMMUNITY_ALGORITHMS = {
  louvain: { label: 'Louvain', short: 'Louvain' },
  leiden: { label: 'Leiden', short: 'Leiden' },
  lpa: { label: 'Label propagation', short: 'label propagation' },
  walktrap: { label: 'Walktrap', short: 'Walktrap' },
  mcl: { label: 'Markov clustering (MCL)', short: 'MCL' },
}

export const WALKTRAP_NODE_LIMIT = 2000

// Renumbers communities 0..k-1 in order of first appearance and scores the
// partition by (resolution-free) modularity on the original graph.
export function partitionStats(g, raw) {
  const n = g.n
  const remap = new Map()
  const membership = new Int32Array(n)
  for (let i = 0; i < n; i++) {
    if (!remap.has(raw[i])) remap.set(raw[i], remap.size)
    membership[i] = remap.get(raw[i])
  }
  const count = remap.size
  const sizes = new Array(count).fill(0)
  const inW = new Float64Array(count),
    totW = new Float64Array(count)
  const twoM = 2 * g.m
  for (let u = 0; u < n; u++) {
    const cu = membership[u]
    sizes[cu]++
    totW[cu] += g.adj[u].length
    const a = g.adj[u]
    for (let k = 0; k < a.length; k++) if (membership[a[k]] === cu) inW[cu] += 1
  }
  let q = 0
  if (twoM) for (let c = 0; c < count; c++) q += inW[c] / twoM - Math.pow(totW[c] / twoM, 2)
  return { membership, count, sizes, modularity: twoM ? q : NaN }
}

// Splits communities that fall apart into connected pieces.
export function splitDisconnected(g, raw) {
  const n = g.n
  const out = new Int32Array(n).fill(-1)
  let next = 0
  for (let s0 = 0; s0 < n; s0++) {
    if (out[s0] >= 0) continue
    const c = raw[s0]
    out[s0] = next
    const stack = [s0]
    while (stack.length) {
      const v = stack.pop()
      const a = g.adj[v]
      for (let k = 0; k < a.length; k++) {
        const u = a[k]
        if (out[u] < 0 && raw[u] === c) {
          out[u] = next
          stack.push(u)
        }
      }
    }
    next++
  }
  return out
}

// page wiring, run by main.ts in the original order
export function init() {
  cy.on('add remove', () => requestAnimationFrame(updateEmptyState))

  document.getElementById('btnEmptyExample').addEventListener('click', openWelcomeExample)

  document.getElementById('btnEmpty2Example').addEventListener('click', openWelcomeExample)

  document.getElementById('btnEmpty2Db').addEventListener('click', () => switchSideTab('db'))

  document.getElementById('btnEmpty2Upload').addEventListener('click', () => {
    switchSideTab('data')
    document.getElementById('btnNormaAdd').click()
  })

  document.getElementById('btnEmpty2Welcome').addEventListener('click', () => switchTab('welcome'))

  document.getElementById('btnEmptyString').addEventListener('click', () => {
    switchSideTab('db')
    document.getElementById('stringSection').classList.remove('collapsed')
    document.querySelector('#stringSection > h3').setAttribute('aria-expanded', 'true')
    const q = document.getElementById('stringQuery')
    q.scrollIntoView({ block: 'center', behavior: 'smooth' })
    q.focus()
  })

  document.getElementById('btnEmptyUpload').addEventListener('click', () => {
    switchSideTab('data')
    document.getElementById('btnNormaAdd').click()
  })

  document
    .getElementById('btnEmptyHelp')
    .addEventListener('click', () => switchTab('help', 'help-start'))
}
