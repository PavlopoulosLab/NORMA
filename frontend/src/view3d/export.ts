// @ts-nocheck
// ponytail: split verbatim from the former single-file script; typed one file at a time (remove @ts-nocheck as it lands)
import { EXPORT_MAX_PIXELS, EXPORT_MAX_SIDE } from '../export/draw'
import { canvas3d, net3d } from './state'
import { canvasPainter, svgPainter } from './painters'
import { currentTheme } from '../themes'
import { drawScene3d } from './draw'
import { fit3d } from './camera'

/* ---------- image export of the 3D view ---------- */
export function export3dSize(scale) {
  const cv = canvas3d()
  const W = cv.clientWidth || 800,
    H = cv.clientHeight || 600
  let k = scale
  const shrink = Math.min(
    1,
    EXPORT_MAX_SIDE / (W * k),
    EXPORT_MAX_SIDE / (H * k),
    Math.sqrt(EXPORT_MAX_PIXELS / (W * H * k * k))
  )
  k *= shrink
  return { W, H, k, outW: Math.round(W * k), outH: Math.round(H * k), limited: shrink < 1 }
}

function export3dCamera(area) {
  if (area !== 'full') return { ...net3d.cam }
  const saved = { ...net3d.cam }
  fit3d(null, false)
  const cam = { ...net3d.cam }
  Object.assign(net3d.cam, saved)
  return cam
}

export function renderExportCanvas3d(o) {
  const { W, H, k, outW, outH } = export3dSize(o.scale)
  const canvas = document.createElement('canvas')
  canvas.width = outW
  canvas.height = outH
  const ctx = canvas.getContext('2d')
  const savedHulls = document.getElementById('showGroupHulls').checked
  if (!o.hulls) document.getElementById('showGroupHulls').checked = false
  try {
    drawScene3d(canvasPainter(ctx), W, H, {
      cam: export3dCamera(o.area),
      bg: o.bg,
      pixelScale: k,
      offscreen: true,
      fogColor: o.bg || currentTheme.bg,
    })
  } finally {
    document.getElementById('showGroupHulls').checked = savedHulls
  }
  return canvas
}

export function buildSvg3d(o) {
  const { W, H } = export3dSize(1)
  const s = Math.min(o.scale, 8)
  const painter = svgPainter(Math.round(W * s), Math.round(H * s))
  const savedHulls = document.getElementById('showGroupHulls').checked
  if (!o.hulls) document.getElementById('showGroupHulls').checked = false
  try {
    drawScene3d(painter, W, H, {
      cam: export3dCamera(o.area),
      bg: o.bg,
      pixelScale: s,
      offscreen: true,
      fogColor: o.bg || currentTheme.bg,
    })
  } finally {
    document.getElementById('showGroupHulls').checked = savedHulls
  }
  return new Blob([painter.toString(o.title)], { type: 'image/svg+xml' })
}

/* ============================================================
   STRING IMPORT
   Queries the STRING database (https://string-db.org, Szklarczyk et al.)
   through its public API:
     1. get_string_ids   resolve the typed names for the chosen organism
     2. network          the query proteins plus their best interactors
     3. get_string_ids   descriptions for every protein in the network
     4. enrichment or functional_annotation   term collections for the
        network's proteins (GO, KEGG, Reactome, keywords, domains,
        diseases, tissues, ...), turned into NORMA groupings
   The network and each term collection become ordinary files in the
   library, so everything else in NORMA works on them unchanged.
   STRING asks callers to wait a second between requests and to identify
   themselves (caller_identity); both are respected here.
   ============================================================ */
export const STRING_CALLER = 'NORMA3'

export const STRING_SPECIES = [
  [9606, 'Homo sapiens (human)'],
  [10090, 'Mus musculus (mouse)'],
  [10116, 'Rattus norvegicus (rat)'],
  [7955, 'Danio rerio (zebrafish)'],
  [7227, 'Drosophila melanogaster (fruit fly)'],
  [6239, 'Caenorhabditis elegans (nematode)'],
  [4932, 'Saccharomyces cerevisiae (baker\u2019s yeast)'],
  [4896, 'Schizosaccharomyces pombe (fission yeast)'],
  [3702, 'Arabidopsis thaliana (thale cress)'],
  [39947, 'Oryza sativa japonica (rice)'],
  [511145, 'Escherichia coli K-12 MG1655'],
  [224308, 'Bacillus subtilis 168'],
  [83332, 'Mycobacterium tuberculosis H37Rv'],
  [9031, 'Gallus gallus (chicken)'],
  [9913, 'Bos taurus (cattle)'],
  [9823, 'Sus scrofa (pig)'],
  [9615, 'Canis lupus familiaris (dog)'],
  [8364, 'Xenopus tropicalis (western clawed frog)'],
  [36329, 'Plasmodium falciparum 3D7 (malaria parasite)'],
]

// score field -> NORMA channel (the classic palette uses STRING's colors)
export const STRING_CHANNELS = [
  ['nscore', 'neighborhood', 'Gene neighborhood'],
  ['fscore', 'fusion', 'Gene fusion'],
  ['pscore', 'cooccurrence', 'Co-occurrence'],
  ['ascore', 'coexpression', 'Co-expression'],
  ['escore', 'experiments', 'Experiments'],
  ['dscore', 'database', 'Curated databases'],
  ['tscore', 'textmining', 'Text mining'],
]

// STRING category key -> readable name, in the order they are offered
export const STRING_CATEGORIES = [
  ['Process', 'GO Biological Process', true],
  ['Function', 'GO Molecular Function', true],
  ['Component', 'GO Cellular Component', true],
  ['KEGG', 'KEGG pathways', true],
  ['RCTM', 'Reactome pathways', true],
  ['WikiPathways', 'WikiPathways', false],
  ['Keyword', 'UniProt keywords', true],
  ['Pfam', 'Pfam domains', false],
  ['InterPro', 'InterPro domains and features', true],
  ['SMART', 'SMART domains', false],
  ['DISEASES', 'Diseases (DISEASES)', true],
  ['TISSUES', 'Tissues (TISSUES)', false],
  ['COMPARTMENTS', 'Subcellular localization (COMPARTMENTS)', false],
  ['HPO', 'Human phenotypes (Monarch)', false],
  ['MPO', 'Mammalian phenotypes (Monarch)', false],
  ['DPO', 'Drosophila phenotypes (Monarch)', false],
  ['WPO', 'C. elegans phenotypes (Monarch)', false],
  ['ZPO', 'Zebrafish phenotypes (Monarch)', false],
  ['FYPO', 'Fission yeast phenotypes (Monarch)', false],
  ['NetworkNeighborAL', 'Local network clusters (STRING)', false],
  ['PMID', 'Reference publications (PubMed)', false],
]

const STRING_CATEGORY_ALIASES = {
  'go process': 'Process',
  'biological process': 'Process',
  'go function': 'Function',
  'molecular function': 'Function',
  'go component': 'Component',
  'cellular component': 'Component',
  'kegg pathways': 'KEGG',
  reactome: 'RCTM',
  'reactome pathways': 'RCTM',
  'uniprot keywords': 'Keyword',
  keywords: 'Keyword',
}

export const STRING_PRIOR = 0.041

export const STRING_PREFERRED_GROUPING = [
  'KEGG',
  'RCTM',
  'Process',
  'WikiPathways',
  'Function',
  'Component',
  'Keyword',
  'InterPro',
  'DISEASES',
]

export function stringCategoryKey(raw) {
  if (!raw) return 'Other'
  if (STRING_CATEGORIES.some((c) => c[0] === raw)) return raw
  return STRING_CATEGORY_ALIASES[String(raw).toLowerCase()] || raw
}

export function stringCategoryName(key) {
  const c = STRING_CATEGORIES.find((x) => x[0] === key)
  return c ? c[1] : key
}
