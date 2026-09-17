// @ts-nocheck
// ponytail: split verbatim from the former single-file script; typed one file at a time (remove @ts-nocheck as it lands)
import { NORMA_EXAMPLE_SETS } from './wiring'
import { downloadText } from './layouts/controls'

/* ---------- Data / Display sub-tabs ---------- */
const SIDE_TABS = [
  ['data', 'sideTabData', 'panelData'],
  ['db', 'sideTabDb', 'panelDb'],
  ['display', 'sideTabDisplay', 'panelDisplay'],
  ['export', 'sideTabExport', 'panelExport'],
]

export function switchSideTab(name) {
  SIDE_TABS.forEach(([k, tabId, panelId]) => {
    const on = k === name
    const tab = document.getElementById(tabId)
    tab.setAttribute('aria-selected', on ? 'true' : 'false')
    tab.tabIndex = on ? 0 : -1
    document.getElementById(panelId).hidden = !on
  })
  document.getElementById('sidebar').scrollTop = 0
}

/* ---------- example file links (Help > Examples) ----------
   Download links for NORMA's example files as published by the NORMA
   server, each paired with the copy bundled in this page (by example set
   and file name), which keeps working offline. */
const NORMA_DOWNLOAD_BASE =
  'https://pavlopoulos-lab-services.org/shiny/app_proxy/a64cd611-0420-4151-a75c-e1e2066f6bc5/session/ef352e36fc9ed86bf6e69f45a83d6216/download/'

const EXAMPLE_FILE_LINKS = [
  {
    title: 'STRING example 1 (TP53 interactors)',
    set: 'string-tp53',
    files: [
      ['STRING Network file', 'string_net_tp53', 'string_interactions.txt'],
      ['STRING Annotation file', 'string_annot', 'string_interactions_groups_comma_duplicate.txt'],
      ['STRING Expression file', 'string_expr', 'string_expression_colors.txt'],
    ],
  },
  {
    title: 'STRING example 2 (BCAR3 interactors)',
    set: 'string-bcar3',
    files: [
      ['STRING Network file', 'string_net_bcar3', 'BCAR3.txt'],
      ['GO Annotation - Biological Process', 'string_bp', 'BCAR3_GO_BP.txt'],
      ['GO Annotation - Molecular Function', 'string_mf', 'BCAR3_GO_MF.txt'],
      ['KEGG pathways', 'string_kegg', 'BCAR3_KEGG.txt'],
    ],
  },
  {
    title: 'Drosophila (Tau) Network',
    set: 'tau',
    refs: [
      ['PMID:31488613', 'https://pubmed.ncbi.nlm.nih.gov/31488613/'],
      ['PMCID:PMC6794924', 'https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6794924/'],
      ['DOI:10.1523/JNEUROSCI.0391-19.2019', 'https://doi.org/10.1523/JNEUROSCI.0391-19.2019'],
    ],
    files: [
      ['Drosophila Network file', 'dros_net', 'TAU_network_DEGs_NORMA.txt'],
      ['Drosophila Kegg pathways', 'dros_annot', 'TAU_KEGG_Annotation_NORMA.txt'],
      ['Drosophila Louvain automated annotation file', 'dros_louvain', 'TAU_Louvain.txt'],
      ['Drosophila Expression file', 'dros_express', 'TAU_expressions.txt'],
    ],
  },
  {
    title: 'Human Gene Co-expression Network',
    set: 'coexpr',
    refs: [
      ['PMID:19081792', 'https://pubmed.ncbi.nlm.nih.gov/19081792/'],
      ['PMCID:PMC2597745', 'https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2597745/'],
      ['DOI:10.1371/journal.pone.0003911', 'https://doi.org/10.1371/journal.pone.0003911'],
      ['bioinfow.dep.usal.es/coexpression', 'http://bioinfow.dep.usal.es/coexpression/'],
    ],
    files: [
      ['Gene Co-expression Network', 'co_express', 'NORMA_Human_coexpression_NETWORK.txt'],
      [
        'GO Annotation - Biological Process',
        'co_express_bp',
        'NORMA_Human_coexpression_Annotation_GO_BP.txt',
      ],
      [
        'GO Annotation - Molecular Function',
        'co_express_mf',
        'NORMA_Human_coexpression_Annotation_GO_MF.txt',
      ],
      [
        'GO Annotation - Cellular Components',
        'co_express_cc',
        'NORMA_Human_coexpression_Annotation_GO_CC.txt',
      ],
      ['KEGG pathways', 'co_express_kegg', 'NORMA_Human_coexpression_Annotation_KEGG.txt'],
      ['MCODE Node coloring', 'co_express_mcode', 'NORMA_Human_coexpression_Expression_MCODE.txt'],
    ],
  },
  {
    title: 'COVID-19: IntAct Database',
    set: 'covid',
    files: [
      ['COVID-19 Network', 'covid_19_net', 'Intact-data_COVID19_no_self_loops.txt'],
      [
        'HomoSapiens Protein Domains - INTERPRO',
        'covid_19_interpro',
        'HomoSapiens_Protein_Domains_INTERPRO_FILTERED.txt',
      ],
      [
        'HomoSapiens GO Annotation - Biological Process',
        'covid_19_bp',
        'HomoSapiens_Gene_Ontology_GOTERM_BP_DIRECT_FILTERED.txt',
      ],
      [
        'HomoSapiens GO Annotation - Molecular Function',
        'covid_19_mf',
        'HomoSapiens_Gene_Ontology_GOTERM_MF_DIRECT_FILTERED.txt',
      ],
      [
        'HomoSapiens Protein Domains GO Annotation - Cellular Components',
        'covid_19_cc',
        'HomoSapiens_Gene_Ontology_GOTERM_CC_DIRECT_FILTERED.txt',
      ],
      [
        'HomoSapiens Protein Domains KEGG pathways',
        'covid_19_kegg',
        'HomoSapiens_Pathways_KEGG_PATHWAY_FILTERED.txt',
      ],
      [
        'HomoSapiens Protein Domains SMART',
        'covid_19_smart',
        'HomoSapiens_Protein_Domains_SMART_FILTERED.txt',
      ],
    ],
  },
  {
    title: 'Gallus gallus: BioGrid Database',
    set: 'gallus',
    files: [
      ['Gallus gallus Network', 'Gallus_gallus_net', 'Biogrid_no_self_loops.txt'],
      [
        'BioGrid Gallus gallus KEGG pathways',
        'Gallus_gallus_kegg',
        'BioGrid_Chicken_Gallus_Pathways_KEGG_PATHWAY_FILTERED.txt',
      ],
    ],
  },
]

function bundledExampleFile(setKey, fileName) {
  const set = NORMA_EXAMPLE_SETS[setKey]
  return set ? set.files.find((f) => f.fileName === fileName) || null : null
}

function renderLocalExampleSets(root) {
  Object.values(NORMA_EXAMPLE_SETS)
    .filter((set) => set.local)
    .forEach((set) => {
      const block = document.createElement('div')
      block.className = 'dl-group'
      const h = document.createElement('h3')
      h.textContent = set.title
      block.appendChild(h)
      const list = document.createElement('ul')
      list.className = 'dl-list'
      set.files.forEach((f) => {
        const li = document.createElement('li')
        li.append(`${f.name} `)
        const btn = document.createElement('button')
        btn.type = 'button'
        btn.className = 'dl-copy'
        btn.textContent = 'Download'
        btn.title = `Download ${f.fileName}`
        btn.addEventListener('click', () => downloadText(f.fileName, f.text))
        li.appendChild(btn)
        list.appendChild(li)
      })
      block.appendChild(list)
      root.appendChild(block)
    })
}

function renderExampleLinks() {
  const root = document.getElementById('helpExampleFiles')
  if (!root) return
  root.innerHTML = ''
  renderLocalExampleSets(root)
  EXAMPLE_FILE_LINKS.forEach((group) => {
    const block = document.createElement('div')
    block.className = 'dl-group'
    const h = document.createElement('h3')
    h.textContent = group.title
    block.appendChild(h)
    if (group.refs) {
      const refs = document.createElement('p')
      refs.className = 'dl-refs'
      group.refs.forEach(([label, url], i) => {
        if (i) refs.append(', ')
        const a = document.createElement('a')
        a.href = url
        a.target = '_blank'
        a.rel = 'noopener'
        a.textContent = label
        refs.appendChild(a)
      })
      block.appendChild(refs)
    }
    const list = document.createElement('ul')
    list.className = 'dl-list'
    group.files.forEach(([label, slug, fileName]) => {
      const li = document.createElement('li')
      const a = document.createElement('a')
      const local = bundledExampleFile(group.set, fileName)
      if (local) {
        // the sample file itself, from this page, so it never goes missing
        a.href = '#'
        a.title = `Download ${fileName}`
        a.addEventListener('click', (e) => {
          e.preventDefault()
          downloadText(fileName, local.text)
        })
      } else {
        a.href = `${NORMA_DOWNLOAD_BASE}${slug}?w=`
        a.target = '_blank'
        a.rel = 'noopener'
      }
      a.textContent = label
      li.appendChild(a)
      const fn = document.createElement('span')
      fn.className = 'dl-name'
      fn.textContent = fileName
      li.appendChild(fn)
      list.appendChild(li)
    })
    block.appendChild(list)
    root.appendChild(block)
  })
}

/* ---------- search panel: collapse / expand ---------- */
function setSearchCollapsed(collapsed) {
  const panel = document.getElementById('searchPanel')
  const toggle = document.getElementById('searchToggle')
  panel.classList.toggle('collapsed', collapsed)
  toggle.setAttribute('aria-expanded', String(!collapsed))
  toggle.title = collapsed ? 'Open the search panel' : 'Minimize the search panel'
  toggle.setAttribute('aria-label', collapsed ? 'Open node search' : 'Minimize node search')
  if (!collapsed) document.getElementById('search').focus()
}

// page wiring, run by main.ts in the original order
export function init() {
  document.querySelectorAll('#sideTabs [role="tab"]').forEach((tab) => {
    tab.addEventListener('click', () => switchSideTab(tab.dataset.side))
    tab.addEventListener('keydown', (e) => {
      if (e.key !== 'ArrowLeft' && e.key !== 'ArrowRight') return
      const keys = SIDE_TABS.map((t) => t[0])
      const i = keys.indexOf(tab.dataset.side)
      const next = keys[(i + (e.key === 'ArrowRight' ? 1 : keys.length - 1)) % keys.length]
      switchSideTab(next)
      document.querySelector(`#sideTabs [data-side="${next}"]`).focus()
    })
  })

  renderExampleLinks()

  document.getElementById('searchToggle').addEventListener('click', () => {
    setSearchCollapsed(!document.getElementById('searchPanel').classList.contains('collapsed'))
  })

  document.getElementById('searchMinimize').addEventListener('click', () => {
    setSearchCollapsed(true)
    document.getElementById('searchToggle').focus()
  })
}
