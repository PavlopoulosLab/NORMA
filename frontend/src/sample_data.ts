// @ts-nocheck
// ponytail: split verbatim from the former single-file script; typed one file at a time (remove @ts-nocheck as it lands)
/* ---------- sample data, modeled on the STRING trp-operon screenshot ---------- */
function buildSampleData() {
  // seeded, so the demo (and its download in Help) is the same every time
  const rand = mulberry32(3)
  const hub = ['trpA', 'trpB', 'trpC', 'trpD', 'trpE', 'pabB', 'tyrA']
  const secondGroup = {
    trpA: 'indole-pathway',
    pabB: 'folate-pathway',
    tyrA: 'tyrosine-pathway',
  }
  // node attributes: arbitrary metadata about an individual node, shown in
  // its info popup (distinct from group attributes below, which describe
  // the *group* rather than any one member).
  const geneFunction = {
    trpA: 'Tryptophan synthase, alpha subunit',
    trpB: 'Tryptophan synthase, beta subunit',
    trpC: 'Indole-3-glycerol-phosphate synthase',
    trpD: 'Anthranilate phosphoribosyltransferase',
    trpE: 'Anthranilate synthase component I',
    pabB: 'Aminodeoxychorismate synthase component I',
    tyrA: 'Chorismate mutase / prephenate dehydrogenase',
    lgt: 'Prolipoprotein diacylglyceryl transferase',
    putP: 'Sodium/proline symporter',
    tnaA: 'Tryptophanase',
    glyA: 'Serine hydroxymethyltransferase',
  }
  const ecNumber = {
    trpA: '4.2.1.20',
    trpB: '4.2.1.20',
    trpC: '4.1.1.48',
    trpD: '2.4.2.18',
    trpE: '4.1.3.27',
    tnaA: '4.1.99.1',
    glyA: '2.1.2.1',
  }
  const nodes = [
    ...hub.map((id) => ({
      id,
      groups: secondGroup[id] ? ['core', secondGroup[id]] : ['core'],
      size: id === 'trpA' ? 54 : 46,
      function: geneFunction[id],
      ...(ecNumber[id] ? { ecNumber: ecNumber[id] } : {}),
    })),
    { id: 'lgt', groups: ['peripheral'], size: 40, function: geneFunction.lgt },
    { id: 'putP', groups: ['peripheral', 'transport'], size: 40, function: geneFunction.putP },
    {
      id: 'tnaA',
      groups: ['peripheral', 'tryptophan-catabolism'],
      size: 40,
      function: geneFunction.tnaA,
      ecNumber: ecNumber.tnaA,
    },
    {
      id: 'glyA',
      groups: ['peripheral'],
      size: 40,
      function: geneFunction.glyA,
      ecNumber: ecNumber.glyA,
    },
  ]

  // group attributes: metadata about each group as a whole -- a nicer
  // display label and a short description -- shown in the Node groups
  // legend (as the row's label and hover tooltip) and in a node's info
  // popup subtitle.
  const groupAttrs = {
    core: { label: 'Core operon', description: 'Tryptophan biosynthesis operon genes (trpA-E)' },
    'indole-pathway': {
      label: 'Indole pathway',
      description: 'Shared branch point with indole/tryptophan interconversion',
    },
    'folate-pathway': {
      label: 'Folate pathway',
      description: 'para-aminobenzoate / folate biosynthesis branch',
    },
    'tyrosine-pathway': {
      label: 'Tyrosine pathway',
      description: 'Aromatic amino acid biosynthesis branch',
    },
    peripheral: {
      label: 'Peripheral genes',
      description: 'Functionally related genes outside the core operon',
    },
    transport: { label: 'Transport', description: 'Membrane transport function' },
    'tryptophan-catabolism': {
      label: 'Trp catabolism',
      description: 'Tryptophan breakdown pathway',
    },
  }

  const edges = []
  let eid = 0
  const pairTypes = [
    'neighborhood',
    'cooccurrence',
    'coexpression',
    'experiments',
    'textmining',
    'database',
  ]

  // dense, richly multi-edged core cluster
  for (let i = 0; i < hub.length; i++) {
    for (let j = i + 1; j < hub.length; j++) {
      const a = hub[i],
        b = hub[j]
      const n = 3 + Math.floor(rand() * 3) // 3-5 parallel edges
      const shuffled = [...pairTypes].sort(() => rand() - 0.5).slice(0, n)
      shuffled.forEach((t) => {
        // edge attributes: arbitrary metadata about an individual edge
        // (evidenceCount here), shown in its popup alongside weight/type.
        edges.push({
          id: 'e' + eid++,
          source: a,
          target: b,
          type: t,
          weight: +(0.4 + rand() * 0.59).toFixed(2),
          evidenceCount: 1 + Math.floor(rand() * 14),
        })
      })
    }
  }

  // peripheral, sparser connections
  edges.push({ id: 'e' + eid++, source: 'lgt', target: 'trpA', type: 'coexpression' })
  edges.push({ id: 'e' + eid++, source: 'lgt', target: 'trpA', type: 'textmining' })

  edges.push({ id: 'e' + eid++, source: 'putP', target: 'trpA', type: 'database' })
  edges.push({ id: 'e' + eid++, source: 'putP', target: 'pabB', type: 'database' })
  edges.push({ id: 'e' + eid++, source: 'putP', target: 'tyrA', type: 'database' })

  edges.push({ id: 'e' + eid++, source: 'tnaA', target: 'trpB', type: 'database' })
  edges.push({ id: 'e' + eid++, source: 'tnaA', target: 'trpB', type: 'cooccurrence' })
  edges.push({ id: 'e' + eid++, source: 'tnaA', target: 'trpA', type: 'database' })

  edges.push({ id: 'e' + eid++, source: 'glyA', target: 'trpB', type: 'database' })
  edges.push({ id: 'e' + eid++, source: 'glyA', target: 'trpB', type: 'cooccurrence' })

  return { nodes, edges, groupAttrs }
}

// Procedurally generates a plausible multi-group, multi-channel network of a
// given size, for demonstrating the tool across a range of scales (a handful
// of nodes up to several thousand). Group count grows with network size;
// edges are a random spanning backbone (guarantees connectivity) plus extra
// random links for density, each carrying a random channel and weight.
export function generateRandomNetwork(n, opts = {}) {
  // seeded, so each demo (and its download in Help) is the same every time
  const rand = mulberry32(opts.seed ?? n * 7919 + 17)
  const INTERACTIONS = ['activates', 'inhibits', 'binds', 'phosphorylates', 'regulates']
  const groupCount = opts.groupCount || Math.max(2, Math.min(16, Math.round(Math.sqrt(n))))
  const groupNames = Array.from({ length: groupCount }, (_, i) => 'group-' + (i + 1))
  const edgeTypePool = [
    'neighborhood',
    'cooccurrence',
    'coexpression',
    'experiments',
    'database',
    'textmining',
  ]

  const nodes = []
  for (let i = 0; i < n; i++) {
    const primary = groupNames[i % groupNames.length]
    const groups = [primary]
    if (rand() < 0.12) {
      const second = groupNames[Math.floor(rand() * groupNames.length)]
      if (second !== primary) groups.push(second)
    }
    nodes.push({ id: 'n' + i, groups, size: 30 + rand() * 20 })
  }

  const edges = []
  let eid = 0
  const makeEdge = (i, j) => ({
    id: 'ge' + eid++,
    source: 'n' + i,
    target: 'n' + j,
    type: edgeTypePool[Math.floor(rand() * edgeTypePool.length)],
    weight: +(0.3 + rand() * 0.69).toFixed(2),
    ...(opts.edgeLabels
      ? { interaction: INTERACTIONS[Math.floor(rand() * INTERACTIONS.length)] }
      : {}),
  })

  // random spanning backbone -- every node (after the first) attaches to
  // some earlier node, so the whole network stays connected
  for (let i = 1; i < n; i++) {
    edges.push(makeEdge(i, Math.floor(rand() * i)))
  }
  // extra random edges for density
  const avgExtra = opts.avgExtraEdges ?? (n <= 100 ? 2 : n <= 1000 ? 1.2 : 0.6)
  const extraCount = Math.round(n * avgExtra)
  for (let k = 0; k < extraCount; k++) {
    const a = Math.floor(rand() * n)
    const b = Math.floor(rand() * n)
    if (a === b) continue
    edges.push(makeEdge(a, b))
  }

  const data = { nodes, edges }
  if (opts.edgeLabels) {
    // open with each edge labelled by its interaction, on curved edges so
    // parallel labels don't sit on top of each other
    data.config = {
      showEdgeLabels: true,
      edgeLabelContent: 'attr:interaction',
      edgeLabelSize: '10',
      edgeCurveStyle: 'bezier',
    }
  }
  return data
}

// A directed multi-edge network: each connected pair gets one to three
// channels (regulation, phosphorylation, binding, ...), each drawn as an
// arrow from source to target; some pairs also have edges in the reverse
// direction, so reciprocal and parallel arrows can be explored.
export function generateDirectedMultiNetwork(n, opts = {}) {
  const rand = mulberry32(opts.seed ?? 4242)
  const channels = ['activation', 'inhibition', 'phosphorylation', 'binding', 'expression']
  const groupNames = ['receptors', 'kinases', 'transcription factors', 'targets']
  const groupAttrs = {
    receptors: { description: 'Upstream sensors that start the signal' },
    kinases: { description: 'Relay the signal by phosphorylation' },
    'transcription factors': { description: 'Turn target genes on or off' },
    targets: { description: 'Downstream genes and effectors' },
  }
  const nodes = []
  const layerOf = []
  for (let i = 0; i < n; i++) {
    // nodes are spread over four layers, roughly a signalling cascade
    const layer = Math.min(3, Math.floor((i / n) * 4))
    layerOf.push(layer)
    const groups = [groupNames[layer]]
    if (rand() < 0.1 && layer < 3) groups.push(groupNames[layer + 1])
    nodes.push({ id: ['R', 'K', 'TF', 'G'][layer] + (i + 1), groups, size: 34 + rand() * 16 })
  }
  const edges = []
  let eid = 0
  const addPair = (a, b) => {
    const count = 1 + Math.floor(rand() * 3) // 1-3 channels
    const picked = [...channels].sort(() => rand() - 0.5).slice(0, count)
    picked.forEach((type) =>
      edges.push({
        id: 'de' + eid++,
        source: nodes[a].id,
        target: nodes[b].id,
        type,
        weight: +(0.3 + rand() * 0.7).toFixed(2),
        directed: true,
      })
    )
    if (rand() < 0.25) {
      // feedback in the other direction
      edges.push({
        id: 'de' + eid++,
        source: nodes[b].id,
        target: nodes[a].id,
        type: rand() < 0.5 ? 'inhibition' : 'binding',
        weight: +(0.3 + rand() * 0.7).toFixed(2),
        directed: true,
      })
    }
  }
  const used = new Set()
  const pairOnce = (a, b) => {
    const key = a < b ? a + ',' + b : b + ',' + a
    if (a === b || used.has(key)) return
    used.add(key)
    addPair(a, b)
  }
  // every node below the top layer is reached from an earlier layer
  for (let i = 0; i < n; i++) {
    if (layerOf[i] === 0) continue
    const upstream = []
    for (let j = 0; j < n; j++) if (layerOf[j] === layerOf[i] - 1) upstream.push(j)
    pairOnce(upstream[Math.floor(rand() * upstream.length)], i)
  }
  // extra links within and across neighbouring layers
  for (let k = 0; k < n * 0.9; k++) {
    const a = Math.floor(rand() * n)
    const candidates = []
    for (let j = 0; j < n; j++)
      if (layerOf[j] === layerOf[a] || layerOf[j] === layerOf[a] + 1) candidates.push(j)
    pairOnce(a, candidates[Math.floor(rand() * candidates.length)])
  }
  return {
    nodes,
    edges,
    groupAttrs,
    groupOrder: groupNames,
    directed: true,
    config: {
      edgeDirection: 'data',
      edgeCurveStyle: 'bezier',
      edgeCurvature: '28',
      arrowShape: 'triangle',
      arrowScale: '1',
      layoutSelect: 'breadthfirst',
    },
  }
}

// Small seeded PRNG so the module examples come out identical on every load.
export function mulberry32(seed) {
  return function () {
    seed |= 0
    seed = (seed + 0x6d2b79f5) | 0
    let t = Math.imul(seed ^ (seed >>> 15), 1 | seed)
    t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296
  }
}

// Four synthetic functional modules (not real genes). Each module biases
// what its members look like (localization, domains) and which evidence
// channels its internal links tend to come from.
const MODULE_DEFS = [
  {
    key: 'signaling',
    prefix: 'SIG',
    label: 'Signaling',
    category: 'regulatory',
    description: 'Receptors and kinases that relay outside cues into the cell',
    localization: ['membrane', 'cytoplasm', 'cytoplasm'],
    domains: ['kinase', 'SH2', 'PH', 'GTPase'],
    channels: ['experiments', 'experiments', 'coexpression', 'textmining'],
  },
  {
    key: 'metabolism',
    prefix: 'MET',
    label: 'Central metabolism',
    category: 'enzymatic',
    description: 'Enzymes that turn nutrients into energy and building blocks',
    localization: ['cytoplasm', 'mitochondrion', 'mitochondrion'],
    domains: ['Rossmann fold', 'TIM barrel', 'aldolase', 'NAD-binding'],
    channels: ['database', 'neighborhood', 'cooccurrence', 'coexpression'],
  },
  {
    key: 'transport',
    prefix: 'TRN',
    label: 'Membrane transport',
    category: 'transport',
    description: 'Carriers and channels that move solutes across membranes',
    localization: ['membrane', 'membrane', 'vesicle'],
    domains: ['MFS', 'ABC', 'ion channel', 'porin'],
    channels: ['cooccurrence', 'database', 'neighborhood', 'experiments'],
  },
  {
    key: 'transcription',
    prefix: 'TSC',
    label: 'Transcription control',
    category: 'regulatory',
    description: 'Factors that switch target genes on and off',
    localization: ['nucleus', 'nucleus', 'cytoplasm'],
    domains: ['zinc finger', 'HTH', 'bZIP', 'bromodomain'],
    channels: ['textmining', 'coexpression', 'experiments', 'database'],
  },
]

const CROSS_MODULE_CHANNELS = ['textmining', 'database', 'cooccurrence']

// Builds a 4-module network that exercises all three attribute levels:
//   node  attrs: localization (text), expression (number), essential (yes/no),
//                length (integer), domains (list)
//   edge  attrs: evidenceCount (integer), firstReported (year)
//   group attrs: label/description (built-in) plus category, enrichmentFDR,
//                curated, keyMembers (custom)
// sizes    -- members per module (before overlaps), sums to the node count
// overlaps -- one entry per shared node: [homeModule, ...extraModules]
// pIn/pOut -- link probability for a node pair inside / across modules
export function buildModuleNetwork({ sizes, overlaps, pIn, pOut, seed }) {
  const rand = mulberry32(seed)
  const pick = (arr) => arr[Math.floor(rand() * arr.length)]
  const shuffle = (arr) => {
    const a = arr.slice()
    for (let i = a.length - 1; i > 0; i--) {
      const j = Math.floor(rand() * (i + 1))
      ;[a[i], a[j]] = [a[j], a[i]]
    }
    return a
  }
  const pickDistinct = (pool, n) => {
    const out = []
    const distinct = new Set(pool).size
    while (out.length < Math.min(n, distinct)) {
      const v = pick(pool)
      if (!out.includes(v)) out.push(v)
    }
    return out
  }

  // 1. nodes, each with a single home module
  const nodes = []
  const members = MODULE_DEFS.map(() => [])
  MODULE_DEFS.forEach((m, gi) => {
    for (let i = 0; i < sizes[gi]; i++) {
      const node = {
        id: `${m.prefix}-${String(i + 1).padStart(2, '0')}`,
        groups: [m.key],
        size: 30 + Math.round(rand() * 16),
        localization: pick(m.localization),
        expression: +((rand() * 2 - 1) * 3).toFixed(2),
        essential: rand() < 0.2,
        length: 120 + Math.round(rand() * 1280),
        domains: pickDistinct(m.domains, rand() < 0.35 ? 2 : 1),
      }
      nodes.push(node)
      members[gi].push(node)
    }
  })

  // 2. small overlaps: a few nodes join one or two extra modules and pick
  // up a domain typical of each module they join
  overlaps.forEach(([home, ...extra]) => {
    const candidates = members[home].filter((n) => n.groups.length === 1)
    if (!candidates.length) return
    const node = pick(candidates)
    extra.forEach((gj) => {
      const m = MODULE_DEFS[gj]
      node.groups.push(m.key)
      members[gj].push(node)
      const d = pick(m.domains)
      if (!node.domains.includes(d)) node.domains.push(d)
    })
  })

  // 3. edges: 1-3 parallel channels per linked pair
  const edges = []
  const seenPairs = new Set()
  const degree = {}
  let eid = 0
  const link = (a, b, channelPool) => {
    if (a === b) return
    const key = a.id < b.id ? a.id + '|' + b.id : b.id + '|' + a.id
    if (seenPairs.has(key)) return
    seenPairs.add(key)
    const channelCount = 1 + (rand() < 0.45 ? 1 : 0) + (rand() < 0.15 ? 1 : 0)
    pickDistinct(channelPool, channelCount).forEach((type) => {
      edges.push({
        id: 'm' + eid++,
        source: a.id,
        target: b.id,
        type,
        weight: +(0.4 + rand() * 0.59).toFixed(2),
        evidenceCount: 1 + Math.floor(rand() * 12),
        firstReported: 1995 + Math.floor(rand() * 30),
      })
      degree[a.id] = (degree[a.id] || 0) + 1
      degree[b.id] = (degree[b.id] || 0) + 1
    })
  }

  // inside each module: a random tree keeps the module connected, then
  // extra links at density pIn
  members.forEach((list, gi) => {
    const pool = MODULE_DEFS[gi].channels
    const order = shuffle(list)
    for (let i = 1; i < order.length; i++) link(order[i], order[Math.floor(rand() * i)], pool)
    for (let i = 0; i < list.length; i++) {
      for (let j = i + 1; j < list.length; j++) {
        if (rand() < pIn) link(list[i], list[j], pool)
      }
    }
  })

  // across modules: sparse links between nodes that share no module
  for (let i = 0; i < nodes.length; i++) {
    for (let j = i + 1; j < nodes.length; j++) {
      const a = nodes[i],
        b = nodes[j]
      if (a.groups.some((g) => b.groups.includes(g))) continue
      if (rand() < pOut) link(a, b, CROSS_MODULE_CHANNELS)
    }
  }

  // 4. group attributes
  const groupAttrs = {}
  MODULE_DEFS.forEach((m, gi) => {
    const keyMembers = members[gi]
      .slice()
      .sort((a, b) => (degree[b.id] || 0) - (degree[a.id] || 0))
      .slice(0, 3)
      .map((n) => n.id)
    groupAttrs[m.key] = {
      label: m.label,
      description: m.description,
      category: m.category,
      enrichmentFDR: +Math.pow(10, -(2 + rand() * 6)).toPrecision(2),
      curated: gi % 2 === 0,
      keyMembers,
    }
  })

  return { nodes, edges, groupAttrs }
}

export const SAMPLE_GENERATORS = {
  trp: () => buildSampleData(),
  // 50 nodes; 5 shared nodes (10%): one bridge per neighbouring module pair
  // around the ring, plus one node sitting in three modules
  modules50: () =>
    buildModuleNetwork({
      sizes: [14, 12, 13, 11],
      overlaps: [
        [0, 1],
        [1, 2],
        [2, 3],
        [3, 0],
        [0, 1, 2],
      ],
      pIn: 0.26,
      pOut: 0.012,
      seed: 50,
    }),
  // 100 nodes; 10 shared nodes (10%): two bridges per neighbouring pair,
  // one across the ring and one node in three modules
  modules100: () =>
    buildModuleNetwork({
      sizes: [28, 24, 26, 22],
      overlaps: [
        [0, 1],
        [1, 0],
        [1, 2],
        [2, 1],
        [2, 3],
        [3, 2],
        [3, 0],
        [0, 3],
        [0, 2],
        [1, 2, 3],
      ],
      pIn: 0.16,
      pOut: 0.006,
      seed: 100,
    }),
  tiny: () => generateRandomNetwork(20, { edgeLabels: true, avgExtraEdges: 1 }),
  directed: () => generateDirectedMultiNetwork(40),
  small: () => generateRandomNetwork(60),
  medium: () => generateRandomNetwork(200),
  large: () => generateRandomNetwork(800),
  massive: () => generateRandomNetwork(5000, { avgExtraEdges: 0.6 }),
}
