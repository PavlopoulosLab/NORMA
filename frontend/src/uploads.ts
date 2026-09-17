// @ts-nocheck
// ponytail: split verbatim from the former single-file script; typed one file at a time (remove @ts-nocheck as it lands)
import {
  NORMA_KIND_NAMES,
  addNormaEntry,
  annotationText,
  articleFor,
  detectNormaKind,
  downloadText,
  fileStem,
  libSelection,
  listSample,
  normaLibrary,
  plural,
  renderLibraryLists,
  selectedNetworks,
  setStatus,
} from './layouts/controls'
import { convertArena3dNetwork, isArena3dNetworkText } from './arena3d'

/* ---------- uploads ---------- */
export function readFileText(file) {
  return new Promise((resolve, reject) => {
    const reader = new FileReader()
    reader.onload = (ev) => resolve(ev.target.result)
    reader.onerror = () =>
      reject(new Error('The file could not be read. Check that it is a plain text file.'))
    reader.readAsText(file)
  })
}

// Splits an annotation's members into those present in `known` and those not.
function checkAnnotationAgainst(entry, known) {
  const missing = new Set()
  const emptied = []
  const kept = []
  entry.parsed.groups.forEach(({ name, members }) => {
    const present = members.filter((m) => known.has(m))
    members.forEach((m) => {
      if (!known.has(m)) missing.add(m)
    })
    if (present.length) kept.push({ name, members: present })
    else emptied.push(name)
  })
  return { missing: [...missing], emptied, kept }
}

export async function handleNormaUploads(fileList) {
  const files = [...fileList]
  if (!files.length) return
  const chosenKind = document.getElementById('normaKind').value
  const nameInput = document.getElementById('normaName')
  const typedName = files.length === 1 ? nameInput.value.trim() : ''
  const notes = []
  const added = []
  const addedNotes = new Map()
  for (const file of files) {
    try {
      const text = await readFileText(file)
      if ((chosenKind === 'auto' || chosenKind === 'network') && isArena3dNetworkText(text)) {
        // an Arena3D network file: a network plus its layers as groups
        const conv = convertArena3dNetwork(text)
        const base = typedName || file.name.replace(/\.[^.]+$/, '') || file.name
        const netEntry = addNormaEntry('network', base, conv.network, file.name, null, {
          directed: document.getElementById('normaDirected').value === 'directed',
        })
        const annEntry = addNormaEntry(
          'annotation',
          `${base} layers`,
          conv.annotation,
          file.name,
          null,
          {}
        )
        ;[netEntry, annEntry].forEach((entry) => {
          added.push(entry)
          const note = {
            level: 'ok',
            text: `Added ${articleFor(NORMA_KIND_NAMES[entry.kind])} ${NORMA_KIND_NAMES[entry.kind]} "${entry.name}" from an Arena3D network file: ${entry.parsed.summary}.`,
          }
          addedNotes.set(entry, note)
          notes.push(note)
          entry.parsed.notes.forEach((t) =>
            notes.push({ level: 'warn', text: `${entry.name}: ${t}` })
          )
        })
        continue
      }
      const kind = chosenKind === 'auto' ? detectNormaKind(text) : chosenKind
      if (!kind) throw new Error('The file is empty.')
      const name = typedName || file.name.replace(/\.[^.]+$/, '') || file.name
      const entry = addNormaEntry(kind, name, text, file.name, null, {
        directed: document.getElementById('normaDirected').value === 'directed',
      })
      added.push(entry)
      const addedNote = {
        level: 'ok',
        text: `Added ${articleFor(NORMA_KIND_NAMES[kind])} ${NORMA_KIND_NAMES[kind]} "${entry.name}": ${entry.parsed.summary}.`,
      }
      addedNotes.set(entry, addedNote)
      notes.push(addedNote)
      entry.parsed.notes.forEach((t) => notes.push({ level: 'warn', text: `${entry.name}: ${t}` }))
    } catch (err) {
      const kindText =
        chosenKind === 'auto'
          ? 'a file'
          : `${articleFor(NORMA_KIND_NAMES[chosenKind])} ${NORMA_KIND_NAMES[chosenKind]}`
      notes.push({
        level: 'error',
        text: `"${file.name}" wasn't added as ${kindText}. ${err.message}`,
      })
    }
  }
  // Check new annotations against the networks they will most likely be
  // used with: networks uploaded together with them, otherwise the ticked
  // networks, otherwise every network in the list. Names missing from all
  // of those are discarded now, with a download of the corrected file.
  const newAnnotations = added.filter((e) => e.kind === 'annotation')
  if (newAnnotations.length) {
    const batchNets = added.filter((e) => e.kind === 'network')
    const tickedNets = selectedNetworks()
    const refNets = batchNets.length
      ? batchNets
      : tickedNets.length
        ? tickedNets
        : normaLibrary.network
    const refNames =
      refNets.length === 1
        ? `"${refNets[0].name}"`
        : `the ${refNets.length} ${batchNets.length ? 'uploaded' : tickedNets.length ? 'ticked' : 'listed'} networks`
    if (!refNets.length) {
      notes.push({
        level: 'ok',
        text: 'No network is loaded yet, so annotation names will be checked when you show them with a network.',
      })
    }
    const known = new Set(refNets.flatMap((e) => e.parsed.nodes))
    newAnnotations.forEach((entry) => {
      if (!refNets.length) return
      const check = checkAnnotationAgainst(entry, known)
      if (!check.missing.length) return
      if (!check.kept.length) {
        normaLibrary.annotation = normaLibrary.annotation.filter((x) => x !== entry)
        added.splice(added.indexOf(entry), 1)
        notes.splice(notes.indexOf(addedNotes.get(entry)), 1)
        notes.push({
          level: 'error',
          text: `"${entry.name}" was not kept: none of its ${plural(check.missing.length, 'node name')} are in ${refNames}. Check that the annotation belongs to this network.`,
        })
        return
      }
      entry.parsed.groups = check.kept
      entry.parsed.summary = plural(check.kept.length, 'group')
      addedNotes.get(entry).text =
        `Added the annotation "${entry.name}": ${entry.parsed.summary} after the check below.`
      entry.correctedText = annotationText(check.kept)
      const parts = [
        `Discarded ${plural(check.missing.length, 'node name')} from "${entry.name}" that ${check.missing.length === 1 ? 'is' : 'are'} not in ${refNames}: ${listSample(check.missing)}.`,
      ]
      if (check.emptied.length)
        parts.push(
          `${plural(check.emptied.length, 'group')} left with no nodes ${check.emptied.length === 1 ? 'was' : 'were'} removed: ${listSample(check.emptied)}.`
        )
      const fileName = `${fileStem(entry.name)}-corrected.txt`
      notes.push({
        level: 'warn',
        text: parts.join(' '),
        action: {
          label: 'Download corrected annotation',
          run: () => downloadText(fileName, entry.correctedText),
        },
      })
    })
  }
  if (added.length) {
    nameInput.value = ''
    // Select what was just added: new networks replace the network
    // selection; the last new annotation / expression becomes the choice.
    const newNets = added.filter((e) => e.kind === 'network')
    if (newNets.length) libSelection.networks = new Set(newNets.map((e) => e.id))
    const lastAnn = added.filter((e) => e.kind === 'annotation').pop()
    if (lastAnn) libSelection.annotation = lastAnn.id
    const lastCol = added.filter((e) => e.kind === 'colors').pop()
    if (lastCol) libSelection.colors = lastCol.id
    notes.push({
      level: 'ok',
      text: 'The new files are ticked. Choose Show in this view or Open in new view to see them.',
    })
  }
  renderLibraryLists()
  setStatus('normaStatus', notes)
}
