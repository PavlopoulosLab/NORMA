// Cytoscape's cose layout, headless, off the main thread. See coseInWorker in run.ts.
import cytoscape from 'cytoscape'

self.onmessage = (e: MessageEvent) => {
  const { elements, style, opts } = e.data
  try {
    const cy = cytoscape({ headless: true, styleEnabled: true, elements, style })
    cy.elements()
      .layout({ ...opts, animate: false })
      .run()
    const positions: Record<string, { x: number; y: number }> = {}
    cy.nodes().forEach((n) => {
      positions[n.id()] = n.position()
    })
    self.postMessage({ positions })
  } catch (err) {
    self.postMessage({ error: String(err) })
  }
}
