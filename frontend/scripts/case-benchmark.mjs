// Re-runs the layout benchmark of the hereditary cancer gene panel use case
// in headless Chromium and writes the table as TSV:
//   node scripts/case-benchmark.mjs --network net.tsv --annotation groups.tsv \
//        --groups "Homologous recombination|Fanconi anemia pathway|..." \
//        --channels "experiments|database" --repeats 3 --out benchmark.tsv
// The page must be served first (python3 ../backend/server.py --port 8765).
import { chromium } from '@playwright/test'
import { writeFileSync } from 'node:fs'

const arg = (name, dflt) => {
  const i = process.argv.indexOf(name)
  return i > 0 ? process.argv[i + 1] : dflt
}
const url = arg('--url', 'http://127.0.0.1:8765/')
const network = arg('--network')
const annotation = arg('--annotation')
const groups = arg('--groups', '').split('|').filter(Boolean)
const channels = arg('--channels', '').split('|').filter(Boolean)
const repeats = arg('--repeats', '3')
const local = arg('--local', 'circle')
const algorithm = arg('--algorithm', 'fr')
const force = arg('--force', '10')
const out = arg('--out', 'benchmark.tsv')
const figure = arg('--figure', '') // optional PNG of the view after applying a benchmark row
const useLayout = arg('--use', '') // label of the benchmark row to apply before the figure

const browser = await chromium.launch()
const page = await browser.newPage({ viewport: { width: 1400, height: 1000 } })
page.setDefaultTimeout(180000)

// sidebar controls may sit in collapsed sections, so values are set directly
// and the events the app listens to are dispatched
const setValue = (sel, value) =>
  page.evaluate(
    ([sel, value]) => {
      const el = document.querySelector(sel)
      if (!el) throw new Error(`no element ${sel}`)
      el.value = value
      el.dispatchEvent(new Event('input', { bubbles: true }))
      el.dispatchEvent(new Event('change', { bubbles: true }))
    },
    [sel, value]
  )
const tickOnly = (attr, wanted) =>
  page.evaluate(
    ([attr, wanted]) => {
      let changed = 0
      document.querySelectorAll(`input[type=checkbox][${attr}]`).forEach((box) => {
        const want = wanted.includes(box.getAttribute(attr))
        if (box.checked === want) return
        box.checked = want
        box.dispatchEvent(new Event('change', { bubbles: true }))
        changed++
      })
      return changed
    },
    [attr, wanted]
  )
const idle = (id) =>
  page.waitForFunction((id) => {
    const s = document.getElementById(id)
    return !s || !/…/.test(s.textContent)
  }, id)

await page.goto(url)
await page.locator('#filesSection h3').click()
await page.setInputFiles('#normaFileInput', [network, annotation])
await page.locator('#normaStatus .note.ok').first().waitFor()
await page.evaluate(() => document.getElementById('btnRefreshView').click())
await page.waitForFunction(() => window.__norma && window.__norma.cy.nodes().length > 0)
await page.waitForTimeout(800)
await idle('layoutStatus')
if (groups.length) console.log('group ticks changed:', await tickOnly('data-group', groups))
if (channels.length) console.log('channel ticks changed:', await tickOnly('data-type', channels))
await page.waitForTimeout(500)
await setValue('#localGroupLayout', local)
await setValue('#strategyAlgorithm', algorithm)
await setValue('#groupForce', force)
const shown = await page.evaluate(() => ({
  nodes: window.__norma.cy.nodes().filter((n) => !n.hasClass('hidden-group')).length,
  edges: window.__norma.cy.edges(':visible').length,
}))
console.log(`shown: ${shown.nodes} nodes, ${shown.edges} edges`)
// benchmark
await page.click('#tabProfiler')
await setValue('#benchRepeats', repeats)
await page.evaluate(() => document.getElementById('btnBench').click())
await page.waitForFunction(() =>
  /Benchmarked|after/.test(document.getElementById('benchStatus').textContent)
)
console.log((await page.locator('#benchStatus').innerText()).trim())
const rows = await page.$$eval('#benchResults table tr', (trs) =>
  trs.map((tr) => Array.from(tr.querySelectorAll('th,td')).map((c) => c.textContent.trim()))
)
writeFileSync(out, rows.map((r) => r.join('\t')).join('\n') + '\n')
console.log(`wrote ${rows.length - 1} rows to ${out}`)
if (figure && useLayout) {
  const row = page.locator('#benchResults tr', { hasText: useLayout })
  await row.locator('button', { hasText: 'Use' }).click()
  await page.click('#tabNetwork')
  await page.waitForTimeout(800)
  await idle('layoutStatus')
  await page.evaluate(() => document.getElementById('btnFit').click())
  await page.waitForTimeout(500)
  await page.click('#btnExportImage')
  await page.check('input[name="imgFormat"][value="png"]')
  await setValue('#imgScale', '3')
  await page.fill('#imgFileName', 'figure')
  const [download] = await Promise.all([page.waitForEvent('download'), page.click('#btnImgSave')])
  await download.saveAs(figure)
  console.log(`wrote ${figure}`)
}
await browser.close()
