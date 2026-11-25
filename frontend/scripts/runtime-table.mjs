// Runs the Network Profiler's Runtime table in headless Chromium and writes
// it as tab-separated text, for a supplementary table that anyone can
// regenerate:  node scripts/runtime-table.mjs [--max 10000] [--grid] [--out file.tsv]
// The page must be served first (python3 ../backend/server.py --port 8765).
import { chromium } from '@playwright/test'
import { writeFileSync } from 'node:fs'

const arg = (name, dflt) => {
  const i = process.argv.indexOf(name)
  return i > 0 ? process.argv[i + 1] : dflt
}
const url = arg('--url', 'http://127.0.0.1:8765/')
const max = arg('--max', '10000')
const grid = process.argv.includes('--grid')
const out = arg('--out', 'runtime-table.tsv')

const browser = await chromium.launch({
  args: ['--enable-precise-memory-info', '--js-flags=--expose-gc'],
})
const page = await browser.newPage()
page.setDefaultTimeout(0)
await page.goto(url)
await page.click('#tabProfiler')
await page.selectOption('#runtimeMax', max)
if (grid) await page.check('#runtimeGrid')
const t0 = Date.now()
await page.click('#btnRuntime')
let last = ''
for (;;) {
  await page.waitForTimeout(5000)
  const status = await page.locator('#runtimeStatus').innerText()
  const btn = await page.locator('#btnRuntime').innerText()
  if (status !== last) {
    console.log(`${Math.round((Date.now() - t0) / 1000)}s  ${status.replace(/\s+/g, ' ').trim()}`)
    last = status
  }
  if (btn.trim() === 'Time the main steps' && status.includes('Done')) break
  if (status.includes('Stopped')) break
}
const rows = await page.$$eval('#runtimeResults table tr', (trs) =>
  trs.map((tr) => Array.from(tr.querySelectorAll('th,td')).map((c) => c.textContent.trim()))
)
const info = await page.evaluate(() => ({
  ua: navigator.userAgent,
  cores: navigator.hardwareConcurrency,
  memory: !!performance.memory,
}))
const tsv =
  rows.map((r) => r.join('\t')).join('\n') + `\n# ${info.ua}; ${info.cores} logical processors\n`
writeFileSync(out, tsv)
console.log(`wrote ${rows.length - 1} rows to ${out} (memory reported: ${info.memory})`)
await browser.close()
