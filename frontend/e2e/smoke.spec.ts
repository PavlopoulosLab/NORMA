import { expect, test } from '@playwright/test'
import { EXAMPLE, nodeCount, openExample, positions, selectValue } from './helpers'

test('welcome page loads with examples and site info', async ({ page }) => {
  await page.goto('/')
  await expect(page).toHaveTitle(/NORMA/)
  await expect(page.locator('#tabWelcome')).toHaveAttribute('aria-selected', 'true')
  expect(await page.locator('#sampleSelect option').count()).toBeGreaterThan(3)
  await expect(page.locator('#welcomeView')).toContainText('MIT License')
})

test('example opens with nodes, edges and groups', async ({ page }) => {
  await openExample(page)
  const nodes = await nodeCount(page)
  const edges = await page.evaluate(() => window.__norma.cy.edges().length)
  expect(edges).toBeGreaterThan(0)
  await expect(page.locator('#statNodes')).toHaveText(nodes.toLocaleString('en-US'))
  await expect(page.locator('#statGroups')).not.toHaveText('0')
})

test('every built-in example loads', async ({ page }) => {
  await page.goto('/')
  const keys = await page.locator('#sampleSelect option').evaluateAll((o) => o.map((e) => (e as HTMLOptionElement).value))
  for (const key of keys.filter((k) => k.startsWith('norma:'))) {
    await openExample(page, key)
  }
})

test('layouts move the nodes', async ({ page }) => {
  await openExample(page)
  const before = await positions(page)
  await page.locator('#sideTabDisplay').click()
  await page.locator('#layoutSection h3').click()
  await expect(page.locator('#btnRunLayout')).toBeVisible()
  await selectValue(page, '#layoutSelect', 'circle')
  await page.locator('#btnRunLayout').click()
  await expect.poll(async () => JSON.stringify(await positions(page)) !== JSON.stringify(before)).toBe(true)
  const circle = await positions(page)
  await selectValue(page, '#layoutSelect', 'grid')
  await page.locator('#btnRunLayout').click()
  await expect.poll(async () => JSON.stringify(await positions(page)) !== JSON.stringify(circle)).toBe(true)
})

test('tabs switch views, 3D view activates', async ({ page }) => {
  await openExample(page)
  for (const [tab, view] of [
    ['tabProfiler', 'profilerView'],
    ['tabCompare', 'compareView'],
    ['tabApi', 'apiView'],
    ['tabHelp', 'helpView'],
    ['tabAbout', 'aboutView'],
  ]) {
    await page.locator(`#${tab}`).click()
    await expect(page.locator(`#${view}`)).toBeVisible()
  }
  await page.locator('#tabNetwork3d').click()
  await expect.poll(() => page.evaluate(() => window.__norma.net3d.active)).toBe(true)
  await expect(page.locator('#cy3d')).toBeVisible()
  await page.locator('#tabNetwork').click()
  await expect.poll(() => page.evaluate(() => window.__norma.net3d.active)).toBe(false)
})

for (const format of ['png', 'svg']) {
  test(`image export downloads a ${format}`, async ({ page }) => {
    await openExample(page)
    await page.locator('#btnExportImage').click()
    await expect(page.locator('#exportDialog')).toBeVisible()
    await page.locator(`input[name="imgFormat"][value="${format}"]`).check()
    const download = page.waitForEvent('download')
    await page.locator('#btnImgSave').click()
    const file = await download
    expect(file.suggestedFilename()).toMatch(new RegExp(`\\.${format}$`))
    expect(await file.failure()).toBeNull()
  })
}

test('profiler computes statistics and the layout benchmark', async ({ page }) => {
  await openExample(page)
  await page.locator('#tabProfiler').click()
  await expect(page.locator('#profNetList input')).not.toHaveCount(0)
  await page.locator('#btnProfile').click()
  await expect(page.locator('#profResults')).not.toBeEmpty({ timeout: 45_000 })
  await selectValue(page, '#benchRepeats', '1')
  await page.locator('#btnBench').click()
  await expect(page.locator('#benchResults')).not.toBeEmpty({ timeout: 45_000 })
})

test('API tab: server round trip and json link both open the payload', async ({ page }) => {
  await page.goto('/norma.html')
  await page.locator('#tabApi').click()
  await expect(page.locator('#apiPayload')).not.toBeEmpty()
  await page.locator('#btnApiServer').click()
  await expect(page.locator('#apiResultUrl')).toHaveValue(/norma\.html\?session=/)
  const url = await page.locator('#apiResultUrl').inputValue()
  await page.goto(url)
  await expect.poll(() => nodeCount(page)).toBeGreaterThan(0)

  await page.goto('/norma.html')
  await page.locator('#tabApi').click()
  await page.locator('#btnApiLink').click()
  await expect(page.locator('#apiResultUrl')).toHaveValue(/#json=/)
  const link = await page.locator('#apiResultUrl').inputValue()
  await page.goto('about:blank') // a hash-only change would not reload the page
  await page.goto(link)
  await expect.poll(() => nodeCount(page)).toBeGreaterThan(0)
})

test('REST API payload via curl-style POST opens in the page', async ({ page, request }) => {
  const r = await request.post('/api/external', {
    data: { name: 'Demo', edges: [{ source: 'A', target: 'B' }, { source: 'B', target: 'C' }], groups: { G1: ['A', 'B'] } },
  })
  expect(r.ok()).toBe(true)
  const { url } = await r.json()
  await page.goto(url)
  await expect.poll(() => nodeCount(page)).toBe(3)
  await expect(page.locator('#statGroups')).toHaveText('1')
})

test('unknown example shows an error, not a crash', async ({ page }) => {
  await page.goto('/norma.html?example=nope')
  await expect(page.locator('body')).toContainText(/no example called/i)
  expect(await nodeCount(page)).toBe(0)
  void EXAMPLE
})
