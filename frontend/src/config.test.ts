import { expect, test } from 'vitest'

test('NORMA_CFG merges window.NORMA_CONFIG over the defaults', async () => {
  ;(window as unknown as { NORMA_CONFIG: unknown }).NORMA_CONFIG = {
    site: { name: 'Test NORMA' },
    app: { maxNodes: 10 },
    features: { relays: { string: false } },
  }
  const { NORMA_CFG } = await import('./config')
  expect(NORMA_CFG.site.name).toBe('Test NORMA')
  expect(NORMA_CFG.site.contactName).toBe('Pavlopoulos Lab')
  expect(NORMA_CFG.app.maxNodes).toBe(10)
  expect(NORMA_CFG.app.theme).toBe('white')
  expect(NORMA_CFG.features.relays).toEqual({ string: false, arena3d: true, databases: true })
  expect((NORMA_CFG as { fromServer?: boolean }).fromServer).toBe(true)
})
