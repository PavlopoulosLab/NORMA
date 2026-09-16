import { expect, type Page } from '@playwright/test'

export const EXAMPLE = 'norma:string-tp53'

export async function openExample(page: Page, key = EXAMPLE) {
  await page.goto(`/norma.html?example=${key}`)
  await expect(page.locator('#tabNetwork')).toHaveAttribute('aria-selected', 'true')
  await expect.poll(() => nodeCount(page)).toBeGreaterThan(0)
}

export function nodeCount(page: Page) {
  return page.evaluate(() => window.__norma.cy.nodes().length)
}

export function positions(page: Page) {
  return page.evaluate(() =>
    window.__norma.cy.nodes().map((n) => [n.id(), n.position('x'), n.position('y')] as const)
  )
}

// Set a <select> and fire change directly; avoids actionability waits while layouts animate.
export async function selectValue(page: Page, selector: string, value: string) {
  await page.locator(selector).evaluate((el, v) => {
    ;(el as HTMLSelectElement).value = v
    el.dispatchEvent(new Event('change', { bubbles: true }))
  }, value)
}

declare global {
  interface Window {
    __norma: {
      cy: {
        nodes(): { length: number; map<T>(f: (n: { id(): string; position(k: 'x' | 'y'): number }) => T): T[] }
        edges(): { length: number }
      }
      net3d: { active: boolean }
      views: unknown[]
      currentTab: string
    }
  }
}
