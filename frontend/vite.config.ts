import { defineConfig } from 'vitest/config'
import { viteSingleFile } from 'vite-plugin-singlefile'

const backend = process.env.VITE_PROXY_TARGET ?? 'http://localhost:8000'

// The page is norma.html (so ?example=, ?session= links keep working) and the
// build inlines everything into dist/norma.html, which opens from disk too.
export default defineConfig({
  base: './',
  plugins: [viteSingleFile()],
  build: { rollupOptions: { input: 'norma.html' } },
  server: {
    // norma_api_client.py (the API template) is imported ?raw from the repo root
    fs: { allow: ['..'] },
    proxy: Object.fromEntries(
      ['/api', '/string-api', '/db-api', '/arena3d-api', '/norma-config.js'].map((p) => [
        p,
        backend,
      ])
    ),
  },
  test: {
    environment: 'jsdom',
    setupFiles: ['./vitest.setup.ts'],
    exclude: ['e2e/**', 'node_modules/**'],
  },
})
