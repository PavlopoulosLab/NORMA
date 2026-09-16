import tseslint from 'typescript-eslint'

// ponytail: the former single-file script (src/app.ts, later its split modules) is
// untyped (`@ts-nocheck`), so the type-aware rules only report `any` noise there.
// Type-checked rules apply to every typed module; drop a file from LEGACY as it gets typed.
const LEGACY = ['src/app.ts']

export default tseslint.config(
  ...tseslint.configs.recommendedTypeChecked,
  {
    languageOptions: {
      parserOptions: {
        project: true,
        tsconfigRootDir: import.meta.dirname,
      },
    },
  },
  {
    files: LEGACY,
    ...tseslint.configs.disableTypeChecked,
  },
  {
    files: LEGACY,
    rules: {
      '@typescript-eslint/ban-ts-comment': 'off',
      '@typescript-eslint/no-unused-vars': 'warn',
      '@typescript-eslint/no-unused-expressions': 'off', // `a && a()` idioms
      'no-var': 'off', // hoisted on purpose in the legacy script
    },
  },
  {
    ignores: ['dist/**'],
  }
)
