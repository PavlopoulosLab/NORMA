import tseslint from 'typescript-eslint'

// ponytail: the modules split from the former single-file script are untyped
// (`@ts-nocheck`), so the type-aware rules only report `any` noise there. Type-checked
// rules apply to every typed module; add a file to TYPED as its @ts-nocheck is removed.
const LEGACY = ['src/**/*.ts']
// tests import the untyped modules, so they inherit the LEGACY rules for now
const TYPED = ['src/state.ts']

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
    ignores: TYPED,
    ...tseslint.configs.disableTypeChecked,
  },
  {
    files: LEGACY,
    ignores: TYPED,
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
