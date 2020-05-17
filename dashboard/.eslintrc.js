module.exports = {
  env: {
    browser: true,
    es6: true
  },
  extends: "eslint:recommended",
  globals: {
    Atomics: "readonly",
    SharedArrayBuffer: "readonly"
  },
  parserOptions: {
    ecmaVersion: 11,
    sourceType: "module"
  },
  rules: {
    "prefer-const": 2,
    "no-var": 2,
    complexity: 0,
    "consistent-return": 1,
    camelcase: 1,
    complexity: ["error", { max: 5 }]
  }
};
