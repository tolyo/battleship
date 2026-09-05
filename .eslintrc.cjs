module.exports = {
  env: {
    browser: true,
    es2021: true,
  },
  extends: ['airbnb-base', 'prettier'],
  parserOptions: {
    ecmaVersion: 'latest',
    sourceType: 'module',
  },
  rules: {
    'import/extensions': 0,
    'no-use-before-define': [
      'error',
      {
        functions: false,
        classes: true,
        variables: true,
        allowNamedExports: false,
      },
    ],
    'class-methods-use-this': 'off',
    'import/prefer-default-export': 'off',
    'no-param-reassign': 'off',
    'no-underscore-dangle': [
      'error',
      {
        allow: ['_checkListenersForAllKeys', '_flushScheduledTasks'],
      },
    ],
  },
  overrides: [
    {
      files: ['assets/**/*.test.js'],
      env: {
        jasmine: true,
        browser: false,
        node: true,
      },
    },
  ],
};
