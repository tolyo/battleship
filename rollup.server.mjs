import resolve from '@rollup/plugin-node-resolve';
import commonjs from '@rollup/plugin-commonjs';
// import terser from "@rollup/plugin-terser";

const plugins = [
  resolve(), // tells Rollup how to find date-fns in node_modules
  commonjs(), // converts date-fns to ES modules
];

const defaultDir = 'priv/static';

export default [
  {
    input: 'app/index.js',
    output: {
      dir: defaultDir,
      format: 'umd',
    },
    plugins: plugins,
  },
];
