import { spawn } from 'node:child_process';
import { copyFile, mkdir, mkdtemp, rm, writeFile } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

// Compile the real stylesheet and handwritten CSS through the managed browser
// pipeline in a temporary CSS-only project. Backend routes are independent of
// this check, and the application's existing build directory stays untouched.
const project = fileURLToPath(new URL('../', import.meta.url));
const compiler =
  process.env.TERLC ??
  path.resolve(project, '../terlan/terlan/target/debug/terlc');
const temporary = await mkdtemp(path.join(tmpdir(), 'battleship-css-'));
try {
  const source = path.join(temporary, 'src/battleship');
  await mkdir(source, { recursive: true });
  await mkdir(path.join(temporary, 'assets'));
  for (const name of ['Styles.terl', 'StylesTest.terl']) {
    await copyFile(path.join(project, 'src/battleship', name), path.join(source, name));
  }
  await writeFile(
    path.join(temporary, 'assets/index.html'),
    '<!doctype html><html><body></body></html>\n'
  );
  await writeFile(
    path.join(temporary, 'terlan.toml'),
    `
[package]
name = "battleship"
version = "0.0.1"

[build]
source_roots = ["src"]

[web.assets]
directory = "assets"
styles = [${JSON.stringify(path.join(project, 'assets/styles.css'))}, "src/battleship/Styles.terl"]
`
  );
  await run(compiler, ['test', path.join(source, 'StylesTest.terl'), '--target', 'terlan-vm']);
  await run(compiler, [
    'build',
    temporary,
    '--target',
    'js.browser',
    '--out-dir',
    path.join(temporary, 'build'),
  ]);
  await run(
    process.execPath,
    [path.join(project, 'tools/css-style-check.mjs')],
    {
      ...process.env,
      TERLAN_WEB_DIR: path.join(temporary, 'build/web'),
    }
  );
} finally {
  await rm(temporary, { recursive: true, force: true });
}

async function run(command, args, env = process.env) {
  await new Promise((resolve, reject) => {
    const child = spawn(command, args, { cwd: project, env, stdio: 'inherit' });
    child.once('error', reject);
    child.once('exit', (code, signal) => {
      if (code === 0) resolve();
      else reject(new Error(`${command} failed: ${signal ?? code}`));
    });
  });
}
