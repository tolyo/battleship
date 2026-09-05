import { readdir, readFile, writeFile } from 'node:fs/promises';
import { fileURLToPath } from 'node:url';

import { format } from 'sql-formatter';

const MAX_LINE_LENGTH = 100;
const projectDirectory = fileURLToPath(new URL('../', import.meta.url));
const sqlDirectory = new URL('../sql/', import.meta.url);
const config = JSON.parse(
  await readFile(new URL('../.sql-formatter.json', import.meta.url), 'utf8')
);
const check = process.argv.includes('--check');
const files = (await readdir(sqlDirectory))
  .filter((name) => name.endsWith('.sql'))
  .sort();
const unformatted = [];

function formatPostgresFunctionDeclarations(sql) {
  return sql.replace(
    /^(CREATE(?: OR REPLACE)? FUNCTION\s+[^\s(]+)\s*\(([^)]*)\)\s+(.+)$/gmu,
    (_match, declaration, parameterSource, clauseSource) => {
      const parameters = parameterSource
        .split(',')
        .map((parameter) => parameter.trim())
        .filter(Boolean);
      const declarationLines =
        parameters.length === 0
          ? [`${declaration} ()`]
          : [
              `${declaration} (`,
              ...parameters.map(
                (parameter, index) =>
                  `    ${parameter}${index < parameters.length - 1 ? ',' : ''}`
              ),
              ')',
            ];
      const clauseLines = clauseSource
        .replace(/\s+(?=LANGUAGE\b)/gu, '\n')
        .replace(/\s+(?=AS\s+\$[A-Za-z0-9_]*\$$)/gu, '\n')
        .split('\n');

      return [...declarationLines, ...clauseLines].join('\n');
    }
  );
}

function formatSql(source) {
  return `${formatPostgresFunctionDeclarations(format(source, config))
    .split('\n')
    .map((line) => line.trimEnd())
    .join('\n')
    .trimEnd()}\n`;
}

for (const name of files) {
  const url = new URL(name, sqlDirectory);
  const source = await readFile(url, 'utf8');
  const formatted = formatSql(source);
  const longLines = formatted
    .split('\n')
    .map((line, index) => ({ length: line.length, number: index + 1 }))
    .filter((line) => line.length > MAX_LINE_LENGTH);

  if (longLines.length > 0) {
    const locations = longLines
      .map((line) => `${name}:${line.number} (${line.length} columns)`)
      .join(', ');
    throw new Error(
      `SQL lines exceed ${MAX_LINE_LENGTH} columns: ${locations}`
    );
  }
  if (source === formatted) {
    continue;
  }
  if (check) {
    unformatted.push(name);
  } else {
    await writeFile(url, formatted);
  }
}

if (unformatted.length > 0) {
  throw new Error(
    `SQL formatting differs in ${unformatted.join(', ')}; run npm run format:sql`
  );
}

console.log(
  `${check ? 'Checked' : 'Formatted'} ${files.length} PostgreSQL migration(s) in ${projectDirectory}`
);
