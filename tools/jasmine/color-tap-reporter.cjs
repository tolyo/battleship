const COLOR = {
  green: '\u001b[32m',
  red: '\u001b[31m',
  yellow: '\u001b[33m',
  cyan: '\u001b[36m',
  dim: '\u001b[2m',
  reset: '\u001b[0m',
};

class ColorTapReporter {
  constructor() {
    this.executed = 0;
    this.failed = 0;
    this.skipped = 0;
    this.disabled = 0;
    this.defined = 0;
    this.startedAt = 0;
  }

  jasmineStarted(summary) {
    this.defined = summary?.totalSpecsDefined ?? 0;
    this.startedAt = Date.now();
    this.log(`${COLOR.cyan}TAP version 13${COLOR.reset}`);
  }

  specStarted() {
    this.executed += 1;
  }

  specDone(spec) {
    if (spec.status === 'failed') {
      this.failed += 1;
      this.log(
        `${COLOR.red}not ok ${this.executed}${COLOR.reset} - ${spec.fullName}`
      );
      this.logFailures(spec.failedExpectations);
      return;
    }

    if (spec.status === 'pending') {
      this.skipped += 1;
      this.log(
        `${COLOR.yellow}ok ${this.executed}${COLOR.reset} - ${spec.fullName} # SKIP`
      );
      return;
    }

    if (spec.status === 'disabled') {
      this.disabled += 1;
      this.log(
        `${COLOR.yellow}ok ${this.executed}${COLOR.reset} - ${spec.fullName} # SKIP`
      );
      return;
    }

    this.log(`${COLOR.green}ok ${this.executed}${COLOR.reset} - ${spec.fullName}`);
  }

  jasmineDone() {
    const elapsed = ((Date.now() - this.startedAt) / 1000).toFixed(3);
    const disabled = this.defined - this.executed + this.disabled;
    this.log(`${COLOR.cyan}1..${this.executed}${COLOR.reset}`);
    this.log(
      `${COLOR.dim}# ${this.defined} specs, ${this.failed} failures, ${this.skipped} skipped, ${disabled} disabled in ${elapsed}s.${COLOR.reset}`
    );
  }

  /**
   * @param {Array<{ message?: string, stack?: string }>} failures
   */
  logFailures(failures) {
    failures.forEach((failure) => {
      if (failure.message) {
        this.log(`${COLOR.red}  # Failure: ${comment(failure.message)}${COLOR.reset}`);
      }

      if (failure.stack && failure.stack !== failure.message) {
        this.log(`${COLOR.dim}  # ${comment(failure.stack)}${COLOR.reset}`);
      }
    });
  }

  /**
   * @param {string} text
   */
  log(text) {
    console.log(text);
  }
}

/**
 * @param {string} text
 * @returns {string}
 */
function comment(text) {
  return text.trim().replace(/\n/g, '\n  # ');
}

module.exports = ColorTapReporter;
