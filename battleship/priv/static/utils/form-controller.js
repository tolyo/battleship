// Create an empty input event
const inputEvent = new Event("input", {
  bubbles: true,
  cancelable: true,
});

/**
 * Default controller for working with forms. Initiated by default for all forms
 * on document load.
 */
export default class FormController {
  /**
   * @param {HTMLFormElement} form - The HTML form element to be controlled.
   */
  constructor(form) {
    /**
     * @type {string} - ID of this form controller
     */
    this.id = `${Date.now()}-${Math.floor(Math.random() * 1000000)}`;

    /**
     * @type {HTMLFormElement} - The HTML form element being controlled.
     */
    this.form = form;

    this.reactive = false;

    if (!this.form) {
      throw new Error("Form required");
    }

    // Prevent the form from submitting and call the submit() method instead
    this.form.addEventListener("submit", (ev) => {
      ev.preventDefault();
      this.submit();
    });

    const { onchange } = this.form.dataset;
    if (onchange) {
      this.reactive = true;
    }

    /**
     * @type {Object.<string, string>} - Data model to store form field values.
     */
    this.dataModel = {};

    // Loop through form elements to handle input and select fields
    Array.from(this.form.elements).forEach((i) => {
      // Input handler
      if (i instanceof HTMLInputElement) {
        if (i.name === "_csrf_token") {
          this.csrf = i.value;
        }
        i.addEventListener("input", () => {
          this.dataModel[i.name] = i.value;
          // Clear all error messages
          this.clearAllMessages();
          if (this.reactive) {
            this.submit();
          }
        });
      }

      // Select handler
      if (i instanceof HTMLSelectElement) {
        this.dataModel[i.name] = i.value; // Set initial value
        i.addEventListener("change", () => {
          this.dataModel[i.name] = i.value;
          // Clear all error messages
          this.clearAllMessages();
          if (this.reactive) {
            this.submit();
          }
        });
      }
    });
  }

  /**
   * Clear all error messages
   */
  clearAllMessages() {
    // Remove invalid states from inputs
    Array.from(this.form.elements).forEach((e) =>
      e.removeAttribute("aria-invalid"),
    );
    // Remove spans with error messages
    this.form.querySelectorAll("span.error").forEach((x) => x.remove());
    // Remove error class from labels
    Array.from(this.form.getElementsByClassName("error")).forEach((x) =>
      x.classList.remove("error"),
    );
  }

  /**
   * Submit the form data
   */
  submit() {
    this.clearAllMessages();
    const { action, method } = this.form.dataset;
    Array.from(this.form.elements).forEach((i) => i.dispatchEvent(inputEvent));
    fetch(action, this.maybeAttachBody(method || "POST")).then((res) => {
      if (res.status < 300 && res.status >= 200) {
        this.onSuccess(res);
      } else {
        this.onError(res);
      }
    });
  }

  /**
   * Success callback
   * @param {Response} res - response data to pass to data-success callback
   */
  async onSuccess(res) {
    if (res.status === 204 || res.headers.get("content-type") === "text/html") {
      // eslint-disable-next-line no-eval
      eval(`${this.form.dataset.success}`);
    } else {
      const data = await res.json();
      if (data) {
        // eslint-disable-next-line no-eval
        eval(`(function() { return ${this.form.dataset.success} })()`)(data);
      } else {
        throw Error("Unknown direction");
      }
    }

    const { update } = this.form.dataset;
    if (update) {
      const partial = document.querySelector(update);
      if (partial) {
        partial.innerHTML = await res.text();
      } else {
        throw new Error("Partial not found");
      }
    }
  }

  /**
   * Error callback
   * TODO: switch to Validation API!
   * @param {Response} res - The response object.
   */
  onError(res) {
    this.res = res;
    this.clearAllMessages();
    this.res.json().then((err) => {
      Object.keys(err).forEach((x) => {
        const input = this.form.elements[x];
        input.setAttribute("aria-invalid", "true");
        const helper = document.createElement("span");
        helper.classList.add("error");
        helper.innerHTML = err[x];
        input.after(helper);
        input.labels?.[0]?.classList.add("error");
      });
    });
  }

  /**
   * Determine whether to attach a request body and headers
   * based on the form's method.
   * @param { string } method
   */
  maybeAttachBody(method) {
    if (["DELETE", "GET"].includes(this.form.method)) {
      return {};
    }
    return {
      method,
      body: JSON.stringify(this.dataModel),
      headers: {
        "Content-Type": "application/json",
        "x-csrf-token": this.csrf,
      },
    };
  }

  /**
   * Destroy the FormController instance.
   */
  destroy() {
    this.form.removeEventListener("submit", (ev) => {
      ev.preventDefault();
      this.submit();
    });

    Array.from(this.form.elements).forEach((element) => {
      if (element instanceof HTMLInputElement) {
        element.removeEventListener("input", () => {
          this.dataModel[element.name] = element.value;
          this.clearAllMessages();
        });
      } else if (element instanceof HTMLSelectElement) {
        element.removeEventListener("change", () => {
          this.dataModel[element.name] = element.value;
          this.clearAllMessages();
        });
      }
    });

    this.form = null;
  }
}
