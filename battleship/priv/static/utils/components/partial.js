customElements.define(
  "partial-component",
  class extends HTMLElement {
    static observedAttributes = ["data-url"];

    updateListener = () => this.getContent();

    connectedCallback() {
      window.EventBus.addEventListener(
        `partial:${this.getAttribute("id")}`,
        this.updateListener,
      );
    }

    attributeChangedCallback() {
      this.getContent();
    }

    disconnectedCallback() {
      window.EventBus.removeEventListener(
        `partial:${this.getAttribute("id")}`,
        this.updateListener,
      );
    }

    async getContent() {
      const { url } = this.dataset;
      if (!url) {
        throw new Error(`
        "url" data-attribute not supplied to partial component.
        Example: <partial-component data-url="/_foo"></partial-component>
      `);
      }
      this.innerHTML = await (await fetch(url)).text();
    }
  },
);
