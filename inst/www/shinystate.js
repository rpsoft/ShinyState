/* ShinyState client runtime: a custom Shiny output binding that patches the
 * DOM in place (like a virtual-DOM reconciler) instead of replacing it
 * wholesale. This lets keyboard focus, cursor position, text selection, and
 * scroll survive re-renders — the React/Vue guarantee — with no server-side
 * partitioning. Self-contained: no external dependencies. */
(function () {
  "use strict";

  if (window.__shinystate_runtime) return;
  window.__shinystate_runtime = true;

  var ELEMENT = 1, TEXT = 3, COMMENT = 8;

  // Two nodes are "the same slot" if they can be morphed into one another.
  // Keyed by id + tag when an id is present, otherwise by node type + tag.
  function sameSlot(a, b) {
    if (a.nodeType !== b.nodeType) return false;
    if (a.nodeType === ELEMENT) {
      if (a.tagName !== b.tagName) return false;
      if (a.id || b.id) return a.id === b.id;
      return true;
    }
    return true;
  }

  // The user is actively interacting with this element — never overwrite it.
  function shouldPreserve(el) {
    if (el.nodeType !== ELEMENT) return false;
    if (el.hasAttribute("data-shinystate-preserve")) return true;
    if (el === document.activeElement &&
        /^(INPUT|TEXTAREA|SELECT)$/.test(el.tagName)) {
      return true;
    }
    return false;
  }

  function morphAttrs(from, to) {
    var i, a, toAttrs = to.attributes, fromAttrs = from.attributes;
    for (i = 0; i < toAttrs.length; i++) {
      a = toAttrs[i];
      if (from.getAttribute(a.name) !== a.value) from.setAttribute(a.name, a.value);
    }
    for (i = fromAttrs.length - 1; i >= 0; i--) {
      a = fromAttrs[i];
      if (!to.hasAttribute(a.name)) from.removeAttribute(a.name);
    }
  }

  function morphNode(from, to) {
    if (from.isEqualNode(to)) return;
    if (from.nodeType === TEXT || from.nodeType === COMMENT) {
      if (from.nodeValue !== to.nodeValue) from.nodeValue = to.nodeValue;
      return;
    }
    if (from.nodeType === ELEMENT) {
      if (shouldPreserve(from)) return; // leave the focused/opted-out node untouched
      morphAttrs(from, to);
      morphChildren(from, to);
    }
  }

  // Positional reconcile with id-keyed matching for reordered children.
  function morphChildren(fromParent, toParent) {
    var fromChildren = Array.prototype.slice.call(fromParent.childNodes);
    var toChildren = Array.prototype.slice.call(toParent.childNodes);
    var fromIdx = 0, i, toChild, fromChild, keyed, j;

    for (i = 0; i < toChildren.length; i++) {
      toChild = toChildren[i];
      fromChild = fromChildren[fromIdx];

      if (!fromChild) {
        fromParent.appendChild(toChild.cloneNode(true));
        continue;
      }

      if (sameSlot(fromChild, toChild)) {
        morphNode(fromChild, toChild);
        fromIdx++;
        continue;
      }

      // Look ahead for an id-keyed match that moved.
      keyed = null;
      if (toChild.nodeType === ELEMENT && toChild.id) {
        for (j = fromIdx + 1; j < fromChildren.length; j++) {
          if (fromChildren[j].nodeType === ELEMENT && fromChildren[j].id === toChild.id) {
            keyed = fromChildren[j];
            break;
          }
        }
      }
      if (keyed) {
        fromParent.insertBefore(keyed, fromChild);
        morphNode(keyed, toChild);
        fromChildren.splice(j, 1);
      } else {
        fromParent.insertBefore(toChild.cloneNode(true), fromChild);
      }
    }

    // Remove any old children the new tree no longer has.
    for (i = fromChildren.length - 1; i >= fromIdx; i--) {
      if (fromChildren[i] && fromChildren[i].parentNode === fromParent) {
        fromParent.removeChild(fromChildren[i]);
      }
    }
  }

  function patch(el, html) {
    var tpl = document.createElement("div");
    tpl.innerHTML = html == null ? "" : html;
    morphChildren(el, tpl);
  }

  function register() {
    if (!window.Shiny || !Shiny.OutputBinding || window.__shinystate_bound) return;
    window.__shinystate_bound = true;

    var binding = new Shiny.OutputBinding();
    var jq = window.jQuery || window.$;

    binding.find = function (scope) {
      // Shiny passes `scope` as a jQuery object; match the built-in bindings.
      if (jq) return jq(scope).find(".shinystate-output");
      var root = scope.querySelectorAll ? scope : (scope[0] || document);
      return root.querySelectorAll(".shinystate-output");
    };

    binding.renderValue = function (el, data) {
      if (el && el.jquery) el = el[0]; // unwrap if handed a jQuery object
      var html = "", deps = [];
      if (typeof data === "string") {
        html = data;
      } else if (data) {
        html = data.html || "";
        deps = data.deps || [];
      }
      if (Shiny.renderDependencies) Shiny.renderDependencies(deps);
      Shiny.unbindAll(el);
      patch(el, html);
      return Shiny.bindAll(el);
    };

    binding.renderError = function (el, err) {
      Shiny.unbindAll(el);
      el.innerHTML =
        '<div style="color:#b00020;padding:1em;border:1px solid #b00020;">' +
        '<strong>ShinyState error: </strong>' +
        (err && err.message ? String(err.message) : "render failed") +
        "</div>";
    };

    binding.onValueError = function (el, err) {
      this.renderError(el, err);
    };

    Shiny.outputBindings.register(binding, "shinystate.outputBinding");
  }

  if (window.Shiny) {
    register();
  } else {
    document.addEventListener("shiny:connected", register);
  }
})();
