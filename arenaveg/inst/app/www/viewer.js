// Photo viewer of the circle details (R: photo_viewer()). The photo is
// rotated in 90 degree steps with a CSS transform; the stage takes the
// aspect ratio of the rotated photo, so the whole photo stays visible.
// Rotations are remembered per photo while the page is open.
window.vegViewer = (function () {
  var turns = {};

  // Quarter turns of the photo; a saved rotation counts until it is changed.
  function turnsOf(img) {
    var key = img.dataset.key, root = img.closest(".veg-viewer");
    if (!(key in turns)) turns[key] = root.dataset.savedKey === key ? Number(root.dataset.savedTurns) : 0;
    return turns[key];
  }

  function fit(img) {
    var w = img.naturalWidth, h = img.naturalHeight;
    if (!w || !h) return;
    var k = ((turnsOf(img) % 4) + 4) % 4;
    var sideways = k % 2 === 1;
    // Width / height of the photo as shown.
    var ratio = sideways ? h / w : w / h;
    img.parentElement.style.setProperty("--ratio", ratio);
    // Before rotating, a sideways photo is as wide as the stage is high and
    // as high as the stage is wide (percentages of the stage's size).
    img.style.width = sideways ? 100 / ratio + "%" : "100%";
    img.style.height = sideways ? 100 * ratio + "%" : "100%";
    img.style.transform = "translate(-50%, -50%) rotate(" + 90 * k + "deg)";
    var root = img.closest(".veg-viewer");
    reveal(root);
    if (root.dataset.input && window.Shiny) {
      Shiny.setInputValue(root.dataset.input, { file_uuid: img.dataset.key, rotation: 90 * k });
    }
  }

  // The viewer sits at the bottom of the scrolling detail panel: scroll it,
  // with the buttons above it, to the top of the panel once when it appears,
  // so the whole photo is in view.
  function reveal(root) {
    if (!root || root.dataset.revealed || !root.offsetParent) return;
    root.dataset.revealed = "1";
    (root.closest(".veg-photos") || root).scrollIntoView({ block: "start", behavior: "smooth" });
  }

  function rotate(el, step) {
    var img = el.closest(".veg-viewer").querySelector(".veg-viewer-img");
    turns[img.dataset.key] = turnsOf(img) + step;
    fit(img);
  }

  function show(select) {
    var opt = select.options[select.selectedIndex];
    var root = select.closest(".veg-viewer");
    var img = root.querySelector(".veg-viewer-img");
    img.dataset.key = opt.dataset.key;
    img.alt = opt.text;
    img.src = opt.dataset.src; // fit() runs when it has loaded
    root.querySelector(".veg-viewer-original").href = opt.dataset.href;
  }

  // Opening the tab that holds a viewer brings the photo into view.
  document.addEventListener("shown.bs.tab", function (e) {
    var target = e.target.getAttribute("data-bs-target") || e.target.getAttribute("href");
    var pane = target && target.charAt(0) === "#" ? document.getElementById(target.slice(1)) : null;
    var root = pane && pane.querySelector(".veg-viewer");
    if (root) {
      delete root.dataset.revealed;
      reveal(root);
    }
  });

  return { fit: fit, rotate: rotate, show: show };
})();
