// Photo viewer of the circle details (R: photo_viewer()). The photo is
// rotated in 90 degree steps with a CSS transform; the stage takes the
// aspect ratio of the rotated photo, so nothing overlaps or gets cut off.
// Rotations are remembered per photo while the page is open.
window.vegViewer = (function () {
  var turns = {};

  function fit(img) {
    var w = img.naturalWidth, h = img.naturalHeight;
    if (!w || !h) return;
    var k = (((turns[img.dataset.key] || 0) % 4) + 4) % 4;
    var sideways = k % 2 === 1;
    // Width / height of the photo as shown.
    var ratio = sideways ? h / w : w / h;
    img.parentElement.style.setProperty("--ratio", ratio);
    // Before rotating, a sideways photo is as wide as the stage is high and
    // as high as the stage is wide (percentages of the stage's size).
    img.style.width = sideways ? 100 / ratio + "%" : "100%";
    img.style.height = sideways ? 100 * ratio + "%" : "100%";
    img.style.transform = "translate(-50%, -50%) rotate(" + 90 * k + "deg)";
  }

  function rotate(el, step) {
    var img = el.closest(".veg-viewer").querySelector(".veg-viewer-img");
    turns[img.dataset.key] = (turns[img.dataset.key] || 0) + step;
    fit(img);
  }

  function show(thumb) {
    var root = thumb.closest(".veg-viewer");
    var img = root.querySelector(".veg-viewer-img");
    root.querySelectorAll(".veg-viewer-thumbs img").forEach(function (t) {
      t.classList.toggle("active", t === thumb);
    });
    img.dataset.key = thumb.dataset.key;
    img.alt = thumb.dataset.caption;
    img.src = thumb.dataset.src; // fit() runs when it has loaded
    root.querySelector(".veg-viewer-caption").textContent = thumb.dataset.caption;
    root.querySelector(".veg-viewer-original").href = thumb.dataset.href;
  }

  return { fit: fit, rotate: rotate, show: show };
})();
