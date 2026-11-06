// Correcting a circle's position (R: mod_karte_server()). While placing, map
// clicks set the new centre; CSS lets them pass through the map's features.
Shiny.addCustomMessageHandler("veg-map-placing", function (m) {
  var el = document.getElementById(m.id);
  if (!el) return;
  el.classList.toggle("veg-placing", m.on);
  // An open popup would hide the spot to click on.
  if (m.on) HTMLWidgets.find("#" + m.id).getMap().closePopup();
});
