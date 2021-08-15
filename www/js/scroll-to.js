Shiny.addCustomMessageHandler("scroll-to", function(msg) {
  let top = $(msg.selector).offset().top;
  $([document.documentElement, document.body]).animate({scrollTop: top});
})
