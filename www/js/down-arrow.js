$(function() {
  $(document).on("click", ".down-arrow", function(d) {
    let $arrow = $(d.target);
    let href = $arrow.closest("a").attr("href");
    let top = $(href).offset().top;
    $([document.documentElement, document.body]).animate({scrollTop: top});
  });
});


