$(document).on('shiny:bound', function(event) {
  console.log('Shiny bound: initializing tooltips');
  $('.info_icon').tooltip({
	placement: 'left',
	trigger: 'hover',
	delay: { 'show': 0, 'hide': 3000 },
	title: function() {
	  return $(this).attr('data-tooltip');
	}
  });
});

$(document).on("shiny:connected", function(e) {
	  Shiny.addCustomMessageHandler("openNewWindow", function(message) {
		window.open(message.url, "_blank");
	  });
});	
	
$(document).on("input", "textarea", function() {
	$(this).height(0);
	$(this).height(this.scrollHeight);
});