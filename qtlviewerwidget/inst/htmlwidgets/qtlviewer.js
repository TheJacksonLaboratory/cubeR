HTMLWidgets.widget({

  name: 'qtlviewer',

  type: 'output',

  factory: function(el, width, height) {

    // TODO: define shared variables for this instance

    return {
      renderValue: function(x) {

        // TODO: code to render the widget, e.g.

        el.innerHTML = x.widget_html;

       // init(x.data_api_url);

      },

      resize: function(width, height) {
/*
          // htmlwidget put all the pass in data in a script data
          // element.  It does not have id, just loop through the find it.
        	var scripts = document.getElementsByTagName("script");
        	for (var i=0; i<scripts.length; i++) {
        		var type = scripts[i].getAttribute("type");
        		if ( type == 'application/json' ) {
        			_xdata = JSON.parse(scripts[i].innerHTML).x
        		}
        	}
        	*/
      }

    };
  }
});
