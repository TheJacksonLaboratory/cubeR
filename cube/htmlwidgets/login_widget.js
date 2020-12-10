HTMLWidgets.widget({

  name: 'login_widget',

  type: 'output',

  factory: function(el, width, height) {

    // TODO: define shared variables for this instance

    return {
      renderValue: function(x) {
        var t = '<br/>';
        t += '<a style="margin-left:20px;" ';
        t += 'href="' + x.validation_url + '">'
        t += '<button style="font-size:130%;padding:8px;" type="button">Login to Cube API</button>';
        t += '</a>'

        el.innerHTML = t;
      },

      resize: function(width, height) {
      }

    };
  }
});
