function selectPhenotypeCall(urls) {
	log(urls)
	for (var i=0; i<urls.length; i++) {
		var url = urls[i].url;
		url = 'static/data/' + urls[i].url_id + ".json";
		log(url);
	    $.ajax({
		    type: 'GET',
		    url: url,
		    success: function (data, status, request) {
		    	log(data)
		    }
		});
	}
}



function log(s) {
	console.log(s)
}