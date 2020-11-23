function log(s) {
  console.log(s)
}

var _handler_id = 0;
var _handler_callback = {};
function downloadData2(URL, description, callback) {
    _handler_id++

    logInfo('Downloading: ', URL);
    $.ajax({
        url: URL,
        method: 'GET',
    }).done(function(data, textStatus, jqXHR) {
        logInfo(`Download of ${description}`);
        logDebug(`Download of ${description}:`, data);
        callback(null, data);
    }).fail(function(jqXHR, textStatus, errorThrown) {
        logError(description, textStatus);
        logError(description, textStatus, errorThrown);
        callback(errorThrown, null);
    });
}

var _group_id = 0;
var _group_data = {};
function newCallGroup(urls) {
    _group_id++
    _group_data[_group_id +''] = {"status": "RUNNING",  "urls": urls}
    return _group_id;
}

function updateGroupResponse(group_id, error, responses) {
   var group = _group_data[group_id +'']
   group.status = 'DONE'
   if ( error ) {
       group.error = error;
   }
   for (var i=0; i<group.urls.length; i++) {
       var url = group.urls[i];
       url.response = responses[i];
   }
}

function getCallGroupResponse(group_id) {
    var group = _group_data[group_id +''];
    var r = {};
    r.status = group.status;
    r.number_tasks_submitted = group.urls.length;

    var completed = 0;
    for (var i=0; i<group.urls.length; i++) {
       var url = group.urls[i];
       if ( url.response !== null ) {
          completed++;
       }
    }
    r.number_tasks_completed = completed;

    if ( group.error ) {
        r.number_tasks_errors = 1;
    } else {
        r.number_tasks_errors = 0;
    }

    r.response_data = [];
    for (var i=0; i<group.urls.length; i++) {
       var url = group.urls[i];

       var response_data = {};
       response_data.from_cache = false;
       response_data.response = url.response;
       r.response_data[url.url_id] = response_data;
    }

    return r;
}
