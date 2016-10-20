
var getCerts = function(onSuccess, onError)
{
  $.ajax(
    { url: '/certs'
    , success: onSuccess
    , error: onError
    , type: 'GET'
    });
}

var deleteCertsByDomain = function(domain, onSuccess, onError)
{
  $.ajax(
    { url: '/certs/' + encodeURIComponent(domain) + ''
    , success: onSuccess
    , error: onError
    , type: 'DELETE'
    });
}
