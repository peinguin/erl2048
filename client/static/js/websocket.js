var Websocket = function(wsHost){
  var self = this;

  var websocket,
    callbacks = [];

  this.connect = function(){
    var deferred = vow.defer();

    websocket = new WebSocket(wsHost);
    websocket.onopen = function(evt) {
      deferred.resolve(evt)
    };
    websocket.onclose = function(evt)   { self.onClose(evt) };
    websocket.onmessage = function(evt) { self.onMessage(evt) };
    websocket.onerror = function(evt)   { self.onError(evt) };

    return deferred.promise();
  },
  this.disconnect = function() {
    websocket.close();
  },
  this.toggle_connection = function(){
    if(websocket.readyState == websocket.OPEN){
      disconnect();
    } else {
      connect();
    };
  },
  this.send = function(message) {
    if(typeof message !== 'string'){
      message = JSON.stringify(message);
    }
    if(websocket.readyState == websocket.OPEN){
      websocket.send(message);
    } else {
      alert('websocket is not connected'); 
    };
  },
  this.onClose = function(evt) {
      alert('DISCONNECTED');
  },
  this.onMessage = function(evt) { 
    callbacks.forEach(function(callback){
      callback(evt);
    });
  },
  this.onError = function(evt) {
      alert('ERROR: ' + evt.data+ '');
  },
  this.on = function(callback){
    if(callback && typeof callback === 'function'){
      callbacks.push(callback);
    }
  }
}