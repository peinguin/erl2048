var SERVER = 'ws://127.0.0.1:8080/websocket';

var MyGame = function(){
  var self = this;

  self.move = function(evt){

  };
  self.restart = function(evt){

  };
  self.keepPlaying = function(evt){

  };
  self.wsHandler = function(evt){
    console.log(JSON.parse(evt.data));
  }

  var inputManager = new KeyboardInputManager;

  inputManager.on("move", self.move);
  inputManager.on("restart", self.restart);
  inputManager.on("keepPlaying", self.keepPlaying);

  websocket.on(self.wsHandler);

  websocket.send('start');
};

if(!("WebSocket" in window)){
  document.getElementById('container').innerHtml = '<p><span style="color: red;">websockets are not supported </span></p>';
} else {

  var websocket = new Websocket(SERVER);
  websocket
  .connect()
  .done(function(){
    var myGame = new MyGame(websocket);    
  });
};