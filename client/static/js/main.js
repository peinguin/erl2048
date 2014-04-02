var SERVER = 'ws://127.0.0.1:8080/websocket';

var MyGame = function(){
  var self = this,
    actuator,
    inputManager;

  self.move = function(direction){
    // 0: up, 1: right, 2:down, 3: left

    if(direction === 0){
      direction = 'up';
    }else if(direction === 1){
      direction = 'right';
    }else if(direction === 2){
      direction = 'down';
    }else if(direction === 3){
      direction = 'left';
    }
    websocket.send('move_' + direction);
  };
  self.restart = function(evt){
    websocket.send('start');
  };
  self.keepPlaying = function(evt){

  };
  self.wsHandler = function(evt){
    var game = JSON.parse(evt.data);

    var grid = {cells: []};
    game.grid.forEach(function (column, y) {
      var row = [];
      column.forEach(function (cell, x) {
        if(cell){
          if(cell.mergedFrom){
            cell.mergedFrom.forEach(function(tile){
              tile['x'] = x;
              tile['y'] = y;
            });
          }
          row.push({
            value:            cell.value,
            x:                x,
            y:                y,
            previousPosition: cell.previousPosition,
            mergedFrom:       cell.mergedFrom
          });
        }
      });
      grid.cells.push(row);
    });

    actuator.actuate(grid, {
      score:     game.score,
      bestScore: game.scores[0].score,
      score: game.score,
      won: game.won,
      over: game.over,
      keepPlaying: game.keepPlaying
    });
  };

  inputManager = new KeyboardInputManager;
  actuator     = new HTMLActuator;

  inputManager.on("move", self.move);
  inputManager.on("restart", self.restart);
  inputManager.on("keepPlaying", self.keepPlaying);

  websocket.on(self.wsHandler);

  self.restart();
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