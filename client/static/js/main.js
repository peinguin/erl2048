var SERVER = 'ws://127.0.0.1:8080/websocket';

var MyGame = function(){
  var self = this,
    actuator,
    inputManager,
    playername = document.getElementById('playername'),
    toMove = true;

  self.move = function(direction){
    // 0: up, 1: right, 2:down, 3: left
    if(!toMove){
      return false;
    }
    if(direction === 0){
      direction = 'up';
    }else if(direction === 1){
      direction = 'right';
    }else if(direction === 2){
      direction = 'down';
    }else if(direction === 3){
      direction = 'left';
    }
    websocket.send(JSON.stringify({
      action:'move',
      value: direction
    }));
  };
  self.restart = function(evt){
    websocket.send(JSON.stringify({
      action:'start'
    }));
  };
  self.keepPlaying = function(evt){

  };
  self.wsHandler = function(evt){
    var game = JSON.parse(evt.data);

    if(game.grid){

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

      var scores = game.scores;

      actuator.actuate(grid, {
        score:     game.score,
        bestScore: scores.length > 0?scores[0].score:0,
        score: game.score,
        won: game.won,
        over: game.over,
        keepPlaying: game.keepPlaying
      });
    }

    //playername actuator
    if(game.name){
      if(playername.value !== playername){
        playername.value = game.name;
      }
    }
  };

  inputManager = new KeyboardInputManager;
  actuator     = new HTMLActuator;

  inputManager.on("move", self.move);
  inputManager.on("restart", self.restart);
  inputManager.on("keepPlaying", self.keepPlaying);

  websocket.on(self.wsHandler);

  playername.onfocus = function(){
    toMove = false;
  };
  playername.onblur = function(){
    websocket.send(JSON.stringify({
      action:'newName',
      value: this.value
    }));
    toMove = true;
  };
  playername.onchange = function(e){
    if(!toMove){
      return false;
    }
    websocket.send(JSON.stringify({
      action:'newName',
      value: this.value
    }));
  };

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