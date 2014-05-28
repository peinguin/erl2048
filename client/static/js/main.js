var SERVER = 'ws://' + location.hostname + ':8081/websocket';

var MyGame = function(){
  var self = this,
    actuator,
    inputManager,
    playername = document.getElementById('playername'),
    toMove = true,
    scoresEl = document.getElementById('scores'),
    bestContainer = document.getElementsByClassName('best-container')[0];;

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
    websocket.send({
      action:'move',
      value: direction
    });
  };
  self.restart = function(evt){
    websocket.send({action:'start'});
    actuator.continue();
  };
  self.keepPlaying = function(evt){
    websocket.send({action:'keepPlaying'});
    actuator.continue();
  };
  self.wsHandler = function(evt){
    var game = JSON.parse(evt.data);

    //win. Go on?
    if(game.ask){
      actuator.message(true);
    }

    //lose
    else if(game.over){
      actuator.message(false);
    }

    else if(game.grid){

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

      var scores = game.scores,
        bestScore = 0;
      if(scores && scores.length>0){
        bestScore = scores[0].score;

        while (scoresEl.firstChild) {
          scoresEl.removeChild(scoresEl.firstChild);
        }

        scores.forEach(function(score){
          var div = document.createElement('Div');
          var name = document.createElement('Div');
          var scoreEl = document.createElement('Div');

          div.setAttribute("class", 'score');
          name.setAttribute("class", 'name');
          scoreEl.setAttribute("class", 'score');

          name.appendChild(document.createTextNode(score.name));
          scoreEl.appendChild(document.createTextNode(score.score));

          div.appendChild(name);
          div.appendChild(scoreEl);
          scoresEl.appendChild(div);
        });
      }

      actuator.actuate(grid, {
        score:     game.score,
        bestScore: bestScore,
        score: game.score,
        won: game.won,
        over: game.over,
        keepPlaying: game.keepPlaying
      });
    }

    //playername actuator
    if(game.user){
      if(playername.value !== playername){
        playername.value = game.user.name;
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
    websocket.send({
      action:'newName',
      value: this.value
    });
    toMove = true;
  };
  playername.onchange = function(e){
    if(!toMove){
      return false;
    }
    websocket.send({
      action:'newName',
      value: this.value
    });
  };

  bestContainer.onmousemove = function(e){
    scoresEl.style.display = 'block';
    scoresEl.style.left = e.pageX + 5 + 'px';
    scoresEl.style.top = e.pageY + 5 + 'px';
  }
  bestContainer.onmouseout = function(){
    scoresEl.style.display = 'none';
  }

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