function processor(){
    purge();
    var q = [];
    grid.forEach(function(row, y, rows){
	row.forEach(function(cell, x){
	    if(cell){
		q.push({cell:cell, x:x, y:y});
		if(cell.mergedFrom !== null){
		    cell.mergedFrom.forEach(function(fromCell){
			add(fromCell, x, y)
		    });
		}
	    }
	});
    });
    q.sort(function(c1, c2){
	var r = 0;
	      if(c1.cell.previousPosition !== null && c2.cell.previousPosition === null){
	    r = 1;
	}else if(c1.cell.previousPosition === null && c2.cell.previousPosition !== null){
	    r = -1;
	}
	return r;
    });
    q.forEach(function(c){
	add(c.cell, c.x, c.y);
    });
}

function add(cell, x, y){
    var tmp = numbers

    var params = {number: cell.value};

    if(cell.previousPosition !== null){
	params.scale = 1
	params.scale = 1
	params.colStart = cell.previousPosition.x
	params.rowStart = cell.previousPosition.y
	params.colEnd = x
	params.rowEnd = y
    }else{
	params.scale = 0
	params.scale = 0
	params.colStart = x
	params.rowStart = y
	params.colEnd = x
	params.rowEnd = y
    }

    var newNumber = number.createObject(
	cellGrid,
	params);
    tmp.push(newNumber)

    numbers = tmp
}
