function Connect4(n, r, c){
	this.n = n;
	this.r = r;
	this.c = c;

	this.jcanvas = $('#game-window');

	this.grid = [];
	for (var i = 0; i < r; i++){
		this.grid.push([]);
		for (var j = 0; j < c; j++)
			this.grid[i].push(null);
	}

	$('#game-window').unbind('onclick');
	$('#game-window').click(this.handleClick.bind(this));

	this.state = 'READY';
}

Connect4.prototype.begin = function(p1, p2){
	if (this.state !== 'READY')
		throw 'connect4 begin: invalid state';
	this.moveFunc = [p1, p2];
	this.currentPlayer = 0;
	this.totalMoves = 0;
	this.state = 'PLAYING';
}

Connect4.prototype.step = function(callback){
	if (this.state !== 'PLAYING')
		throw 'connect4 step: invalid state';

	var self = this;
	this.moveFunc[this.currentPlayer](function (m) {
		var valid = self.play(m, self.currentPlayer);
		if (valid){
			self.totalMoves++;
			self.currentPlayer = (self.currentPlayer + 1) % 2;
			var w = self.winner();
			if (w !== null){
				self.state = 'DONE';
				self.result = w;

				if (self.finishedCallback)
					self.finishedCallback(w);
			}
		}
		callback(valid);
	});
}

Connect4.prototype.winner = function(){
	if (this.checkColor(0)) return 0;
	if (this.checkColor(1)) return 1;
	if (this.totalMoves === this.r * this.c) return 'DRAW';
	return null;
}

Connect4.prototype.checkColor = function(p){
	for (var i = 0; i < this.r; i++){
		var count = 0;
		for (var j = 0; j < this.c; j++){
			if (this.grid[i][j] === p)
				count++;
			else
				count = 0;

			if (count === this.n) return true;
		}
	}

	for (var j = 0; j < this.c; j++){
		var count = 0;
		for (var i = 0; i < this.r; i++){
			if (this.grid[i][j] === p)
				count++;
			else
				count = 0;

			if (count === this.n) return true;
		}
	}

	for (var k = 0; k <= this.r + this.c - 2; k++){
		var count = 0;
		for (i = 0; i <= k; i++){
			if (i >= this.r || k-i >= this.c) continue;
			if (this.grid[i][k-i] === p)
				count++;
			else
				count = 0;

			if (count === this.n) return true;
		}
	}

	for (var k = 0; k <= this.r + this.c - 2; k++){
		var count = 0;
		for (i = 0; i <= k; i++){
			if (i >= this.r || k-i >= this.c) continue;
			if (this.grid[this.r - i - 1][k-i] === p)
				count++;
			else
				count = 0;

			if (count === this.n) return true;
		}
	}

	return false;
}


Connect4.prototype.handleClick = function(e){
	if (this.clickCallback){
		var realX = e.clientX - this.jcanvas.offset().left;
		var column = Math.floor(realX / this.block);
		this.clickCallback(column);
	}
};

Connect4.prototype.cursor = function (s){
	this.jcanvas.css('cursor', s);
}

Connect4.prototype.play = function(j, p){
	var i = this.r;
	for (; i > 0 && this.grid[i-1][j] === null; i--);
	if (i === this.r){
		console.log('play: invalid move ' + j + ' by player ' + p);
		return false;
	}
	this.grid[i][j] = p;
	this.draw();
	return true;
}

Connect4.prototype.draw = function(){
	var c = this.jcanvas;
	var p = $(c).parent();

	var pw = $(p).innerWidth();
	var ph = $(p).innerHeight();

	var block = this.block = Math.min(pw/this.c, ph/this.r);
	var r = .4*block;

	var w = block*this.c;
	var h = block*this.r;

	$(c).attr('width', w);
	$(c).attr('height', h);

	var canvas = c.get(0);
	var ctx = canvas.getContext('2d');

	ctx.fillStyle = 'black';
	ctx.fillRect(0, 0, w, h);

	if (this.state === 'READY')
		return;

	ctx.beginPath();

	var off = block/2;
	for (var x = 0; x < this.c; x++)
		for (var y = 0; y < this.r; y++){
			ctx.moveTo((x+1)*block,
			           y*block + off);
			           
			ctx.arc(x*block + off,
			        y*block + off,
				r, 0, Math.PI*2);
		}

	ctx.clip();

	ctx.fillStyle = 'white';
	ctx.fillRect(0, 0, w, h);

	for (var p = 0; p < 2; p++){
		ctx.fillStyle = p == 0 ? 'blue' : 'red';
		for (var i = 0; i < this.r; i++)
			for (var j = 0; j < this.c; j++)
				if (this.grid[this.r-i-1][j] === p)
					ctx.fillRect(j*block,
					             i*block,
					             block,
					             block);
	}
}

function Stopwatch(){
	this.state = 'STOPPED';
}

Stopwatch.prototype.reset = function(){
	if (this.state === 'RUNNING')
		this.stop();
	$('#stopwatch').text('00:00.000');
};

Stopwatch.prototype.start = function(){
	if (this.state !== 'STOPPED')
		throw "stopwatch: invalid state."
	this.state = 'RUNNING';
	this.startTime = new Date().getTime();
	this.stopTime = undefined;
	requestAnimationFrame(this.refreshDisplay.bind(this));
};

Stopwatch.prototype.stop = function(){
	if (this.state !== 'RUNNING')
		throw 'stopwatch: invalid state.'
	this.stopTime = new Date().getTime();
	this.state = 'STOPPED';
};

Stopwatch.prototype.refreshDisplay = function(time){
	var t = this.stopTime || new Date().getTime();
	var diff = t - this.startTime;
	var millis = Math.floor(diff) % 1000;
	var seconds = Math.floor(diff / 1000) % 60;
	var minutes = Math.floor(diff / (1000 * 60));
	var pad = function(i,l){
		var r = '' + i;
		while (r.length < l)
			r = '0' + r;
		return r;
	}
	$('#stopwatch').text(pad(minutes,2)+':'+pad(seconds,2)+'.'+pad(millis,3));
	if (this.state === 'RUNNING')
		requestAnimationFrame(this.refreshDisplay.bind(this));
};

$(document).ready(function (){
	var currentGame = new Connect4(4,6,7);

	currentGame.finishedCallback = function (w){
		if (w === 'DRAW')
			alert('Draw!');
		else
			alert((w === 0 ? 'Blue' : 'Red') + ' player won!');
	};

	$(window).resize(currentGame.draw.bind(currentGame));

	var randomPlayer = function(callback){
		currentGame.cursor('wait');
		window.setTimeout(function () {
			currentGame.cursor('auto');
			callback(Math.floor(Math.random() * 7));
		}, 300);
	}

	var humanPlayer = function(callback){
		currentGame.cursor('pointer');
		currentGame.clickCallback = function (m){
			currentGame.cursor('auto');
			currentGame.clickCallback = null;
			callback(m);
		};
	}

	var AIPlayer = function(exp) {
		var f = function(callback){
			var payload = {
				target: currentGame.n,
				rows: currentGame.r,
				cols: currentGame.c,
				grid: currentGame.grid,
				experience: exp,
				player: currentGame.currentPlayer,
				token: "SEEKRITTOKEN"
			};

			var stopwatch = new Stopwatch();
			stopwatch.start();
			currentGame.cursor('wait');

			$.post({
				type: "POST",
				url: "http://ssh.freddy.mx:9593",
				data: JSON.stringify(payload),
				success: function(data){
					var r = JSON.parse(data);
					callback(r.move);
				}}).fail(function (){
					if(confirm("Something went wrong... Retry?"))
						f(callback);
				}).always(function (){
					stopwatch.stop();
					currentGame.cursor('auto');
				});
		};
		
		return f;
	};

	var players = [
		{func: humanPlayer, name: 'Human'},
		{func: randomPlayer, name: 'Random (CPU)'},
		{func: AIPlayer('Novice'), name: 'Novice (CPU)'},
		{func: AIPlayer('Amateur'), name: 'Amateur (CPU)'},
		{func: AIPlayer('Expert'), name: 'Expert (CPU)'},
	];

	for (var i = 0; i < players.length; i++){
		var a = new Option(players[i].name, i);
		var b = new Option(players[i].name, i);

		$('#playerA').append($(a));
		$('#playerB').append($(b));
	}

	if (sessionStorage){
		if (sessionStorage.getItem('playerA') !== undefined)
			$('#playerA').prop('selectedIndex', sessionStorage.getItem('playerA'));

		if (sessionStorage.getItem('playerB') !== undefined)
			$('#playerB').prop('selectedIndex', sessionStorage.getItem('playerB'));
	}

	$('#start').click(function() {
		$('#start').hide();
		$('#reset').show();

		currentGame.begin(players[$('#playerA').val()].func,
		                  players[$('#playerB').val()].func);
		currentGame.draw();

		var loop = function () {
			if (currentGame.state !== 'DONE')
				currentGame.step(loop);
		};
		loop();
	});

	$('#reset').hide();
	$('#reset').click(function (){
		if (sessionStorage){
			sessionStorage.setItem('playerA', $('#playerA').val());
			sessionStorage.setItem('playerB', $('#playerB').val());
		}

		document.location.reload();
	});

	currentGame.draw();
});
