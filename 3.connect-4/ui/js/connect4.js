function Connect4(r, c){
	this.r = r;
	this.c = c;

	this.jcanvas = $('#game-window');

	this.grid = [];
	for (var i = 0; i < r; i++){
		this.grid.push([]);
		for (var j = 0; j < c; j++)
			this.grid[i].push('');
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
	this.state = 'PLAYING';
}

Connect4.prototype.step = function(callback){
	if (this.state !== 'PLAYING')
		throw 'connect4 step: invalid state';

	var self = this;
	this.moveFunc[this.currentPlayer](function (m) {
		var valid = self.play(m, self.currentPlayer);
		if (valid)
			self.currentPlayer = (self.currentPlayer + 1) % 2;
		callback(valid);
	});
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
	for (; i > 0 && this.grid[i-1][j] === ''; i--);
	if (i == this.r){
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

$(document).ready(function (){
	var connect4 = new Connect4(6,7);

	$(window).resize(connect4.draw.bind(connect4));

	var randomPlayer = function(callback){
		connect4.cursor('wait');
		window.setTimeout(function () {
			connect4.cursor('auto');
			callback(Math.floor(Math.random() * 7));
		}, 700);
	}

	var humanPlayer = function(callback){
		connect4.cursor('pointer');
		connect4.clickCallback = function (m){
			connect4.cursor('auto');
			connect4.clickCallback = null;
			callback(m);
		};
	}

	connect4.draw();
	connect4.begin(humanPlayer, humanPlayer);
	var i = 0;
	var loop = function () { connect4.step(loop); };
	connect4.step(loop);
});
