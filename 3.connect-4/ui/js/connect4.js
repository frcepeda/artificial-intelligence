function Connect4(r, c){
	this.r = r;
	this.c = c;

	this.grid = [];
	for (var i = 0; i < r; i++){
		this.grid.push([]);
		for (var j = 0; j < c; j++)
			this.grid[i].push('');
	}

	this.state = 'READY';
}

Connect4.prototype.begin = function(p1, p2){
	if (this.state !== 'READY')
		throw 'connect4 begin: invalid state';
	this.moveFunc = [p1, p2];
	this.currentPlayer = 0;
	this.state = 'PLAYING';
}

Connect4.prototype.step = function(){
	if (this.state !== 'PLAYING')
		throw 'connect4 step: invalid state';

	var m = this.moveFunc[this.currentPlayer]();
	this.play(m, this.currentPlayer);

	this.currentPlayer = (this.currentPlayer + 1) % 2;
}

Connect4.prototype.play = function(j, p){
	var i = this.r;
	for (; i > 0 && this.grid[i-1][j] === ''; i--);
	if (i == this.r)
		throw 'play: invalid move ' + j + ' by player ' + p;
	this.grid[i][j] = p;
	this.draw();
}

Connect4.prototype.draw = function(){
	var c = $('#game-window');
	var p = $(c).parent();

	var pw = $(p).innerWidth();
	var ph = $(p).innerHeight();

	var block = Math.min(pw/this.c, ph/this.r);
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

	var randomPlayer = function(){
		return Math.floor(Math.random() * 7);
	}

	connect4.begin(randomPlayer, randomPlayer);
	for (var i = 0; i < 10; i++)
		connect4.step();
});
