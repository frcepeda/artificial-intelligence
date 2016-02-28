function Puzzle(o){
	var p = $("#puzzle-wrapper");
	p.empty();

	var n = this.n = o.n;

	if (!o.start){
		this.perm = [];
		for (var i = 0; i < n*n; i++)
			this.perm.push(i);
	} else this.perm = o.start;

	this.nextStep = 0;
	this.start = this.perm.slice(0);

	this.solvable = o.solvable != null ? o.solvable : null;
	this.solution = o.solution;

	this.inv = [];

	for (var i = 0; i < n*n; i++)
		this.inv[this.perm[i]] = i;

	this.pos = {};
	this.cells = {};

	for (var i = 0; i < n*n; i++){
		this.cells[i] = $("<div></div>",
				 { class: "cell",
				   text: i});
		p.append(this.cells[i]);
	}

	var w = $(".cell").outerWidth();
	var h = $(".cell").outerHeight();

	for (var i = 0; i < n*n; i++){
		var pi = this.perm[i];
		this.pos[pi] = {
			top: h*Math.floor(i/n),
			left: w*(i%n)
		}
		this.cells[pi].css(this.pos[pi]);
	}

	this.cells[0].css("display", "none");
}

Puzzle.prototype.move = function(id, callback){
	var tmp = this.pos[0];
	this.pos[0] = this.pos[id];
	this.pos[id] = tmp;

	var iinv = this.inv[id]
	var zinv = this.inv[0];

	this.perm[iinv] = 0;
	this.perm[zinv] = id;
	this.inv[0] = iinv;
	this.inv[id] = zinv;

	this.cells[id].animate(
		this.pos[id],
		{complete: callback}
	);
}

Puzzle.prototype.step = function(){
	if (!this.solvable) return;
	if (this.nextStep == this.solution.length) return;
	this.move(this.solution[this.nextStep++]);
}

Puzzle.prototype.sequence = function(xs, callback){
	var i = 0;

	var loop = function(){
		if (i == xs.length){
			if (callback)
				callback();
			return;
		}
		this.move(xs[i++], loop.bind(this));
	};

	loop.bind(this)();
}

Puzzle.prototype.allSteps = function(){
	if (!this.solvable) return;
	this.sequence(this.solution.slice(this.nextStep));
	this.nextStep = this.solution.length;
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

$(function (){
	stopwatch = new Stopwatch();
	var currentGame = undefined;
	var firstRun = true;

	var loadPermutation = function(p){
		$("#permutation").val(p);
		var n = Math.round(Math.sqrt(p.length));
		currentGame = new Puzzle({n: n, start: p});
		currentGame.solvable = undefined;
		firstRun = false;
		$("#message").text('Ready.');
		stopwatch.reset();
	};

	var toNumArray = function(str){
		var xs = str.split(',');
		for (var i = 0; i < xs.length; i++)
			xs[i] = parseInt(xs[i]);
		return xs;
	}

	$("#permutation-form").submit(function(){
		var perm = toNumArray($("#permutation").val());
		loadPermutation(perm);
		return false;
	});

	$("#random").click(function(){
		var perm = [];
		var n = 2 + Math.floor(Math.random()*3);
		for (var i = 0; i < n*n; i++){
			var r = Math.floor(Math.random() * (i+1));
			perm.push(perm[r]);
			perm[r] = i;
		}
		loadPermutation(perm);
	});

	$("#reset").click(function(){
		if (firstRun == true){
			currentGame.start = undefined;
			firstRun = false;
		}
		currentGame = new Puzzle(currentGame);
	});

	$("#step").click(function(){
		currentGame.step();
	});

	$("#allSteps").click(function(){
		currentGame.allSteps();
	});

	$("#solve").click(function(){
		if (currentGame.solvable == null){
			$("#message").text('Solving...');

			var target = null;
			if ($("#customTarget").is(":checked"))
				target = toNumArray($("#target").val());

			var payload = {
				size: currentGame.n,
				permutation: currentGame.start,
				target: target,
				token: "SEEKRITTOKEN"
			};

			stopwatch.start();

			$.post({
				type: "POST",
				url: "http://ssh.freddy.mx:9592",
				data: JSON.stringify(payload),
				success: function(data){
					var r = JSON.parse(data);

					if (r.solution){
						currentGame.solution = r.solution;
						currentGame.solvable = true;
						$("#message").text('Solved in '
								  + currentGame.solution.length
								  + ' step'
								  + (r.solution.length == 1 ? '' : 's')
								  + '!');
					} else {
						currentGame.solvable = false;
						$("#message").text('Not solvable :c');
					}
				}}).fail(function (){
					$("#message").text('Something went wrong...');
				}).always(function (){
					stopwatch.stop();
				});
		}
	});

	var currentGame = new Puzzle({n: 3});

	currentGame.sequence([1,2,5,4,3,6,7,8,
	                      4,5,2,3,8,7,6,1,
	                      3,2,5,8,1,6,7,4,
	                      8,5,2,1,4,7,6,3]);

	$('#message').text('Ready.');
});
