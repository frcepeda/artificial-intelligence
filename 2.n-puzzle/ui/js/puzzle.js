function setup(n, perm){
	var self = this;

	var p = $("#puzzle-wrapper");

	self.n = n;

	p.empty();

	if (perm === undefined){
		perm = [];
		for (var i = 0; i < n*n; i++)
			perm.push(i);
	}

	self.nextStep = 0;
	self.start = perm.slice(0);
	self.perm = perm;

	self.inv = [];

	for (var i = 0; i < n*n; i++)
		self.inv[perm[i]] = i;

	self.pos = {};
	self.cells = {};

	for (var i = 0; i < n*n; i++){
		self.cells[i] = $("<div></div>",
				 { class: "cell",
				   text: i});
		p.append(self.cells[i]);
	}

	var w = $(".cell").outerWidth();
	var h = $(".cell").outerHeight();

	for (var i = 0; i < n*n; i++){
		var pi = perm[i];
		self.pos[pi] = {
			top: h*Math.floor(i/n),
			left: w*(i%n)
		}
		self.cells[pi].css(self.pos[pi]);
	}

	self.cells[0].css("display", "none");

	self.move = function(id, callback){
		var tmp = self.pos[0];
		self.pos[0] = self.pos[id];
		self.pos[id] = tmp;

		var iinv = self.inv[id]
		var zinv = self.inv[0];

		self.perm[iinv] = 0;
		self.perm[zinv] = id;
		self.inv[0] = iinv;
		self.inv[id] = zinv;

		self.cells[id].animate(
			self.pos[id],
			{complete: callback}
		);
	}

	self.step = function(){
		if (!self.solvable) return;
		if (self.nextStep == self.solution.length) return;
		self.move(self.solution[self.nextStep++]);
	}

	self.sequence = function(xs, callback){
		var i = 0;

		var loop = function(){
			if (i == xs.length){
				if (callback !== undefined)
					callback();
				return;
			}
			self.move(xs[i++], loop);
		};

		loop();
	}

	self.allSteps = function(){
		if (!self.solvable) return;
		self.sequence(self.solution.slice(self.nextStep));
		self.nextStep = self.solution.length;
	}

	return self;
}

function StopWatch(){
	var self = this;

	self.state = 'STOPPED';

	self.reset = function(){
		if (self.state === 'RUNNING')
			self.stop();
		$('#stopwatch').text('00:00.0000');
	};

	self.start = function(){
		if (self.state !== 'STOPPED')
			throw "stopwatch: invalid state."
		self.state = 'RUNNING';
		self.startTime = new Date().getTime();
		self.tickEvent = setInterval(self.refreshDisplay, 1);
	};

	self.stop = function(){
		if (self.state !== 'RUNNING')
			throw 'stopwatch: invalid state.'
		clearInterval(self.tickEvent);
		self.state = 'STOPPED';
	};

	self.refreshDisplay = function(){
		var diff = new Date().getTime() - self.startTime;
		var millis = diff % 1000;
		var seconds = Math.floor(diff / 1000) % 60;
		var minutes = Math.floor(diff / (1000 * 60));
		var pad = function(i,l){
			var r = '' + i;
			while (r.length < l)
				r = '0' + r;
			return r;
		}
		$('#stopwatch').text(pad(minutes,2)+':'+pad(seconds,2)+'.'+pad(millis,4));
	};

	return self;
}

$(function (){
	stopwatch = new StopWatch();
	var currentGame = undefined;
	var firstRun = true;

	var loadPermutation = function(p){
		$("#permutation").val(p);
		var n = Math.round(Math.sqrt(p.length));
		currentGame = setup(n, p);
		currentGame.solvable = undefined;
		firstRun = false;
		$("#message").text('Ready.');
		stopwatch.reset();
	};

	$("#permutation-form").submit(function(){
		var perm = $("#permutation").val().split(',');
		for (var i = 0; i < perm.length; i++)
			perm[i] = parseInt(perm[i]);
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
		console.log(perm);
		loadPermutation(perm);
	});

	$("#reset").click(function(){
		if (firstRun == true){
			currentGame.start = undefined;
			firstRun = false;
		}
		currentGame = setup(currentGame.n, currentGame.start);
	});

	$("#step").click(function(){
		currentGame.step();
	});

	$("#allSteps").click(function(){
		currentGame.allSteps();
	});

	$("#solve").click(function(){
		if (currentGame.solvable === undefined){
			$("#message").text('Solving...');

			var payload = {
				size: currentGame.n,
				permutation: currentGame.start,
				token: "SEEKRITTOKEN"
			};

			stopwatch.start();

			$.post({
				type: "POST",
				url: "http://ssh.freddy.mx:9592",
				data: JSON.stringify(payload),
				success: function(data){
					var r = JSON.parse(data);
					console.log(r);

					if (r.solution){
						currentGame.solution = r.solution;
						currentGame.solvable = true;
						$("#message").text('Solved in '
								  + currentGame.solution.length
								  + ' steps!');
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

	var currentGame = setup(3);

	currentGame.sequence([1,2,5,5,2,1]);

	$('#message').text('Ready.');
});
