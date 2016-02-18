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

$(function (){
	var currentGame = undefined;
	var firstRun = true;

	$("#permutation-form").submit(function(){
		var perm = eval($("#permutation").val());
		var n = Math.round(Math.sqrt(perm.length));
		currentGame = setup(n, perm);
		currentGame.solvable = undefined;
		$("#message").text('Ready.');
		return false;
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
				});
		}
	});

	var currentGame = setup(3);

	currentGame.sequence([1,2,5,5,2,1]);

	$('#message').text('Ready.');
});
