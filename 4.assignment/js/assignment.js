function assign(graph, params){
	var N = graph.length;
	var closure = [];

	for (var i = 0; i < N; i++){
		var w = [];
		for (var j = 0; j < N; j++)
			w.push(graph[i][j]);
		closure.push(w);
	}

	for (var i = 0; i < N; i++)
		for (var j = 0; j < N; j++)
			for (var k = 0; k < N; k++)
				closure[i][j] = closure[i][j] ||
				                (closure[i][k] && closure[k][j]);

	var v = [], blocked = [], assignment = [];
	for (var i = 0; i < N; i++){
		var k = 0;

		for (var j = 0; j < N; j++)
			if (closure[i][j])
				k++;

		v.push(k);
		assignment.push(null);
		blocked.push([]);
	}

	var cnt = [];
	for (var k = 0; k < N; k++){
		var m = null;

		for (var i = 0; i < N; i++)
			if (assignment[i] == null &&
			    (m == null || v[i] > v[m]))
				m = i;

		if (m == null) return null;

		for (var i = 0; true; i++){
			if (blocked.indexOf(i) != -1) continue;
			while (cnt.length <= i) cnt.push(0);
			if (cnt[i] >= params.maxCnt) continue;
			cnt[i]++;
			assignment[m] = i;
			break;
		}

		
		for (var i = 0; i < N; i++)
			if (closure[m][i])
				v[i]--;

		for (var i = 0; i < N; i++)
			if (closure[m][i])
				v[i]--;
	}

	return assignment;
}

var colors = [
"#FFFF00", "#1CE6FF", "#FF34FF", "#FF4A46", "#008941", "#006FA6", "#A30059",
"#FFDBE5", "#7A4900", "#0000A6", "#63FFAC", "#B79762", "#004D43", "#8FB0FF", "#997D87",
"#5A0007", "#809693", "#FEFFE6", "#1B4400", "#4FC601", "#3B5DFF", "#4A3B53", "#FF2F80",
"#61615A", "#BA0900", "#6B7900", "#00C2A0", "#FFAA92", "#FF90C9", "#B903AA", "#D16100",
"#DDEFFF", "#000035", "#7B4F4B", "#A1C299", "#300018", "#0AA6D8", "#013349", "#00846F",
"#372101", "#FFB500", "#C2FFED", "#A079BF", "#CC0744", "#C0B9B2", "#C2FF99", "#001E09",
"#00489C", "#6F0062", "#0CBD66", "#EEC3FF", "#456D75", "#B77B68", "#7A87A1", "#788D66",
"#885578", "#FAD09F", "#FF8A9A", "#D157A0", "#BEC459", "#456648", "#0086ED", "#886F4C",
"#34362D", "#B4A8BD", "#00A6AA", "#452C2C", "#636375", "#A3C8C9", "#FF913F", "#938A81",
"#575329", "#00FECF", "#B05B6F", "#8CD0FF", "#3B9700", "#04F757", "#C8A1A1", "#1E6E00",
"#7900D7", "#A77500", "#6367A9", "#A05837", "#6B002C", "#772600", "#D790FF", "#9B9700",
"#549E79", "#FFF69F", "#201625", "#72418F", "#BC23FF", "#99ADC0", "#3A2465", "#922329",
"#5B4534", "#FDE8DC", "#404E55", "#0089A3", "#CB7E98", "#A4E804", "#324E72", "#6A3A4C",
];

$(document).ready(function (){
	var s;

	$('#assign').click(function(){
		var N = 10;
		var nodes = [], edges = [], matrix = [];

		if (s != null) s.kill();

		for (var i = 0; i < N; i++){
			nodes.push({
				id: i,
				label: i,
				x: 100 * Math.cos(2 * i * Math.PI / N),
				y: 100 * Math.sin(2 * i * Math.PI / N),
				size: 5,
				color: colors[i]
			});
		}

		edges.push({id:0, source:0, target:1});

		var data = { nodes: nodes, edges: edges };

		s = new sigma({
			container: 'graph-container',
			graph: data,
			settings: {
				defaultNodeColor: '#FFFFFF',
				defaultEdgeColor: '#FFFFFF',
				defaultEdgeType: 'arrow'
			}
		});
	});
});
