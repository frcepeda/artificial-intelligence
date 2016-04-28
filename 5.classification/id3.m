function node = id3(m)

class = m(1,1);

if (all(m(:,1) == m(1,1)))
	node = struct('type', 'terminal',...
	              'label', class);
	return;
end

subs = cell(size(m,2));
sizes = max(m);
best = [2 Inf];
for c = 2:size(m,2)
	subs{c} = {};

	for i = 1:sizes(c)
		subs{c,i} = [];
	end
	
	if (m(1,c) == -1)
		continue;
	end

	for row = m'
		attr = row(c);
		row(c) = -1;
		subs{c,attr} = [subs{c,attr}; row'];
	end

	h = condEntropy(subs(c,:));

	if (h < best(2))
		best = [c h];
	end
end

col = best(1);

node.type = 'split';
node.column = col;
node.children = {};

for attr = 1:size(subs,2)
	go = subs{col, attr};

	if (size(go,1) == 0)
		continue;
	end;

	if (isequal(m,go))
		error('Can''t split the data set!');
	end

	node.children{attr} = id3(go);
end
