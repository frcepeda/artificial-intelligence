function [nodes, edges, id] = toGraphViz(tree, id)

if (strcmp(tree.type, 'terminal'))
	label = num2str(tree.label);
	color = 'lawngreen';
else
	label = num2str(tree.column);
	color = 'deepskyblue';
end

myid = num2str(id);
id = id + 1;

nodes = {sprintf('%s [label = "%s", style = "filled", color = "%s"];\n',...
				 myid, label, color)};
edges = {};

if (strcmp(tree.type, 'split'))
	for i=1:length(tree.children)
		child = tree.children{i};
		if (size(child) == 0)
			continue;
		end
		edge = {sprintf('%s -> %s [label = "%s"];\n',...
						 myid, num2str(id), num2str(i))};
		[n, e, id] = toGraphViz(child, id);
		nodes = [nodes; n];
		edges = [edges; edge; e];
	end
end
