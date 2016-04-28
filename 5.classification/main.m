m = dlmread('dataset.dat');
s = id3(m);
[nodes, edges] = toGraphViz(s, 0);
printf('digraph G {\n %s %s}', strjoin(nodes), strjoin(edges))
