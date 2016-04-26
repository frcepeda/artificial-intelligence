function h = condEntropy(m)

h = 0;
n = 0;

for i=1:length(m)
	block = m{1,i};
	p = size(block, 1);
	n += p;
	h += p * entropy(block);
end

h /= n;
