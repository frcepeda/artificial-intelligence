function h = entropy(m)

h = 0;

if (size(m) == 0)
	return;
end

classes = m(:,1)';

if (all(classes == classes(1)))
	return;
end

[freqs, ~] = hist(classes, unique(classes));
n = size(classes,2);

for x=freqs
	p = x / n;
	h = h - p * log2(p);
end
