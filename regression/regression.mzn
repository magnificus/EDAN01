include "globals.mzn";

int: num_nodes;
int: num_types;
array[1..num_nodes, 1..2] of 0..num_nodes: dependencies;
array[1..num_nodes] of 0..num_types: types;
array[1..num_types] of int: cycles;
array[1..num_types] of int: resources;
array[1..num_nodes] of int: durations = [cycles[types[i]] | i in 1..num_nodes];
array[1..num_nodes] of var 0..5000: nodes;

constraint forall (k in 2..num_types) (
	cumulative(nodes, durations, [bool2int(types[i] == k) | i in 1..num_nodes], resources[k])
);

constraint   forall (i in 2..num_nodes) (
	nodes[i] >= nodes[dependencies[i,1]] + cycles[types[dependencies[i,1]]] /\
	nodes[i] >= nodes[dependencies[i,2]] + cycles[types[dependencies[i,2]]]
	);

var int: result;

constraint forall(i in 2..num_nodes) (
	result >= nodes[i] + cycles[types[i]]
);

output ["result = " ++ show(result),"\t", "nodes = " ++ show([nodes[i] | i in 2..num_nodes])];

solve minimize(result);