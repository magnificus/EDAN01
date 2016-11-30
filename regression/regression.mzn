include "globals.mzn";
int: num_adders;
int: num_mults;
int: num_nodes;
int: num_types;
array[1..num_nodes, 1..2] of 0..num_nodes: dependencies;
array[1..num_nodes] of 0..num_types: types;
array[1..num_types] of int: cycles;
array[1..num_types] of int: resources;
array[1..num_nodes] of int: durations = [cycles[types[i]] | i in 1..num_nodes];
array[1..num_nodes] of var 0..5000: nodes;

%constraint forall (k in 2..num_types) (
%	cumulative(adders, durations, [1 | i in 1..num_adders], [1 | i in 1..num_adders])
%);

constraint cumulative(adders, [1 | i in 1..num_adders], [1 | i in 1..num_adders], resources[2]);

constraint cumulative(mults, [2 | i in 1..num_mults], [1 | i in 1..num_mults], resources[3]);

constraint forall (i in 2..num_nodes) (
	nodes[i] >= nodes[dependencies[i,1]] + cycles[types[dependencies[i,1]]] /\
	nodes[i] >= nodes[dependencies[i,2]] + cycles[types[dependencies[i,2]]]
);

var int: result;

% nbr of adders and mults
array[1..num_adders] of var 1..resources[2]: adder_constraints;
array[1..num_mults] of var 1..resources[3]: mult_constraints;

array[1..num_adders] of var 0..500: adders = [nodes[i] | i in 2..num_nodes, where types[i] == 2];
array[1..num_mults] of var 0..500: mults = [nodes[i]| i in 2..num_nodes, where types[i] == 3];

constraint diffn(adders, adder_constraints, [cycles[2] | i in 1..num_adders],[1 | i in 1..num_adders] );
constraint diffn(mults, mult_constraints, [cycles[3] | i in 1..num_mults], [1 | i in 1..num_mults]);

constraint forall(i in 2..num_nodes) (
	result >= nodes[i] + cycles[types[i]]
);

output ["result = " ++ show(result)
	++ "\nnodes = " ++ show([nodes[i] | i in 2..num_nodes])
	++ "\nadders: " ++ show([i | i in 2..num_nodes, where types[i] == 2])
	++ "\nmults: " ++ show([i | i in 2..num_nodes, where types[i] == 3])
	++ "\nadder_constraints: " ++ show(adder_constraints)
	++ "\nmult_constraints: " ++ show(mult_constraints)
];

%solve minimize(result);

%solve satisfy;
%solve :: int_search(nodes, smallest, indomain_min, complete) minimize result ;
solve :: seq_search([
	int_search(adders ++ mults, input_order, indomain_min, complete),
	int_search(adder_constraints ++ mult_constraints, input_order, indomain_min, complete)
]) minimize result 
	
