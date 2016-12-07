include "lex2.mzn";

int: n;
int: n_commercial;
int: n_residential;
array[1..n+1] of int: point_distribution;
array[1..n, 1..n] of var 0..1: positions;

array[1..n] of var int: column_positions;
array[1..n] of var int: row_positions;

array [1..2*n] of var int: results;


constraint forall (i in 1..n)(
	column_positions[i] = sum(j in 1..n)(positions[i, j]) /\
	row_positions[i] = sum(j in 1..n)(positions[j, i])
);

constraint sum(column_positions) = n_residential;

constraint forall (i in 1..n)(
	results[i] = point_distribution[column_positions[i]+1]
);
constraint forall (i in 1..n)(
	results[i+n] = point_distribution[row_positions[i]+1]
);

var int: total_result = sum(results);

constraint lex2(positions);

solve maximize total_result;

output["final result: ", show(total_result), " positions: ", show(positions), "results: ", show(results)];