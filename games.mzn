include "globals.mzn";

int: num;
int: cap;
int: refill;
array[int] of int: fun;

array[1..num] of var 0..cap: pocket;
array[1..num] of var 0..cap: visits;


constraint pocket[1] = cap;

constraint
    forall (i in 2..num) (
		minimum(pocket[i], [pocket[i-1] - visits[i-1] + refill, cap])
    );
	
constraint
    forall (i in 1..num) (
		(visits[i] <= pocket[i])  %/\ (visits[i] > 0)
    );

constraint at_most(1, visits, 0);


var int: reward = sum(i in 1..num) (visits[i]*fun[i]);

solve maximize reward;

output ["reward = ", show(reward), show(visits)];
