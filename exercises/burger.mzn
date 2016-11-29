include "alldifferent.mzn";

var 1..5: Beef;
var 1..5: Lettuce;

constraint	Beef = Lettuce;


solve satisfy;

output ["   ",show(Beef),show(Lettuce)];
