Set t 	'stages'	/1*3/;
Set i	'assets'	/stock, cbond, gbond, alternative, cash/;

Set ci(i) 	'cash index'	/5/;
Set cn(i)	'noncash index'	/1*4/;

Positive Variables
	x(i, t)	'dollar amount in asset i in stage t'
	y(i, t)	'amount sold of asset i in stage t'
	z(i, t)	'amount bought of asset i in stage t';

Free Variable
	utility;

Scalars
	W		'initial wealth'	/1000000/
	alpha 	'utility parameter'		/10/
	mu		'transaction cost - sell'	/0.0005/
	nu		'transaction cost - buy'	/0.0005/;

alias (i, ii);
Parameters
	mean(i)	'shape of return distribution - mean of normal distribution of asset i'
/
1   10.5
2 	8.5
3 	7
4	4
5	3
/

	stdev(i)	'stdev of normal distribution of asset i'
/
1	15.5
2	11.9
3	8.2
4 	0.1
5	0.3
/
	r(i) 	'return of asset i';

r(i) = normal(mean(i),stdev(i));

Equations
	defutility "definition of utility"
	balance(i, t)	'initial asset allocation'
	rebalance1(i, t) 'balance equation for assets'
	rebalance2(i, t) 'balance equation for cash';

defutility.. 	utility =e= ((sum((i, t), r(i) * x(i, t)))**(1-alpha)-1)/(1-alpha);
balance(i, t)..	W =e= sum(ii, x(i, t));
rebalance1(i, t).. x(i, t) + y(i, t) - z(i, t) = -r(i, t-1)*x(i, t-1);
rebalance2(i, t).. sum((i, t), -(1-mu)*y(i, t)$cn(i)+(1+nu)*z(i, t)$cn(i)) =e= -r(i, t-1)$ci(i)*x(i, t-1)$ci(i)+x(i, t)$ci(t);

Model endowment /all/;

* EMP Annotations
File emp / '%emp.info%' /; 
put emp; emp.nd=6;
$onput
stage 1 x('1') y('1') z('1') balance('1') rebalance1('1') rebalance2('1') 
stage 2 x('2') y('2') z('2') rebalance1('2') rebalance2('2') 
stage 3 x('3') y('3') z('3') rebalance1('3') rebalance2('3')
$offput
putclose emp;
option emp = lindo;
solve endowment using emp max utility;
display utility;

