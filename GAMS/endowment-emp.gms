* MS&E 348 - Endowment Fund Management
* 
* This model represents a multistage stochastic program exploring managing
* an endowment fund portfolio, with an initial deterministic stage for 
* asset allocation and then rebalancing in each stochastic stage.

* Core Model
Set 
 	t 	"stages"	/1*3/
 	dt(t)	"deterministic stage" /1/
 	st(t)	"stochastic stages"	/2*3/
	i	"assets"	/stock, cbond, gbond, alter, cash/
	ci(i) "cash asset" /cash/
	ai(i) "non-cash assets" /stock, cbond, gbond, alter/
	s 	"scenarios" /s1*s5/;

Positive Variables
	x(i, t)	"dollar amount in asset i in stage t"
	y(ai, st)	"amount sold of non-cash asset i in stage t"
	z(ai, st)	"amount bought of non-cash asset i in stage t";

Variable
	utility;

Scalars
	W		"initial wealth"	/1000000/
	alpha 	"power utility parameter"	/20/
	mu		"transaction cost - sell"	/0.0005/
	nu		"transaction cost - buy"	/0.0005/

Table r1s(s,i) "stage 1 scenario returns from assets (plug in with real values later)"
	stock	cbond	gbond	alter	cash
s1	1.300	1.225	1.149	1.295	1.020
s2	1.103	1.290	1.260	1.120	1.011
s3	0.954	0.728	0.988	0.733	0.998
s4	0.984	0.889	0.970	0.860	0.950
s5	1.103	1.203	1.149	1.34	1.040 ;

Table r2s(s,i) "stage 2 scenario returns from assets (plug in with real values later)"
	stock	cbond	gbond	alter	cash
s1	1.300	1.225	1.149	1.295	1.020
s2	1.103	1.290	1.260	1.120	1.011
s3	0.954	0.728	0.988	0.733	0.998
s4	0.984	0.889	0.970	0.860	0.950
s5	1.103	1.203	1.149	1.34	1.040 ;

Table r3s(s,i) "stage 3 scenario returns from assets (plug in with real values later)"
	stock	cbond	gbond	alter	cash
s1	1.300	1.225	1.149	1.295	1.020
s2	1.103	1.290	1.260	1.120	1.011
s3	0.954	0.728	0.988	0.733	0.998
s4	0.984	0.889	0.970	0.860	0.950
s5	1.103	1.203	1.149	1.34	1.040 ;

Parameters
* Random parameters
	r1(i) "return r.v. of asset at end of stage 1"
/
stock	1.0
cbond	1.0
gbond	1.0
alter	1.0
cash	1.0
/

	r2(i) "return r.v. of asset at end of stage 2"
/
stock	1.0
cbond	1.0
gbond	1.0
alter	1.0
cash	1.0
/

	r3(i) "return r.v. of asset at end of stage 3"
/
stock	1.0
cbond	1.0
gbond	1.0
alter	1.0
cash	1.0
/

* Remaining parameters
	r(i, t)	"return of asset i at end of stage t"
	p(s) "scenario probability";

p(s) = 1/card(s);
r(i, "1") = r1(i);
r(i, "2") = r2(i);
r(i, "3") = r3(i);
x.l(i, "1") = 20000;

Equations
	defutility "definition of utility"
	balance(dt)	"initial asset allocation equation"
	rebalance1(ai, st) "sbalance equation for assets"
	rebalance2(st) "balance equation for cash";

defutility.. 	utility =e= ((sum((i, t), r(i, t) * x(i, t)))**(1-alpha)-1)/(1-alpha);
balance(dt)..	W =e= sum(i, x(i, dt));
rebalance1(ai, st).. x(ai, st) + y(ai, st) - z(ai, st) =e= -r(ai, st-1)*x(ai, st-1);
rebalance2(st).. -r("cash", st-1)*x("cash", st-1)+x("cash", st) =e= sum(ai, (1-mu)*y(ai, st)-(1+nu)*z(ai, st));

Model endowment /all/;

* EMP Annotations
File emp / "%emp.info%" /; 
put emp 
 / "jrandvar r1('stock') r1('cbond') r1('gbond') r1('alter') r1('cash')"
loop(s,
  put / p(s) r1s(s,"stock") r1s(s,"cbond") r1s(s,"gbond") r1s(s,"alter") r1s(s,"cash"));
put /

 / "jrandvar r2('stock') r2('cbond') r2('gbond') r2('alter') r2('cash')"
loop(s,
  put / p(s) r2s(s,"stock") r2s(s,"cbond") r2s(s,"gbond") r2s(s,"alter") r2s(s,"cash"));
put /

 / "jrandvar r3('stock') r3('cbond') r3('gbond') r3('alter') r3('cash')"
loop(s,
  put / p(s) r3s(s,"stock") r3s(s,"cbond") r3s(s,"gbond") r3s(s,"alter") r3s(s,"cash"));
put /

$onput
stage 1 r1 x("1") balance("1")
stage 2 r2 x("2") y("2") z("2") rebalance1("2") rebalance2("2") 
stage 3 r3 x("3") y("3") z("3") rebalance1("3") rebalance2("3")
$offput
putclose emp;

option emp = lindo;
solve endowment using emp max utility;
