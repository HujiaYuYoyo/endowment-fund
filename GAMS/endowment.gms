* MS&E 348 - Endowment Fund Management
* 
* This model represents a multistage stochastic program exploring managing
* an endowment fund portfolio, with an initial deterministic stage for 
* asset allocation and then rebalancing in each stochastic stage.

* Data - from fitted distributions
Set 
	i "assets"	/stock, cbond, gbond, alter, cash/
	ci(i) "cash asset" /cash/
	ai(i) "non-cash assets" /stock, cbond, gbond, alter/;

Alias (i, ii);

Parameter rbar(i) "mean returns"
/
stock	1.0653
cbond	1.0440
gbond	1.0336
alter	1.0880
cash	1.0214
/;

Parameter rstd(i) "standard deviation of returns"
/
stock	0.5233
cbond	0.1390
gbond	0.3820
alter	0.1280
cash	0.9395
/;

Table chol(i, ii) "covariance matrix of returns"
	stock	cbond	gbond	alter	cash
stock	1.020	1.235	0.950	0.102	0.238
cbond	0.000	0.200	1.392	1.580	0.280
gbond	0.000	0.000	1.029	0.138	1.000
alter	0.000	0.000	0.000	0.481	1.390
cash	0.000	0.000	0.000	0.000	0.021;

* Model params
Scalar 
	w0			"initial funds/wealth"		/1000000/
	wtarget		"wealth target scale"		/1.5/; 

Parameter 
	x0(i)	"amount of initial holdings in assets";

x0(ai) = 0.0;
x0(ci) = w0;
display x0;

Parameter 
	strc(i)		"sell transaction cost/asset"
	btrc(i)		"buy transaction cost/asset";

strc(i) = 0.0005;  
btrc(i) = 0.0005;

* Settings - define scenarios
options limrow = 0, limcol = 0;
option reslim = 1000000;
option iterlim = 1000000;
option solprint = off;
option lp = minos5;
option nlp = minos5;

Set
	w1	"first stage scenarios"		/w1_01*w1_05/
	w2 	"second stage scenarios"	/w2_01*w2_05/
	w3	"third stage scenarios"		/w3_01*w3_05/;

Scalars 
	n1	"number of scenarios in stage 1"
	n2	"number of scenarios in stage 2"
	n3	"number of scenarios in stage 3";

n1 = card(w1);
n2 = card(w2);
n3 = card(w3);
display n1, n2, n3;

Parameter 
	nall	"total number of scenarios";

nall = n1 * n2 * n3;

* Cash flow - define and initializing 
Parameters
	cf1				"cash flow into 1 from end of stage 0"
	cf2(w1)			"cash flow into 2 from end of stage 1"
	cf3(w1,w2)		"cash flow into 3 from end of stage 2"
	cf4(w1,w2,w3)	"cash flow into 4 from end of stage 3";

cf1				= 0.0;
cf2(w1)			= 0.0;
cf3(w1,w2) 		= 0.0;
cf4(w1,w2,w3) 	= 0.0;

* Sampling - use cholesky and lognormal
Parameters
	r1(i, w1)	"return scenarios at end of stage 1"
	r2(i, w2)	"return scenarios at end of stage 2"
	r3(i, w3)	"return scenarios at end of stage 3";

Parameters
	r1bar_est(i)	"estimate for mean return 1"
	r2bar_est(i)	"estimate for mean return 2"
	r3bar_est(i)	"estimate for mean return 3"
	r1std_est(i)	"estimate for std dev return 1"
	r2std_est(i)	"estimate for std dev return 2"
	r3std_est(i)	"estimate for std dev return 3"
	nv(ii)	"normal variable samples";

loop(w1,
	loop(ii, 
		nv(ii) = normal(0,1);
	);
	r1(i, w1) = sum(ii, chol(ii, i) * nv(ii));
);

loop(w2,
	loop(ii, 
		nv(ii) = normal(0,1);
	);
	r2(i, w2) = sum(ii, chol(ii, i) * nv(ii));
);

loop(w3,
	loop(ii, 
		nv(ii) = normal(0,1);
	);
	r3(i, w3) = sum(ii, chol(ii, i) * nv(ii));
);
display r1, r2, r3;

r1bar_est(i) = sum(w1, r1(i,w1))/n1;
r2bar_est(i) = sum(w2, r2(i,w2))/n2;
r3bar_est(i) = sum(w3, r3(i,w3))/n3;

* Sampling - mean correction
r1(i, w1) = r1(i, w1) - r1bar_est(i);
r2(i, w2) = r2(i, w2) - r2bar_est(i);
r3(i, w3) = r3(i, w3) - r3bar_est(i);

r1bar_est(i) = sum(w1, r1(i,w1))/n1;
r2bar_est(i) = sum(w2, r2(i,w2))/n2;
r3bar_est(i) = sum(w3, r3(i,w3))/n3;

* Sampling - calculate estimated std
r1std_est(i) = sqrt(sum(w1, sqr(r1(i,w1) - r1bar_est(i)))/(n1-1));
r2std_est(i) = sqrt(sum(w2, sqr(r2(i,w2) - r2bar_est(i)))/(n2-1));
r3std_est(i) = sqrt(sum(w3, sqr(r3(i,w3) - r3bar_est(i)))/(n3-1));

* Sampling - std correction
r1(i, w1) = r1(i, w1)*rstd(i)/r1std_est(i);
r2(i, w2) = r2(i, w2)*rstd(i)/r1std_est(i);
r3(i, w3) = r3(i, w3)*rstd(i)/r1std_est(i);

r1std_est(i) = sqrt(sum(w1, sqr(r1(i,w1)- r1bar_est(i)))/(n1-1));
r2std_est(i) = sqrt(sum(w2, sqr(r2(i,w2)- r2bar_est(i)))/(n2-1));
r3std_est(i) = sqrt(sum(w3, sqr(r3(i,w3)- r3bar_est(i)))/(n3-1));

* Sampling - adding true mean 
r1(i, w1) = r1(i, w1) + rbar(i);
r2(i, w2) = r2(i, w2) + rbar(i);
r3(i, w3) = r3(i, w3) + rbar(i);

r1bar_est(i) = sum(w1, r1(i,w1))/n1;
r2bar_est(i) = sum(w2, r2(i,w2))/n2;
r3bar_est(i) = sum(w3, r3(i,w3))/n3;

display rbar, r1bar_est, r2bar_est, r3bar_est;
display rstd, r1std_est, r2std_est, r3std_est;

* Sampling - convert to actual return from log
r1(i, w1) = exp(r1(i, w1));
r2(i, w2) = exp(r2(i, w2));
r3(i, w3) = exp(r3(i, w3));

* Stage params
Positive Variables
	x1(i)		"amount held of assets for stochastic stage 1"
	z1(i)		"amount bought of assets for stochastic stage 1"
	y1(i)		"amount sold of assets for stochastic stage 1"
	x2(i,w1)		"amount held of assets for stochastic stage 2"
	z2(i,w1)		"amount bought of assets for stochastic stage 2"
	y2(i,w1)		"amount sold of assets for stochastic stage 2"
	x3(i,w1,w2)		"amount held of assets for stochastic stage 3"
	z3(i,w1,w2)		"amount bought of assets for stochastic stage 3"
	y3(i,w1,w2)		"amount sold of assets for stochastic stage 3";
 
x1.lo(i) = 0;
x2.lo(i,w1) = 0;
x3.lo(i,w1,w2) = 0;

* Initialization of params
x1.l(i) = w0/card(i);
x2.l(i,w1) = w0/card(i);
x3.l(i,w1,w2) = w0/card(i);

* Cash asset cannot be bought or sold
z1.up(ci) = 0;
y1.up(ci) = 0;
z2.up(ci,w1) = 0;
y2.up(ci,w1) = 0;
z3.up(ci,w1,w2) = 0;
y3.up(ci,w1,w2) = 0;

* Definitions for utility
Scalar
	alpha		"alpha for utility function"	/20/;

Free Variable
	utility		"utility of investments";

* Constraint equations
*
* Removed:
* termw(w1,w2,w3)	"terminal utility constraints";
* termw(w1,w2,w3).. -sum(j,r3(j,w3)*x3(j,w1,w2)) + u(w1,w2,w3) - v(w1,w2,w3) =e= - wtarget + cf4(w1,w2,w3);
Equations
	objfunc			"objective function - utility"
	abalance1(ai)	"initial balancing for assets, at stage 1"
	cbalance1(ci)	"initial balancing for cash, at stage 1"
	arebalance2(ai,w1)	"rebalancing for assets, at stage 2"
	crebalance2(ci,w1)	"rebalancing for cash, at stage 2"
	arebalance3(ai,w1,w2)	"rebalancing for assets, at stage 3"
	crebalance3(ci,w1,w2)	"rebalancing for cash, at stage 3";

objfunc..	utility =e= sum((w1,w2,w3),(sum(i,r3(i,w3)*x3(i,w1,w2))**(1-alpha)-1)/(1-alpha))/nall;

abalance1(ai).. x1(ai) + y1(ai) - z1(ai) =e= x0(ai);

cbalance1(ci).. x1(ci) + sum(ii, (1-strc(ii))*y1(ii) + (1+btrc(ii))*z1(ii)) =e= x0(ci) + cf1;

arebalance2(ai,w1).. -r1(ai,w1)*x1(ai) + x2(ai,w1) + y2(ai, w1) - z2(ai,w1) =e= 0;

crebalance2(ci,w1).. -r1(ci,w1)*x1(ci) + x2(ci,w1) + sum(ii, - (1-strc(ii))*y2(ii,w1) + (1+btrc(ii))*z2(ii,w1)) =e= cf2(w1);

arebalance3(ai,w1,w2).. -r2(ai,w2)*x2(ai,w1) + x3(ai,w1,w2) + y3(ai,w1,w2) - z3(ai,w1,w2) =e= 0;

crebalance3(ci,w1,w2).. -r2(ci,w2)*x2(ci,w1) + x3(ci,w1,w2) + sum(ii, (1-strc(ii))*y3(ii,w1,w2) + (1+btrc(ii))*z3(ii,w1,w2)) =e= cf3(w1,w2);

* Solve
*option lp = cplex;
 option mpec=nlpec;

Model endowment /all/;
Solve endowment using mpec maximizing utility;

display utility.l, x1.l, y1.l, z1.l, x2.l, y2.l, z2.l, x3.l, y3.l, z3.l;