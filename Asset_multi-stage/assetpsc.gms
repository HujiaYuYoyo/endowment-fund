*     MSandE348 Prof. Gerd Infanger
*     model asset -- asset allocation

*$onempty;
*$offlisting;
*$offsymxref;
*$offuellist;

*----------------------------------------------------------------------
$include "assetm.inc"
*----------------------------------------------------------------------

* Documentation of sets and parameters available
* set j assets
* set jj assets (alias)
* parameter rbar(j) mean returns
* parameter rstd(j) standard deviation of returns
* parameter rcorr(j, jj) correlation matrix


* redefining rbar(j), rstd(j) and chol(j, jj)  in per unit
rbar(j) = 1.0 + 0.01 * rbar(j);
rstd(j) = 0.01 * rstd(j);
chol(j, jj)= 0.01* chol(j, jj);


scalar wtarget  wealth target;
wtarget = 1.5;


* initial holdings
parameter x0(j) initial holdings;
x0(j) = 0.0;
x0(j)$(ord(j) = card(j)) = 1.0;
display x0;

parameter trc(j) transaction cost factor;
trc(j) = 1.0;


options limrow = 0, limcol = 0;
option reslim = 1000000;
option iterlim = 1000000;
option solprint = off;
option lp = minos5;
option nlp = minos5;
Option Seed=1;

set w1 scenarios /w1_01*w1_20/;
set w2 scenarios /w2_01*w2_20/;
set w3 scenarios /w3_01*w3_20/;
scalars nscen1, nscen2, nscen3;
nscen1=card(w1);
nscen2=card(w2);
nscen3=card(w3);
display nscen1, nscen2, nscen3;
parameter nscenall;
nscenall = nscen1*nscen2*nscen3;


* defining cash flow
parameter cf1              cash flow period 0;
parameter cf2(w1)          cash flow period 1;
parameter cf3(w1,w2)       cash flow period 2;
parameter cf4(w1,w2,w3)    cash flow period 3;

cf1           = 0.0;
cf2(w1)       = 0.0;
cf3(w1,w2)    = 0.0;
cf4(w1,w2,w3) = 0.0;



parameter r1(j, w1) return scenarios at period one;

parameter r2(j, w2) return scenarios at period two;

parameter r3(j, w3) return scenarios at period three;
parameters r1bar_est(j), r2bar_est(j), r3bar_est(j), r1std_est(j), r2std_est(j), r3std_est(j);
parameter nv(jj);

* calculating scenarios by sampling;
loop(w1,
  loop(jj,
  nv(jj) = normal(0,1);
  );
  r1(j, w1) = sum(jj, chol(jj, j) * nv(jj));
);

loop(w2,
  loop(jj,
  nv(jj) = normal(0,1);
  );
  r2(j, w2) = sum(jj, chol(jj, j) * nv(jj));
);

loop(w3,
  loop(jj,
  nv(jj) = normal(0,1);
  );
  r3(j, w3) = sum(jj, chol(jj, j) * nv(jj));
);

display r1, r2, r3;

r1bar_est(j) = sum(w1, r1(j,w1))/nscen1;
r2bar_est(j) = sum(w2, r2(j,w2))/nscen2;
r3bar_est(j) = sum(w3, r3(j,w3))/nscen3;

* mean correction
r1(j, w1) = r1(j, w1) - r1bar_est(j);
r2(j, w2) = r2(j, w2) - r2bar_est(j);
r3(j, w3) = r3(j, w3) - r3bar_est(j);

r1bar_est(j) = sum(w1, r1(j,w1))/nscen1;
r2bar_est(j) = sum(w2, r2(j,w2))/nscen2;
r3bar_est(j) = sum(w3, r3(j,w3))/nscen3;


* calculating estimated std
r1std_est(j) = sqrt(sum(w1, sqr(r1(j,w1) - r1bar_est(j)))/(nscen1-1));
r2std_est(j) = sqrt(sum(w2, sqr(r2(j,w2) - r2bar_est(j)))/(nscen2-1));
r3std_est(j) = sqrt(sum(w3, sqr(r3(j,w3) - r3bar_est(j)))/(nscen3-1));

* std correction
r1(j, w1) = r1(j, w1) * rstd(j)/r1std_est(j);
r2(j, w2) = r2(j, w2) * rstd(j)/r2std_est(j);
r3(j, w3) = r3(j, w3) * rstd(j)/r3std_est(j);

r1std_est(j) = sqrt(sum(w1, sqr(r1(j,w1)- r1bar_est(j)))/(nscen1-1));
r2std_est(j) = sqrt(sum(w2, sqr(r2(j,w2)- r2bar_est(j)))/(nscen2-1));
r3std_est(j) = sqrt(sum(w3, sqr(r3(j,w3)- r3bar_est(j)))/(nscen3-1));

* Adding true mean mean
r1(j, w1) = r1(j, w1) + rbar(j);
r2(j, w2) = r2(j, w2) + rbar(j);
r3(j, w3) = r3(j, w3) + rbar(j);

r1bar_est(j) = sum(w1, r1(j,w1))/nscen1;
r2bar_est(j) = sum(w2, r2(j,w2))/nscen2;
r3bar_est(j) = sum(w3, r3(j,w3))/nscen3;


display rbar, r1bar_est, r2bar_est,r3bar_est;
display rstd, r1std_est, r2std_est, r3std_est;

positive variable x1(j) holding period 1;
x1.lo(j) = 0;
x1.l(j) = 1/card(j);
positive variable z1(j) bought period 1;
positive variable y1(j) sold   period 1;
z1.up(j)$(ord(j) = card(j)) = 0;
y1.up(j)$(ord(j) = card(j)) = 0;


positive variable x2(j,w1) holding period 2;
x2.lo(j,w1) = 0;
x2.l(j,w1) = 1/card(j);
positive variable z2(j,w1) bought period 2;
positive variable y2(j,w1) sold   period 2;
z2.up(j,w1)$(ord(j) = card(j)) = 0;
y2.up(j,w1)$(ord(j) = card(j)) = 0;

positive variable x3(j,w1,w2) holding period 3;
x3.lo(j,w1,w2) = 0;
x3.l(j,w1,w2) = 1/card(j);
positive variable z3(j,w1,w2) bought period 3;
positive variable y3(j,w1,w2) sold   period 3;
z3.up(j,w1,w2)$(ord(j) = card(j)) = 0;
y3.up(j,w1,w2)$(ord(j) = card(j)) = 0;


positive variable u(w1,w2,w3) upside wealth;
positive variable v(w1,w2,w3) downside wealth;
parameter sup upside slope;
sup = 1;
parameter sdo downside slope;
sdo = 10;


free variable utility portfolio utility;

equations
objfunsc         objective
ebalance1(j)      rebalancing period 1
cbalance1(j)      rebalancing period 1
ebalance2(j,w1)     rebalancing period 2
cbalance2(j,w1)     rebalancing period 2
ebalance3(j,w1,w2)  rebalancing period 3
cbalance3(j,w1,w2)  rebalancing period 3
termw(w1,w2,w3) terminal utility constraints
;

objfunsc..
utility =e= sup*sum((w1,w2,w3),u(w1,w2,w3))/nscenall - sdo*sum((w1,w2,w3),v(w1,w2,w3))/nscenall;

ebalance1(j)$(ord(j) ne card(j))..
x1(j) + y1(j) - z1(j) =e= x0(j);

cbalance1(j)$(ord(j) eq card(j))..
x1(j) + sum(jj, - trc(jj)*y1(jj) + trc(jj)*z1(jj)) =e= x0(j) + cf1;

ebalance2(j, w1)$(ord(j) ne card(j))..
-r1(j,w1)*x1(j) + x2(j,w1) + y2(j, w1) - z2(j,w1) =e= 0;

cbalance2(j, w1)$(ord(j) eq card(j))..
-r1(j,w1)*x1(j) + x2(j,w1) + sum(jj, - trc(jj)*y2(jj,w1) + trc(jj)*z2(jj,w1)) =e= cf2(w1);

ebalance3(j, w1,w2)$(ord(j) ne card(j))..
-r2(j,w2)*x2(j,w1) + x3(j,w1,w2) + y3(j,w1,w2) - z3(j,w1,w2) =e= 0;

cbalance3(j, w1,w2)$(ord(j) eq card(j))..
-r2(j,w2)*x2(j,w1) + x3(j,w1,w2) + sum(jj, - trc(jj)*y3(jj,w1,w2) + trc(jj)*z3(jj,w1,w2)) =e=  cf3(w1,w2);

termw(w1,w2,w3)..
-sum(j,r3(j,w3)*x3(j,w1,w2)) + u(w1,w2,w3) - v(w1,w2,w3) =e= - wtarget + cf4(w1,w2,w3);

option lp = cplex;

model portfoliomult /all/;
solve portfoliomult using lp maximizing utility;

display utility.l, x1.l, y1.l, z1.l, x2.l, y2.l, z2.l, x3.l, y3.l, z3.l;

File results / results.txt /;
put results;
put "Objective", utility.l /;
put "Stage 1 x1 y1 z1 "/;
loop(j,
  put x1.l(j), y1.l(j), z1.l(j)/
);
put "Stage 2 x2 y2 z2 "/;
loop(w1,
  put "scenario:"/;
  loop(j, 
    put x2.l(j, w1), y2.l(j, w1), z2.l(j, w1)/;
  );
);
put "Stage 3 x3 y3 z3 "/;
loop((w1, w2),
  put "scenario:"/;
  loop(j, 
    put x3.l(j, w1, w2), y3.l(j, w1, w2), z3.l(j, w1, w2)/;
  );
);
putclose;
