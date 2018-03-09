* MS&E 348 - Endowment Fund Management
* include file asset.inc

Set i "assets"	
/stock
cbond
gbond
alter
cash/;

Alias (i, ii);

Parameter rbar(i) "mean returns"
/
stock	0.06044
cbond	-0.03083
gbond	-0.00891
alter	0.054306
cash	-0.00267
/;

Parameter rstd(i) "standard deviation of returns"
/
stock	0.15326
cbond	0.14756
gbond	0.09429
alter	0.04826
cash	0.00737
/;

Table chol(i, ii) "covariance matrix of returns"
	stock	cbond	gbond	alter	cash
stock	0.15326	0.04216	-0.0074 0.00097	0.03874
cbond	0.00000	0.20000	0.08114	0.00550	0.00551
gbond	0.00000	0.00000	0.04746	0.00061	-0.00863
alter	0.00000	0.00000	0.00000	0.00476	-0.00341
cash	0.00000	0.00000	0.00000	0.00000	0.02668;