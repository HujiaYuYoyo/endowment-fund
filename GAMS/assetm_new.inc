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
stock	0.059948
cbond	0.052363
gbond	0.046906
alter	0.060571
cash	-0.00477
/;

Parameter rstd(i) "standard deviation of returns"
/
stock	0.15041
cbond	0.05878
gbond	0.06227
alter	0.04840
cash	0.01568
/;

Table chol(i, ii) "covariance matrix of returns"
	stock	cbond	gbond	alter	cash
stock	0.15041	0.02054	-0.0172 0.03784	-0.0011
cbond	0.00000	0.05508	0.03922	0.01047	0.00126
gbond	0.00000	0.00000	0.04518	-0.0123	0.00022
alter	0.00000	0.00000	0.00000	0.02548	-0.00245
cash	0.00000	0.00000	0.00000	0.00000	0.01539;