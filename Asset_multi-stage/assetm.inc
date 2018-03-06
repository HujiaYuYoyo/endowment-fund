* MSandE348 Prof. Gerd Infanger
* include file asset.inc

set j assets
/
Stocks
CorpBonds
GvmtBonds
Cash
/;

alias (j, jj);

parameter rbar(j) mean returns
/
Stocks      10.5
CorpBonds    8.5
GvmtBonds    7
Cash         4
/;


parameter rstd(j) standard deviation of returns
/
Stocks      15.5
CorpBonds   11.9
GvmtBonds    8.2
Cash         0.1
/;

Table rcorr(j, jj) Correlatrion matrix of returns
                      Stocks      CorpBonds       GvmtBonds      Cash
Stocks                 1.0         0.394            0.326        0.0
CorpBonds                          1.0              0.966        0.0
GvmtBonds                                           1.0          0.0
Cash                                                             1.0;

rcorr(j, jj)$(ord(jj) lt ord(j)) = rcorr(jj, j);


Table chol(j, jj)
              Stocks   CorpBonds   GvmtBonds        Cash
Stocks        15.500       4.689       2.673       0.000
CorpBonds      0.000      10.937       7.472       0.000
GvmtBonds      0.000       0.000       2.063       0.000
Cash           0.000       0.000       0.000       0.100
;
