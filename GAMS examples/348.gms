$TITLE Mean-variance model.

SET Assets;

ALIAS(Assets,i,j);

PARAMETERS
         RiskFreeRate
         ExpectedReturns(i)  Expected returns
         VarCov(i,j)         Variance-Covariance matrix ;

* Read from Estimate.gdx the data needed to run the mean-variance model

$GDXIN Estimate
$LOAD Assets=subset RiskFreeRate=MeanRiskFreeReturn VarCov ExpectedReturns
$GDXIN

* Risk attitude: 0 is risk-neutral, 1 is very risk-averse.;

SCALAR
    lambda Risk attitude;


POSITIVE VARIABLES
    x(i) Holdings of assets;

VARIABLES
    PortVariance Portfolio variance
    PortReturn   Portfolio return
    z            Objective function value;

EQUATIONS
    ReturnDef    Equation defining the portfolio return
    VarDef       Equation defining the portfolio variance
    NormalCon    Equation defining the normalization contraint
    ObjDef       Objective function definition;

ReturnDef ..   PortReturn    =e= SUM(i, ExpectedReturns(i)*x(i));

VarDef    ..   PortVariance  =e= SUM((i,j), x(i)*VarCov(i,j)*x(j));

NormalCon ..   SUM(i, x(i))  =e= 1;

ObjDef    ..   z             =e= (1-lambda) * PortReturn - lambda * PortVariance;

MODEL MeanVar 'PFO Model 3.2.3' /ReturnDef,VarDef,NormalCon,ObjDef/;

* Define a file on the disk and associate it to the file handle FrontierHandle.
* By default, the file will be written in the current directory.

* Define a file on the disk and associate it to the file handle FrontierHandle.
* By default, the file will be written in the current directory.

FILE FrontierHandle /"MeanVarianceFrontier.csv"/;

* Just add some options to appropriately format the output.
* We will write a comma separeted value (CSV) file
* which can be easily read from any spreadsheet (see pag. 137 of the
* GAMS User's Guide).
* Also, enalarge the page width to be sure that the portfolio holdings
* fit in a row.

FrontierHandle.pc = 5;
FrontierHandle.pw = 1048;

* Assign the output stream to the file handle "Frontier"

PUT FrontierHandle;

* Write the heading

PUT "Lambda","z","Variance","ExpReturn";

LOOP (i, PUT i.tl);

PUT /;

FOR  (lambda = 0 TO 1 BY 0.1,

   SOLVE MeanVar MAXIMIZING z USING nlp;

   PUT lambda:6:5, z.l:6:5, PortVariance.l:6:5, PortReturn.l:6:5;

   LOOP (i, PUT x.l(i):6:5 );

   PUT /;
)

* Close file

PUTCLOSE;

* Output directly to an Excel file through the GDX utility

SET FrontierPoints / PP_0 * PP_10 /

ALIAS (FrontierPoints,p);

PARAMETERS
         RiskWeight(p)           Investors risk attitude parameter
         MinimumVariance(p)      Optimal level of portfolio variance
         PortfolioReturn(p)      Portfolio return
         OptimalAllocation(p,i)  Optimal asset allocation
         SolverStatus(p,*)       Status of the solver
         SummaryReport(*,*)      Summary report;

* The risk weight, lambda,  has to range in the interval [0,1]

RiskWeight(p) = (ORD(p)-1)/(CARD(p)-1);

DISPLAY RiskWeight;
LOOP(p,
   lambda = RiskWeight(p);
   SOLVE MeanVar MAXIMIZING z USING NLP;
   MeanVar.SOLPRINT = 2;
   MinimumVariance(p)= PortVariance.l;
   PortfolioReturn(p)  = PortReturn.l;
   OptimalAllocation(p,i)     = x.l(i);
   SolverStatus(p,'solvestat') = MeanVar.solvestat;
   SolverStatus(p,'modelstat') = MeanVar.modelstat;
   SolverStatus(p,'objective') = MeanVar.objval
);

* Store results by rows

SummaryReport(i,p) = OptimalAllocation(p,i);
SummaryReport('Variance',p) = MinimumVariance(p);
SummaryReport('Return',p) = PortfolioReturn(p);
SummaryReport('Lambda',p)   = RiskWeight(p);

DISPLAY SummaryReport,SolverStatus;

* Write SummaryReport into an Excel file

EXECUTE_UNLOAD 'Summary.gdx', SummaryReport;
EXECUTE 'gdxxrw.exe Summary.gdx O=MeanVarianceFrontier.xls par=SummaryReport rng=sheet1!a1 1>%system.nullfile% 2>%system.nullfile%' ;