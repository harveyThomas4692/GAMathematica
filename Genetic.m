(* ::Package:: *)

(* ::Title:: *)
(*Genetic: Genetic evolution of bit sequences*)


(* ::Section:: *)
(*Start package*)


BeginPackage["Genetic`"];


(* ::Section:: *)
(*Documentation*)


(* ::Subsection:: *)
(*General help*)


Genetic::usage="Package Genetic: Genetic evolution of bit sequences.\n"<>
"\[FilledSmallSquare]  This package realises genetic evolution of bit sequences.\n"<>
"\[FilledSmallSquare]  Basic parameters of the environment are defined in Options[Genetic].\n"<>
"\[FilledSmallSquare]  Basic modules are listed under ?GeneticModules.\n"<>
"\[FilledSmallSquare]  The central module is the module EvolvePopulation, which genetically evolves an initial population.\n"<>
"\[FilledSmallSquare]  An individual in a population is specified by an association of the form <|\"Bits\"->list of bits,\"Fitness\"->fitness value,...|>.\n"<>
"\[FilledSmallSquare]  A population is a list of such associations and an evolution is given by a list of such population lists.\n"<>
"\[FilledSmallSquare]  For a list of global package options see ?GeneticOptions.";

GeneticModules::usage="Modules: \n"<>
"\[FilledSmallSquare]  CrossBits[bits1,bits2,opt] crosses pairs from a list of bit sequences.\n"<>
"\[FilledSmallSquare]  MutateBits[bitseq,opt] mutates a list of bit sequences.\n"<>
"\[FilledSmallSquare]  NextPopulation[pop,numgen,opt] determines the next generation.\n"<>
"\[FilledSmallSquare]  EvolvePopulation[poplst,numgen,opt] evolves the population poplst for numgen generations. If poplst is an empty list {}, then initial population is generated with InitModule.\n"<>
"\[FilledSmallSquare]  PlotFitness[poplst,opt] produces a fitness plot for an evolution of a population.\n"<>
"\[FilledSmallSquare]  PlotValues[poplst,opt]produces a plot of the various values contributing to the fitness versus generation.\n"<>
"\[FilledSmallSquare]  PlotTerminal[poplst,opt]plots fraction of terminal states versus generation.";

GeneticOptions::usage="Gloabl option defaults can be set with SetOptions[Genetic,{optionname->option,...}]. They are overidden by options specified in module calls. Option list:\n"<>
"\[FilledSmallSquare]  \"InitModule\"->name, to provide the name of the module (external to the package) which initialises an individual. The module has no input and must return an individual as an association (see ?Genetic).\n"<>
"\[FilledSmallSquare]  \"FitnessModule\"->name, to provide the name of the module (external to the package) which computes fitness. The module receives a list of bits (0 or 1) as input and must return an individual as an association (see ?Genetic).\n"<>
"\[FilledSmallSquare]  \"GenSize\"->integer, to specify the number of individuals in a generation (default 50).\n"<>
"\[FilledSmallSquare]  \"MutationRate\"->real or list, to specify the mutation rate (defaul 0.01). If a list {{mrate1,gen1},{mrate2,gen2},...} is given mrate1 is used for all generations gen<=gen1, mrate2 is used for all generations gen1 < gen <= gen2 etc.\n"<>
"\[FilledSmallSquare]  \"KeepFitest\"->boolean, if True fitest individual is kept for the next generation (and weakest deleted), if False (default) all parents are deleted.\n"<>
"\[FilledSmallSquare]  \"NumCuts\"->1 or 2, number of cuts performined when crossing two individuals (default 1). If input is invalid will be taken as 1.\n"<>
"\[FilledSmallSquare]  \"SelectionMethods\"->method, can be \"Roulette\" or \"Ranking\". Will take \"Ranking\" if input is invalid.\n"<>
"\[FilledSmallSquare]  \"Alpha\"->real, average number of times the fitest individual breeds (used for \"SelectionMethod\"->\"Roulette\").";


(* ::Subsection::Closed:: *)
(*Module help*)


CrossBits::usage="CrossBits[bits1,bits2,opt] crosses two bit lists.\n"<>
"\[FilledSmallSquare]  Performs one or two cuts (depending on option \"NumCuts\") and swaps segments between the two bit sequences.";

MutateBits::usage="MutateBits[bits,opt] mutates a list of bit sequences.\n"<>
"\[FilledSmallSquare] Bits in the list bits are flipped in accordance with the option \"MutationRate\"."; 

NextGeneration::usage="NextGeneration[poplst,opt] determines the next generation.\n"<>
"\[FilledSmallSquare] Input is a population, that is a list of associations, one for each individual.\n"<>
"\[FilledSmallSquare] A new population of the same size is determined by crossing and mutation and returned.";

EvolvePopulation::usage="EvolvePopulation[poplst,numgen,opt] further evolves the evolution poplst for numgen generations.\n"<>
"\[FilledSmallSquare] The input poplst is either the empty list {} in which case a random initial population is generated or an evolution (list of population lists) in which case evolution starts from the last population of the list.\n"<>
"\[FilledSmallSquare] The module NextGeneration is used to compute the numgen subsequent generations.\n"<>
"\[FilledSmallSquare] Output is the input list poplst with the numgen new generations appended.";

PlotFitness::usage="PlotFitness[poplst,opt] produces a fitness plot for an evolution of a population.\n"<>
"\[FilledSmallSquare] Options which can be given are \"PlotRange\"->{{xmin,xmax},{ymin,ymax}} (default \"Automatic\"), \"PointSize\"->value (default \"Automatic\") and \"ColorScheme\"->string (default \"TemperatureMap\") and \"Method\"->string with choices \"Contours\" (default),\"Histogram\"  or \"Points\".";

PlotValues::usage="PlotValues[poplst,opt] produces a plot of the various values contributing to the fitness versus generation.\n"<>
"\[FilledSmallSquare] Options are \"PlotRange\"->{xmin,xmax},{ymin,ymax}} (default \"Automatic\") and \"PlotLegends\"->list (default \"Automatic\".";

PlotTerminal::usage="PlotTerminal[poplst,opt]plots fraction of terminal states versus generation.\n"<>
"\[FilledSmallSquare] Options are \"PlotRange\"->{xmin,xmax},{ymin,ymax}} (default \"Automatic\")";


(* ::Section:: *)
(*Initialize*)


(* ::Subsection:: *)
(*Start-up messages*)


Begin["`Private`"]
Print[Style["Genetic: Genetic Evolution of Bit Sequences",Underlined,FontColor -> Blue,TextAlignment -> Center, FontSize -> 16,FontFamily->"Times"]];
Print[Style["S. A. Abel, A. Constantin, T. R. Harvey and A. Lukas ",Underlined,FontColor -> Black,TextAlignment -> Center, FontSize -> 14,FontFamily->"Times"]];
Print[Style["Last Update: 9 June 2022",Underlined,FontColor -> Black,TextAlignment -> Center, FontSize -> 14,FontFamily->"Times"]];
Print[Style["Execute \!\(\*
StyleBox[\"?\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"Genetic\",\nFontWeight->\"Bold\"]\) for help. Current Genetic options:",FontColor -> Black,TextAlignment -> Center, FontSize -> 14,FontFamily->"Times"]];


(* ::Subsection:: *)
(*Options*)


Options[Genetic]:={"InitModule"->MonadEnv`MonadBitInit,"FitnessModule"->MonadEnv`MonadFitness,
                   "GenSize"->50,"MutationRate"->0.01,"KeepFitest"->False,"NumCuts"->1,
                   "SelectionMethod"->"Roulette","Alpha"->3};
Genetic::OptionError = "Invalid Options given to function - `1`";
Genetic::ParameterError = "Invalid Paramter given to function - `1`";
Print[Style[Options[Genetic],"Output"]];                                 


(* ::Section:: *)
(*Modules*)


(* ::Subsection:: *)
(*CrossBits*)


CrossBits[bits1_List,bits2_List,opt___]:=Module[{gensize,bitlen,segment,range,newbits1,newbits2,numcuts,cutpos,cutrange,uni},

uni = Union[bits1,bits2];

If[Not[uni=={0,1}||uni=={1}||uni=={0}],
(Message[Genetic::ParameterError, "Invalid bit strings"]; $Failed);
Return[]];

(* options, basic definitions *)
bitlen=Min[Length[bits1],Length[bits2]];
numcuts="NumCuts"/.{opt}/.Options[Genetic];

If[numcuts!=1 && numcuts!=2,
(Message[Genetic::OptionError, "Only 1 or 2 cuts are allowed"]; $Failed);
Return[]];

(* perform cross for one cut *)
If[numcuts==1,
  cutpos=RandomInteger[{1,bitlen}];
  newbits1=Join[Take[bits2,cutpos],Take[bits1,{cutpos+1,bitlen}]];
  newbits2=Join[Take[bits1,cutpos],Take[bits2,{cutpos+1,bitlen}]],
(* perform cross for two cuts *)  
  cutrange=Sort[{RandomInteger[{1,bitlen}],RandomInteger[{1,bitlen}]}];
  newbits1=Join[Take[bits1,cutrange[[1]]-1],Take[bits2,cutrange],Take[bits1,{cutrange[[2]]+1,bitlen}]];
  newbits2=Join[Take[bits2,cutrange[[1]]-1],Take[bits1,cutrange],Take[bits2,{cutrange[[2]]+1,bitlen}]]
];  

{newbits1,newbits2}];


(* ::Subsection:: *)
(*MutateBits*)


MutateBits[bits_List,opt___]:=Module[{mrate,bitlen,pos,i,nummut,bitsnew,uni},

uni = Union[bits];
If[Not[uni=={0,1}||uni=={1}||uni=={0}],
(Message[Genetic::ParameterError, "Invalid bit-string"]; $Failed);
Return[]];

(* options *)
mrate="MutationRate"/.{opt}/.Options[Genetic]; 
bitlen=Length[bits];

If[mrate<0 || mrate>1 || Not[NumericQ[mrate]],
(Message[Genetic::OptionError, "MutationRate must be numeric and between 0 and 1"]; $Failed);
Return[]];

(* determine number of mutations *)
nummut=bitlen*mrate;
If[nummut<=0.5,nummut=RandomChoice[{nummut,1-nummut}->{1,0}]];
nummut=Round[nummut];

bitsnew=bits;
For[i=1,i<=nummut,i++,pos=RandomInteger[{1,bitlen}];bitsnew[[pos]]=Mod[bitsnew[[pos]]+1,2]];

bitsnew];


(* ::Subsection:: *)
(*NextGeneration*)


NextGeneration[pop_List,opt___]:=Module[
{poplen,newpop,keepfitest,fitnessmodule,alpham,fitest,unfitest,fmax,fmin,fmean,plst,labels,choices,
 bits1,bits2,newbits1,newbits2,pos,pop1,selectmeth,alpha},

(* options and basis definitions *)
poplen=Length[pop]; 
keepfitest="KeepFitest"/.{opt}/.Options[Genetic];
fitnessmodule="FitnessModule"/.Options[Genetic];
alpha=("Alpha"/.{opt}/.Options[Genetic]);
selectmeth="SelectionMethod"/.{opt}/.Options[Genetic];

If[Not[BooleanQ[keepfitest]],
(Message[Genetic::OptionError, "keepfitest must be a boole"]; $Failed);
Return[]];

If[alpha<0 || Not[NumericQ[alpha]],
(Message[Genetic::OptionError, "Alpha must be numeric and greater than 0"]; $Failed);
Return[]];

(* keep fitest individual if appropriate *)
fitest=First[MaximalBy[pop,#["Fitness"]&]];
unfitest=First[MinimalBy[pop,#["Fitness"]&]];
If[keepfitest,
  (* keep fitest in new population and delete unfitest *)
  newpop={fitest}; pos=Position[pop,unfitest][[1,1]]; pop1=Delete[pop,pos],
  (* otherwise, initialise new population as emty and keep current population as is *)
  newpop={}; pop1=pop;
];

(* compute selection propabilities *)
If[selectmeth=="Roulette",
  (* roulette wheel selection *)
  fmax=fitest["Fitness"]; fmin=unfitest["Fitness"];fmean=Mean[Map[#["Fitness"]&,pop1]];
  (* alpham=Min[alpha,0.95*(fmax-fmin)/(1.01*fmean-fmin)]-1; *) alpham=alpha;
  {labels,plst}=Transpose[Table[{i,alpham*(pop1[[i]]["Fitness"]-fmean)+fmax-fmean},{i,1,Length[pop1]}]],
  (* selection by fitness ranking *)
  {labels,plst}=Transpose[Table[{i,2*(1+(i-1)/(poplen-1)*(alpha-1))/poplen/(1+alpha)},{i,1,Length[pop1]}]];
  (* {labels,plst}=Transpose[Table[{i,a*i+b},{i,1,Length[pop1]}]]; *)
  pop1=SortBy[pop1,#["Fitness"]&];
];
plst=plst/.(X_/;X<0)->0;  (* negative selection weights are set to zero *)
If[Total[Abs[plst]]==0,plst=Table[1,{Length[pop1]}]];
        
(* determine individuals in next generation *)
Parallelize[While[Length[newpop]<poplen,
  choices=RandomChoice[plst->labels,2];
  bits1=pop1[[First[choices]]]["Bits"]; bits2=pop1[[Last[choices]]]["Bits"];
  {newbits1,newbits2}=CrossBits[bits1,bits2,opt];
  newbits1=MutateBits[newbits1,opt]; newbits2=MutateBits[newbits2,opt];
  newpop=Join[newpop,{fitnessmodule[newbits1,opt],fitnessmodule[newbits2,opt]}]];
];  

Take[newpop,poplen]];


(* ::Subsection:: *)
(*EvolvePopulation*)


EvolvePopulation[poplst_List,ngen_Integer,opt___]:=Module[{gensize,bitlen,initmodule,fitnessmodule,pop,i,mutrate,mrate,termfrac,lastpop},

(* options *)
gensize="GenSize"/.{opt}/.Options[Genetic];
initmodule="InitModule"/.Options[Genetic];
fitnessmodule="FitnessModule"/.Options[Genetic];
mutrate="MutationRate"/.{opt}/.Options[Genetic];

If[Not[IntegerQ[ngen]]||ngen<1,
(Message[Genetic::ParameterError, "number of generations must be a positive Integer."]; $Failed);
Return[]];

If[Not[IntegerQ[gensize]]||gensize<1,
(Message[Genetic::OptionError, "GenSize must be a positive Integer."]; $Failed);
Return[]];

If[mutrate<0 || mutrate>1 || Not[NumericQ[mutrate]],
(Message[Genetic::OptionError, "MutationRate must be numeric and between 0 and 1"]; $Failed);
Return[]];

(* initialise population if necessary *)
If[poplst=={},pop={Table[initmodule[opt],{gensize}]},pop=poplst];
bitlen=Length[First[Last[pop]]["Bits"]];

(* iterate over generations *)
termfrac=0.00;
Monitor[For[i=1,i<=ngen,i++,
  If[ListQ[mutrate],mrate=Select[mutrate,#[[2]]>=i &][[1,1]],mrate=mutrate];
  pop=Append[pop,NextGeneration[Last[pop],"MutationRate"->mrate,opt]];
  lastpop=Last[pop]; termfrac=Round[Count[Map[#["Terminal"]&,lastpop],True]/Length[lastpop],0.01];
],{ProgressIndicator[i,{0,ngen}],termfrac}];
 
pop];


(* ::Subsection:: *)
(*PlotFitness*)


Options[PlotFitness]:={"PlotRange"->"Automatic","ColorScheme"->"Rainbow","PointSize"->"Automatic","Method"->"Points",
                       "Contours"->15};

PlotFitness[poplst_List,opt___]:=Module[
{fitness,fitpts,fitnessplot,fitmean,gensize,fmin,fmax,df,ngen,plotrange,ff,mean,i,j,
 dens,densf,plotstyle,cf,colorscheme,pointsize,f,fitpts1,method,ncontours},
 
(* options *)
plotrange="PlotRange"/.{opt}/.Options[PlotFitness];
colorscheme="ColorScheme"/.{opt}/.Options[PlotFitness];
pointsize="PointSize"/.{opt}/.Options[PlotFitness];
method="Method"/.{opt}/.Options[PlotFitness];
ncontours="Contours"/.{opt}/.Options[PlotFitness];
  
(* extract fitness values *)
fitness=Map[#["Fitness"]&,poplst,{2}]; ff=Flatten[fitness];
fmin=Min[ff];fmax=Max[ff]; df=fmax-fmin; mean=Mean[ff];
{ngen,gensize}=Dimensions[fitness];
fitmean=Map[Mean,fitness];
fitpts=Flatten[Table[Map[List[i,#]&,fitness[[i]]],{i,1,Length[fitness]}],1];
fitpts1=Map[List,fitpts];

(* automatic options settings *)
If[plotrange=="Automatic",plotrange={{0,ngen},{mean-0.15*df,fmax+0.01*df}}];
If[pointsize=="Automatic",pointsize=0.8/gensize];

(* create fitness plot *)
If[method=="Points",
 fitnessplot=Show[ListPlot[fitpts,Frame->True,PlotRange->plotrange,PlotStyle->{Blue,PointSize[pointsize]},
                  GridLines->Automatic,FrameLabel->{"generation","fitness"}],
                  ListLinePlot[fitmean,PlotRange->plotrange,PlotStyle->{Red,Thickness[0.004]}],ImageSize->Medium],
 If[method=="Histogram",
 fitnessplot=Show[Histogram3D[fitpts,ColorFunction->colorscheme],ImageSize->Medium],
 fitnessplot=Show[ContourPlot[f[{x,y}],{x,plotrange[[1,1]],plotrange[[1,2]]},{y,plotrange[[2,1]],plotrange[[2,2]]},Frame->True,
                  GridLines->Automatic,FrameLabel->{"generation","fitness"},ColorFunction->colorscheme,Contours->ncontours],
                  ListLinePlot[fitmean,PlotRange->plotrange,PlotStyle->{Black,Thickness[0.007]}],ImageSize->Medium]]];

fitnessplot];


(* ::Subsection:: *)
(*PlotValues*)


Options[PlotValues]:={"PlotRange"->"Automatic","PlotLegends"->"Automatic"};

PlotValues[poplst_List,opt___]:=Module[
{valuelst,meanvalues,vmin,vmax,nvalues,ngen,plotrange,valueplot,plotlegends},

(* options *)
plotrange="PlotRange"/.{opt}/.Options[PlotValues];
plotlegends="PlotLegends"/.{opt}/.Options[PlotValues];

(* extract values *)
valuelst=Map[#["ValueLst"]&,poplst,{2}]; meanvalues=Transpose[Map[Mean,valuelst]];
{nvalues,ngen}=Dimensions[meanvalues];

(* automatic option settings *)
vmin=Min[Flatten[meanvalues]];vmax=Max[Flatten[meanvalues]];
If[plotrange=="Automatic",plotrange={{0,ngen},{vmin,vmax}}];
If[plotlegends=="Automatic",plotlegends=Table[ToString[i],{i,1,nvalues}]];

(* produce plot *)
valueplot=Show[ListLinePlot[meanvalues,PlotRange->plotrange,GridLines->Automatic,Frame->True,
                                  PlotLegends->plotlegends,FrameLabel->{"generation","values"}],ImageSize->Medium];

valueplot];


(* ::Subsection:: *)
(*PlotTerminal*)


Options[PlotTerminal]:={"PlotRange"->"Automatic"};

PlotTerminal[poplst_List,opt___]:=Module[{termlst,gensize,ngen,termfrac,plotrange,termplot,max},

(* options *)
plotrange="PlotRange"/.{opt}/.Options[PlotTerminal];

(* extract number of terminal states *)
termlst=Map[#["Terminal"]&,poplst,{2}];
{ngen,gensize}=Dimensions[termlst];
termfrac=Map[Count[#,True]&,termlst]/gensize; max=Max[termfrac];

(* automatic option setting *)
If[plotrange=="Automatic",plotrange={{0,ngen},{0,1.2*max}}];

(* produce plot *)
termplot=Show[ListLinePlot[termfrac,PlotRange->plotrange,GridLines->Automatic,Frame->True,
                              FrameLabel->{"generation","terminal fraction"},PlotStyle->{Red,Thickness[0.004]}],ImageSize->Medium];

termplot];


(* ::Section:: *)
(*End Package*)


End[]
EndPackage[]
