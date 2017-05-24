(* ::Package:: *)

(* :Title: Trigonometria *)

(* :Context: Trigonometria *)
(* :Author: Gianluca Lutero, Filippo Soncini, Adele Valerii, Sara Gattari  *)
(* :Summary: Insieme di illustrazioni ed esercizzi interattivi sulla trigonometria *)
(* :Package Version: 11.0.1.0 *)
(* :Mathematica Version: 11.0.1.0 *)

BeginPackage["Trigonometria`"]

(* Funzioni *)
angolo::usage = "Calcola i due angoli Subscript[\[Theta], 1], Subscript[\[Theta], 2] necessari\n
				 per poter disegnare una arco di circonferenza tramite Disk[{x,y},{Subscript[\[Theta], 1],Subscript[\[Theta], 2]}] "; 

bottonesen::usage = "Funzione che genera il bottone che richiama la fiunzione grafiseno[]";

bottonecos::usage = "Funzione che genera il bottone che richiama la fiunzione graficocoseno[]";

bottonetan::usage = "Funzione che genera il bottone che richiama la fiunzione graficotangente[]";

bottonepitagora::usage = "Funzione che genera il bottone che richiama la fiunzione bottonepitagora[]";

bottonecalcolatrice::usage = "Funzione che genera il bottone che richiama la fiunzione bottonecalcolatrice[]";

grafiseno::usage = "Illustrazione del seno e della sua funzione";

graficocoseno::usage = "Illustrazione del coseno e della sua funzione";

graficotangente::usage = "Illustrazione della tangente e della sua funzione";

defsencos::usage = "Illustrazione del seno e del coseno sulla circonferenza unitaria";

rapporti::usage = "Illustrazione dei rapporti tra seno e del coseno";

triangolorett::usage = "Illustrazione del un triangolo rettangolo formato da seno e coseno";

tangent::usage = "Illustrazione della tangente sulla circonferenza unitaria";

definizionetangente::usage = "Illustrazione della tangente sulla circonferenza unitaria con rapporti";

angolinoti30::usage = "Illustrazione angoli noti multipli di 30\[Degree]";

angolinoti45::usage = "Illustrazione angoli noti multipli di 45\[Degree]";

teoremacorda::usage = "Illustrazione del teorema della corda";

teoremacorda2::usage = "Illustrazione del teorema della corda interattivo pt.1";

teoremacorda3::usage = "Illustrazione del teorema della corda interattivo pt.2";

teoremaseni::usage = "Illustrazione del teorema dei seni";

teoremacoseno::usage = "Illustrazione del teorema dei coseni pt.1";

teoremacoseno2::usage = "Illustrazione del teorema dei coseni pt.2";

pitagora::usage = "Illustrazione del teorema di pitagora con testo";

EsercizioEsempio::usage = "Stampa un'esercizio d'esempio per calcolare Sen(\[Alpha])";

Esercizio1::usage= "Esercizio su seno coseno tangente";

Esercizio2::usage = "Esercizio a risposta multipla";

Esercizio3::usage = "Esercizio: calcolo lato triangolo - Teorema della corda ";

Esercizio4::usage= "Esercizio: trovare lato di un triangolo a risposta multipla";

Esercizio5::usage = "Esercizio: trovare il lato C usando la relazione \!\(\*FractionBox[\(\(\\\ \)\(A\)\), \(sen \((\[Alpha])\)\)]\) =\!\(\*FractionBox[\(\(\\\ \)\(B\)\), \(sen \((\[Beta])\)\)]\)";

Esercizio6::usage="Esercizio: teorema dei seni risposta multipla";

Esercizio7::usage = "Esercizio: trovare il lato C con il teorema del coseno";

Esercizio8::usage = "Esercizio: trovare cos(\[Gamma]) usando il teorema del coseno";

Esercizio9::usage = "Esercizio: campanile";

Esercizio10::usage = "Esercizio: scivolo";
CheckAnswer::usage = "Modulo che compara i parametri answer_ e correct_ passati in input.\n
                      Il risultato della valutazione \[EGrave] \[Checkmark] se i parametri sono uguali\n
                      X altrimenti ";

Calcolatrice::usage = "Calcolatrice semplice per il calcolo di funzioni trigonometriche";

TPitagora::usage = "Suggerimento Teorema di pitagora";

ClearAll["Global`*"]


(* FUNZIONE CALCOLO ANGOLO *)
(* Angolo calcola i due angoli Subscript[\[Theta], 1], Subscript[\[Theta], 2] necessari per poter disegnare una arco di circonferenza tramite Disk[{x,y},{Subscript[\[Theta], 1],Subscript[\[Theta], 2]}] *)
angolo[p1_,p2_,p3_]:=Module[{anga},
anga={If[(p3[[1]]-p1[[1]])!=0, ArcTan[(p3[[2]] -p1[[2]])/(p3[[1]]-p1[[1]])] ,0] ,
If[(p3[[1]]-p2[[1]])!=0, ArcTan[(p3[[2]] -p2[[2]])/(p3[[1]]-p2[[1]])],0] }
];



(* PUNTI GENERICI GLOBALI*)
pa = {Cos[(Pi/2)+0.3], Sin[(Pi/2)+0.3]};
pb = {Cos[-0.2], Sin[-0.2]};
pc = {Cos[Pi +0.5], Sin[Pi +0.5]};

pa2 = {Cos[Pi/2], Sin[(Pi/2)]}
pb2 = {Cos[-0.2], Sin[-0.2]}
pc2 = {Cos[3Pi/2], Sin[3Pi/2]}


(* GENERA BOTTONE *)
(* Funzione che genera il bottone che richiama la fiunzione grafiseno[] *)
bottonesen[]:=
Button["Funzione Seno",MessageDialog[  graficoseno[] ,WindowSize->All,Editable->False]]


(* GENERA BOTTONE *)
(* Funzione che genera il bottone che richiama la fiunzione graficocoseno[] *)
bottonecos[]:=
Button["Funzione Coseno",MessageDialog[  graficocoseno[] ,WindowSize->All,Editable->False]]


(* GENERA BOTTONE *)
(* Funzione che genera il bottone che richiama la fiunzione graficotangente[] *)
bottonetan[]:=
Button["Funzione Tangente",MessageDialog[  graficotangente[] ,WindowSize->All,Editable->False]]


(* GENERA BOTTONE *)
(* Funzione che genera il bottone che richiama la fiunzione pitagora[] *)
bottonepitagora[]:=
Button["Teorema di Pitagora",MessageDialog[  pitagora[] ,WindowSize->All,Editable->False]]


(* GENERA BOTTONE *)
(* Funzione che genera il bottone che richiama la fiunzione Calcolatrice[] *)
bottonecalcolatrice[]:=
Button["Calcolatrice",MessageDialog[  Calcolatrice[] ,WindowSize->All,Editable->False]]


(* Grafico Seno *)
graficoseno[] := 
Manipulate[
(* genero disegno *)
Module[{anglegraph,maingraph},
anglegraph[th_]:=Show[
Graphics[{
{Lighter[Gray,0.5],Circle[{0,0},1]},
{Darker[Green,0.2],Thick,Circle[{0,0},1,{0,th}]},

{Lighter[Gray,0.5],Line[{{0,0},{Cos[th],Sin[th]}}]},
{Red,Thick,Line[{{Cos[th],0},{Cos[th],Sin[th]}}]},
(* yp *)
{Black,Disk[{0, Sin[th]},0.02]},

(* sin *)
{Red,Thick,Dashing[Medium],Line[{{0,0},{0,Sin[th]}}]},

(* retta punto *)
{Lighter[Gray,0.5],Line[{{-6Cos[th],-6Sin[th]},{6Cos[th],6Sin[th]}}]},

(* angolo *)
{Opacity[0.2],Darker[Green,0.3],Thick,Disk[{0,0},0.3,{0,th}]},
{Darker[Green,0.3],Circle[{0,0},0.3,{0,th}]},

(* linea tratteggiata per cos *)
{Lighter[Gray,0.5],Dashing[Medium],Line[{{Cos[th]-3,Sin[th]},{3,Sin[th]}}]},

(* linea tratteggiata per sin *)
{Lighter[Gray,0.5],Dashing[Medium],Line[{{Cos[th],Sin[th]-3},{Cos[th],3}}]},

(* TESTO *)
Text["Yp",{0.1,Sin[th]+0.1}],
Text["P",{Cos[th] +0.1,Sin[th]+0.1}],
Text[Style["\[Theta]",Darker[Green,0.3]],{0.2,0.1}],
Rotate[Text[Style["Sin(\[Theta])",Red],{-0.1,Sin[th]/2}],90\[Degree]]

}],
PlotRange->1,ImageSize->400,BaseStyle->{15},Axes->True,PlotRange->{{-1,1},{-1,1}},PlotRangePadding->0.25];

(* genero grafico *)
maingraph[th_]:=Module[{},
	Show[Plot[{Sin[x]},{x,0.0001,th},PlotRange->{{0,2Pi},{-1,1}},ImageSize->650,PlotRangePadding->{0,0.25},ImagePadding->{{30,12},{0,0}},PlotRangeClipping->False,PlotStyle->Darker[Red,0.6],
	Ticks->{Table[{n Pi/4,n Pi/4},{n,0,8}],Table[n,{n,-1,1,1/2}]},
	GridLines->{Table[{n Pi/4,Lighter[Gray,0.7]},{n,-2,8}],Table[{n,Lighter[Gray,0.7]},{n,-1,1,1/2}]},ImageSize->{Automatic,145}],
	Graphics[{
		{Darker[Green,0.2],Thick,Line[{{0,0},{th,0}}]},
		{Red,Thick,Line[{{th,0},{th,Sin[th]}}]}
	}],
	AspectRatio->Automatic,BaseStyle->{12}]];

DynamicModule[{pt={Cos[ptctrl],Sin[ptctrl]},pt2={ptctrl,0}},
Labeled[Grid[{

{LocatorPane[Dynamic[pt,
	{(pt={Cos[pt2[[1]]],Sin[pt2[[1]]]})&,
	(pt=Normalize[#];pt2={If[pt2=={2Pi,0},2Pi,Mod[ArcTan[#[[1]],#[[2]]],2 Pi]],0})&,
	(pt=Normalize[#];ptctrl=pt2[[1]])&}],
Dynamic[anglegraph[If[pt2=={2Pi,0},2Pi,Mod[ArcTan[pt[[1]],pt[[2]]],2 Pi]]]]],

LocatorPane[Dynamic[pt2,
	{(pt2={If[pt2=={2Pi,0},2Pi,Mod[ArcTan[pt[[1]],pt[[2]]],2 Pi]],0})&,
	(pt2={#[[1]],0};pt={Cos[#[[1]]],Sin[#[[1]]]})&,
	(pt2={#[[1]],0};ptctrl=#[[1]])&}],
Dynamic[maingraph[If[pt2=={2Pi,0},2Pi,Mod[ArcTan[pt[[1]],pt[[2]]],2 Pi]]]]]}},Spacings->0],{Row[{Style["Funzione ","Label",22,Gray],Text@Style["Seno",Red,22]}],
Style["",10,Lighter[Gray,0.7],"Label"]},{{Top,Center},{Bottom,Right}}]]
],
{{ptctrl,Pi/6,"angle"},0,2Pi},TrackedSymbols:>{ptctrl}]


graficotangente[]:=
Manipulate[
(* genero disegno *)
Module[{anglegraph,maingraph},
anglegraph[th_]:=Show[
Graphics[{
{Lighter[Gray,0.5],Circle[{0,0},1]},
{Darker[Green,0.2],Thick,Circle[{0,0},1,{0,th}]},
{Lighter[Gray,0.5],Line[{{-6Cos[th],-6Sin[th]},{6Cos[th],6Sin[th]}}]},

{Lighter[Gray,0.5],Line[{{0,0},{Cos[th],Sin[th]}}]},
(* punti *)
{Black,Disk[{1, Tan[th]},0.02]},
{Black,Disk[{Cos[th], Sin[th]},0.02]},
{Black,Disk[{0, Tan[th]},0.02]},
{Black,Disk[{1,0},0.02]},

(* tangente punto *)
{Lighter[Gray,0.5],Line[{{-6Cos[th],-6Sin[th]},{6Cos[th],6Sin[th]}}]},

(* angolo *)
{Opacity[0.2],Darker[Green,0.3],Thick,Disk[{0,0},0.3,{0,th}]},
{Darker[Green,0.3],Circle[{0,0},0.3,{0,th}]},

(* linea tratteggiata per Tan *)
{Lighter[Gray,0.5],Dashing[Medium],Line[{{1,3},{1,Tan[th]-3}}]},

(* linea tratteggiata per Tan 2*)
{Lighter[Gray,0.5],Dashing[Medium],Line[{{-3,Tan[th]},{3,Tan[th]}}]},

(* TESTO *)
Text["P",{Cos[th] +0.1,Sin[th]+0.1}],
Text["T",{1.1,Tan[th]+0.1}],
Text["Xt",{1.1,0.1}],
Text["Yt",{0.1,Tan[th]+0.1}],
Text[Style["\[Theta]",Darker[Green,0.3]],{0.2,0.1}],
Rotate[Text[Style["Tan(\[Theta])",Orange],{1.1,Tan[th]/2}],90\[Degree]],

(* TAN *)
(* tan *)  {Orange,Thick,Line[{{1,0},{1,Tan[th]}}]},
		{Orange,Thickness[0.008],Dashing[Medium],Line[{{0,0},{0,Tan[th]}}]}

}],
PlotRange->1,ImageSize->400,BaseStyle->{15},Axes->True,PlotRange->{{-1,1},{-1,1}},PlotRangePadding->0.25];

(* genero grafico *)
maingraph[th_]:=Module[{},
	Show[Plot[{Tan[x]},{x,0.0001,th},PlotRange->{{0,2Pi},{-2.2,2.2}},ImageSize->650,PlotRangePadding->{0,0},ImagePadding->{{30,12},{0,0}},PlotRangeClipping->False,PlotStyle->Darker[Orange,0.5],
	Ticks->{Table[{n Pi/4,n Pi/4},{n,0,8}],Table[n,{n,-2,2,1/2}]},
	GridLines->{Table[{n Pi/4,Lighter[Gray,0.7]},{n,-2,8}],Table[{n,Lighter[Gray,0.7]},{n,-2,2,1/2}]},ImageSize->{Automatic,145}],
	Graphics[{
		{Darker[Green,0.2],Thick,Line[{{0,0},{th,0}}]},
		{Orange,Thick,Line[{{th,0},{th,Tan[th]}}]}
	}],
	AspectRatio->Automatic,BaseStyle->{12}]];

DynamicModule[{pt={Cos[ptctrl],Sin[ptctrl]},pt2={ptctrl,0}},
Labeled[Grid[{

{LocatorPane[Dynamic[pt,
	{(pt={Cos[pt2[[1]]],Sin[pt2[[1]]]})&,
	(pt=Normalize[#];pt2={If[pt2=={2Pi,0},2Pi,Mod[ArcTan[#[[1]],#[[2]]],2 Pi]],0})&,
	(pt=Normalize[#];ptctrl=pt2[[1]])&}],
Dynamic[anglegraph[If[pt2=={2Pi,0},2Pi,Mod[ArcTan[pt[[1]],pt[[2]]],2 Pi]]]]],

LocatorPane[Dynamic[pt2,
	{(pt2={If[pt2=={2Pi,0},2Pi,Mod[ArcTan[pt[[1]],pt[[2]]],2 Pi]],0})&,
	(pt2={#[[1]],0};pt={Cos[#[[1]]],Sin[#[[1]]]})&,
	(pt2={#[[1]],0};ptctrl=#[[1]])&}],
Dynamic[maingraph[If[pt2=={2Pi,0},2Pi,Mod[ArcTan[pt[[1]],pt[[2]]],2 Pi]]]]]}},Spacings->0],{Row[{Style["Funzione ","Label",22,Gray],Text@Style["Tangente",Orange,22]}],
Style["",10,Lighter[Gray,0.7],"Label"]},{{Top,Center},{Bottom,Right}}]]
],
{{ptctrl,Pi/6,"angle"},0,2Pi},TrackedSymbols:>{ptctrl}]


(* Grafico Coseno *)
graficocoseno[] := 
Manipulate[
(* genero disegno *)
Module[{anglegraph,maingraph},
anglegraph[th_]:=Show[
Graphics[{
{Lighter[Gray,0.5],Circle[{0,0},1]},
{Darker[Green,0.2],Thick,Circle[{0,0},1,{0,th}]},

{Lighter[Gray,0.5],Line[{{0,0},{Cos[th],Sin[th]}}]},
{Lighter[Gray,0.5],Line[{{Cos[th],0},{Cos[th],Sin[th]}}]},

(* yp *)
{Black,Disk[{Cos[th], 0},0.02]},

(* cos *)
{Blue,Thick,Dashing[Medium],Line[{{0,Sin[th]},{Cos[th],Sin[th]}}]},
{Blue,Thick,Line[{{0,0},{Cos[th],0}}]},

(* retta punto *) 
{Lighter[Gray,0.5],Line[{{-6Cos[th],-6Sin[th]},{6Cos[th],6Sin[th]}}]},

(* angolo *)
{Opacity[0.2],Darker[Green,0.3],Thick,Disk[{0,0},0.3,{0,th}]},
{Darker[Green,0.3],Circle[{0,0},0.3,{0,th}]},
  
(* linea tratteggiata per cos *)
{Lighter[Gray,0.5],Dashing[Medium],Line[{{Cos[th]-3,Sin[th]},{3,Sin[th]}}]},

(* linea tratteggiata per sin *)
{Lighter[Gray,0.5],Dashing[Medium],Line[{{Cos[th],Sin[th]-3},{Cos[th],3}}]},

(* testo *)
Text["Xp",{Cos[th]+0.1,0.1}],
Text["P",{Cos[th] +0.1,Sin[th]+0.1}],
Text[Style["\[Theta]",Darker[Green,0.3]],{0.2,0.1}],
Text[Style["Cos(\[Theta])",Blue],{Cos[th]/2,-0.1}]

}],
PlotRange->1,ImageSize->400,BaseStyle->{15},Axes->True,PlotRange->{{-1,1},{-1,1}},PlotRangePadding->0.25];

(*genero grafico*)
maingraph[th_]:=Module[{},
	Show[
		Plot[{Cos[x]},{x,0.0001,th},PlotRange->{{0,2Pi},{-1,1}},ImageSize->650,PlotRangePadding->{0,0.25},ImagePadding->{{30,12},{0,0}},PlotRangeClipping->False,PlotStyle->Darker[Blue,0.9],
		Ticks->{Table[{n Pi/4,n Pi/4},{n,0,8}],Table[n,{n,-1,1,1/2}]},
		GridLines->{Table[{n Pi/4,Lighter[Gray,0.7]},{n,-2,8}],Table[{n,Lighter[Gray,0.7]},{n,-1,1,1/2}]},ImageSize->{Automatic,145}],
		Graphics[{
			{Darker[Green,0.2],Thick,Line[{{0,0},{th,0}}]},
			{Blue,Thick,Line[{{th,0},{th,Cos[th]}}]}
	}],
	AspectRatio->Automatic,BaseStyle->{12}]];


DynamicModule[{pt={Cos[ptctrl],Sin[ptctrl]},pt2={ptctrl,0}},
Labeled[Grid[{

{LocatorPane[Dynamic[pt,
	{(pt={Cos[pt2[[1]]],Sin[pt2[[1]]]})&,
	(pt=Normalize[#];pt2={If[pt2=={2Pi,0},2Pi,Mod[ArcTan[#[[1]],#[[2]]],2 Pi]],0})&,
	(pt=Normalize[#];ptctrl=pt2[[1]])&}],
Dynamic[anglegraph[If[pt2=={2Pi,0},2Pi,Mod[ArcTan[pt[[1]],pt[[2]]],2 Pi]]]]],

LocatorPane[Dynamic[pt2,
	{(pt2={If[pt2=={2Pi,0},2Pi,Mod[ArcTan[pt[[1]],pt[[2]]],2 Pi]],0})&,
	(pt2={#[[1]],0};pt={Cos[#[[1]]],Sin[#[[1]]]})&,
	(pt2={#[[1]],0};ptctrl=#[[1]])&}],
Dynamic[maingraph[If[pt2=={2Pi,0},2Pi,Mod[ArcTan[pt[[1]],pt[[2]]],2 Pi]]]]]}},Spacings->0],{Row[{Style["Funzione ","Label",22,Gray],Text@Style["Coseno",Blue,22]}],
Style["",10,Lighter[Gray,0.7],"Label"]},{{Top,Center},{Bottom,Right}}]]
],
{{ptctrl,Pi/6,"angle"},0,2Pi},TrackedSymbols:>{ptctrl}]


(* Definizione Seno Coseno *)
defsencos[] :=
Manipulate[
Module[{anglegraph,maingraph},
anglegraph[th_]:=Show[
Graphics[{
{Lighter[Gray,0.5],Circle[{0,0},1]},
{Lighter[Gray,0.5],Line[{{0,0},{Cos[th],Sin[th]}}]},

(* punti *)
{Black,Disk[{Cos[th], 0},0.02]},
{Black,Disk[{0, Sin[th]},0.02]},

(* tangente punto *) 
{Lighter[Gray,0.5],Line[{{-6Cos[th],-6Sin[th]},{6Cos[th],6Sin[th]}}]},

(* angolo *) 
{Opacity[0.2],Darker[Green,0.3],Thick,Disk[{0,0},0.3,{0,th}]},
{Darker[Green,0.3],Circle[{0,0},0.3,{0,th}]},
			  
(* linea tratteggiata per cos *)
{Lighter[Gray,0.5],Dashing[Medium],Line[{{Cos[th]-3,Sin[th]},{3,Sin[th]}}]},

(* linea tratteggiata per sin *){
Lighter[Gray,0.5],Dashing[Medium],Line[{{Cos[th],Sin[th]-3},{Cos[th],3}}]},

(* TESTO *)
Text["Xp",{Cos[th]+0.1,0.1}],
Text["Yp",{0.1,Sin[th]+0.1}],
Text["P",{Cos[th] +0.1,Sin[th]+0.1}],
Text[Style["\[Theta]",Darker[Green,0.3]],{0.2,0.1}],

Text[Style["Cos(\[Theta])",Blue],{Cos[th]/2,-0.1}],
Rotate[Text[Style["Sin(\[Theta])",Red],{-0.1,Sin[th]/2}],90\[Degree]],

(* SEN COS TAN *)

(* sin *)
{Red,Thickness[0.008],Line[{{0,0},{0,Sin[th]}}]},
{Red,,Dashing[Medium],Line[{{Cos[th],0},{Cos[th],Sin[th]}}]},

(* cos *) 
{Blue,Thickness[0.008],Line[{{0, 0},{Cos[th],0}}]},
{Blue,Dashing[Medium],Line[{{0,Sin[th]},{Cos[th],Sin[th]}}]}


}],
PlotRange->1,ImageSize-> 400,BaseStyle->{15},Axes->True,Ticks->Automatic,PlotRangePadding->0.25];

DynamicModule[{pt={Cos[ptctrl],Sin[ptctrl]},pt2={ptctrl,0}},
Grid[{
{LocatorPane[Dynamic[pt,
	{(pt={Cos[pt2[[1]]],Sin[pt2[[1]]]})&,
	(pt=Normalize[#];pt2={If[pt2=={2Pi,0},2Pi,Mod[ArcTan[#[[1]],#[[2]]],2 Pi]],0})&,
	(pt=Normalize[#];ptctrl=pt2[[1]])&}],
     Dynamic[anglegraph[If[pt2=={2Pi,0},2Pi,Mod[ArcTan[pt[[1]],pt[[2]]],2 Pi]]]]
     ],
   
	LineLegend[{Red, Blue,Darker[Green,0.3]},{"Sin","Cos", "\[Theta]"}]
}},Alignment->{Center,Center}]]
],
{{ptctrl,Pi/6,"Angle"},0,2Pi},TrackedSymbols:>{ptctrl}]


(* Definizione rapporti *)
rapporti[] :=
Manipulate[
Module[{anglegraph,maingraph},
anglegraph[th_]:=Show[
Graphics[{
{Lighter[Gray,0.5],Circle[{0,0},1]},
{Lighter[Gray,0.5],Line[{{0,0},{Cos[th],Sin[th]}}]},
{Black,Disk[{Cos[th], 0},0.02]},
{Black,Disk[{0, 0},0.02]},
(* {Darker[Green,0.2],Thick,Circle[{0,0},1,{0,th}]}, *)

(* tangente punto *) 
{Lighter[Gray,0.5],Line[{{-6Cos[th],-6Sin[th]},{6Cos[th],6Sin[th]}}]},

(* arco *) 
{Opacity[0.2],Darker[Green,0.3],Thick,Disk[{0,0},0.3,{0,th}]},
{Darker[Green,0.3],Thick,Circle[{0,0},0.3,{0,th}]},

(* triangolo  sin cos*) 
{Opacity[0.2],Cyan,EdgeForm[Directive[Thick,Cyan]], Triangle[{{0,0},{Cos[th],Sin[th]},{Cos[th],0}}]},

(* angolo 90\[Degree]*)  
If[th<= Pi/2 ,
	{
		{Opacity[0.2],Darker[Green,0.3],Thick,Polygon[{{Cos[th]-0.1, 0},{Cos[th]-0.1, 0.1},{Cos[th], 0.1},{Cos[th], 0}}]},
		{Darker[Green,0.3],Line[{{Cos[th]-0.1, 0},{Cos[th]-0.1, 0.1},{Cos[th], 0.1}}]}
	},
		{If[ th <= Pi,
		{
			{Opacity[0.2],Darker[Green,0.3],Thick,Polygon[{{Cos[th]+0.1, 0},{Cos[th]+0.1, 0.1},{Cos[th], 0.1},{Cos[th], 0}}]},
			{Darker[Green,0.3],Line[{{Cos[th]+0.1, 0},{Cos[th]+0.1, 0.1},{Cos[th], 0.1}}]}
		},
			{If[  th <=(3*Pi)/2,
			{
				{Opacity[0.2],Darker[Green,0.3],Thick,Polygon[{{Cos[th]+0.1, 0},{Cos[th]+0.1, -0.1},{Cos[th], -0.1},{Cos[th], 0}}]},
				{Darker[Green,0.3],Line[{{Cos[th]+0.1, 0},{Cos[th]+0.1, -0.1},{Cos[th], -0.1}}]}
			},
				{{Opacity[0.2],Darker[Green,0.3],Thick,Polygon[{{Cos[th]-0.1, 0},{Cos[th]-0.1, -0.1},{Cos[th], -0.1},{Cos[th], 0}}]},
				{Darker[Green,0.3],Line[{{Cos[th]-0.1, 0},{Cos[th]-0.1, -0.1},{Cos[th], -0.1}}]}
			}]
		}]
}],

(* linea tratteggiata per sin *)
{Lighter[Gray,0.5],Dashing[Medium],Line[{{Cos[th],Sin[th]-3},{Cos[th],3}}]},

(* TESTO *)
Text["H",{Cos[th]+0.1,0.1}],
Text["P",{Cos[th] +0.1,Sin[th]+0.1}],
Text["O",{-0.1,0.1}],
Text[Style["\[Theta]",Darker[Green,0.3]],{0.2,0.1}]

}],
PlotRange->1,ImageSize-> 400,BaseStyle->{15},Axes->True,Ticks->Automatic,PlotRangePadding->0.25];

DynamicModule[{pt={Cos[ptctrl],Sin[ptctrl]},pt2={ptctrl,0}},
Grid[{
{LocatorPane[Dynamic[pt,
	{(pt={Cos[pt2[[1]]],Sin[pt2[[1]]]})&,
	(pt=Normalize[#];pt2={If[pt2=={2Pi,0},2Pi,Mod[ArcTan[#[[1]],#[[2]]],2 Pi]],0})&,
	(pt=Normalize[#];ptctrl=pt2[[1]])&}],
     Dynamic[anglegraph[If[pt2=={2Pi,0},2Pi,Mod[ArcTan[pt[[1]],pt[[2]]],2 Pi]]]]
     ],
     
     LineLegend[{Darker[Green,0.3]},{ "\[Theta]"}]
}},Alignment->{Center,Center}]]
],
{{ptctrl,Pi/6,"angle"},0,2Pi},TrackedSymbols:>{ptctrl}]


triangolorett[] :=
Grid[{{
Graphics[{
(* INIZIALIZZAZIONE PUNTI *)
p11  = {-1,0};
p22 = {1,0};
p33 = {Cos[Pi/4],Sin[Pi/4]};
hp33 = {p33[[1]],0};

(* TRIANGOLO *)
{Opacity[0.1],Cyan,EdgeForm[Directive[Thick,Cyan]],Triangle[{p11,hp33, p33}]},

(* ARCO SU C *)
{Opacity[0.2],Darker[Green,0.3],Thick,Disk[p11,0.2, angolo[p33,p22, p11]]},
{Darker[Green,0.2],Thick,Circle[p11,0.2, angolo[p33,p22, p11]]},

(* ANGOLO RETTO *)
{Opacity[0.2],Darker[Green,0.3],Thick,Polygon[{{hp33[[1]]-0.1, 0},{hp33[[1]]-0.1, 0.1},{hp33[[1]], 0.1},hp33}]},
{Darker[Green,0.3],Line[{{hp33[[1]]-0.1, 0},{hp33[[1]]-0.1, 0.1},{hp33[[1]], 0.1}}]},

(* h *)
{Black,Disk[hp33,0.02]},
(* PUNTI *)
(* A *) {Black,Disk[p33,0.02]},
(* C *) {Black,Disk[p11,0.02]},

(* TESTO *)
Text["A",{p33[[1]], p33[[2]]+0.1}],
Text["C",{p11[[1]]-0.1, p11[[2]]}],
Text["B",{hp33[[1]], hp33[[2]]-0.1}],
Text[Style["\[Theta]",Darker[Green,0.3]], {p11[[1]]+0.3, p11[[2]]+0.06}]

},PlotRange->1,ImageSize-> 400,BaseStyle->{15},Axes->False,PlotRangePadding->{0.20,0}]
}},Frame->Directive[Lighter[Gray,0.5]]]



tangente[] :=
Manipulate[
Module[{anglegraph,maingraph},
anglegraph[th_]:=Show[
Graphics[{
{Lighter[Gray,0.5],Circle[{0,0},1]},
{Lighter[Gray,0.5],Line[{{0,0},{Cos[th],Sin[th]}}]},

(* punti *)
{Black,Disk[{1, Tan[th]},0.02]},
{Black,Disk[{0, Tan[th]},0.02]},
{Black,Disk[{1,0},0.02]},

(* tangente punto *) 
{Lighter[Gray,0.5],Line[{{-6Cos[th],-6Sin[th]},{6Cos[th],6Sin[th]}}]},

(* angolo *) 
{Opacity[0.2],Darker[Green,0.3],Thick,Disk[{0,0},0.3,{0,th}]},
{Darker[Green,0.3],Circle[{0,0},0.3,{0,th}]},
			  
(* linea tratteggiata per Tan *)
{Lighter[Gray,0.5],Dashing[Medium],Line[{{1,3},{1,Tan[th]-3}}]},

(* linea tratteggiata per Tan 2*)
{Lighter[Gray,0.5],Dashing[Medium],Line[{{-3,Tan[th]},{3,Tan[th]}}]},

(* TESTO *)
Text["P",{Cos[th] +0.1,Sin[th]+0.1}],
Text["T",{1.1,Tan[th]+0.1}],
Text["Xt",{1.1,0.1}],
Text["Yt",{0.1,Tan[th]+0.1}],
Text[Style["\[Theta]",Darker[Green,0.3]],{0.2,0.1}],
Rotate[Text[Style["Tan(\[Theta])",Orange],{1.1,Tan[th]/2}],90\[Degree]],

(* TAN *)
(* tan *)  
{Orange,Thick,Line[{{1,0},{1,Tan[th]}}]},
{Orange,Thickness[0.008],Dashing[Medium],Line[{{0,0},{0,Tan[th]}}]},

}],
PlotRange->1,ImageSize-> 400,BaseStyle->{15},Axes->True,Ticks->Automatic,PlotRangePadding->0.25];

DynamicModule[{pt={Cos[ptctrl],Sin[ptctrl]},pt2={ptctrl,0}},
Grid[{
{LocatorPane[Dynamic[pt,
	{(pt={1,Tan[ptctrl]})&,
	(pt=Normalize[#];pt2={If[pt2=={2Pi,0},2Pi,Mod[ArcTan[#[[1]],#[[2]]],2 Pi]],0})&,
	(pt=Normalize[#];ptctrl=pt2[[1]])&}],
     Dynamic[anglegraph[If[pt2=={2Pi,0},2Pi,Mod[ArcTan[pt[[1]],pt[[2]]],2 Pi]]]]
     ],
     
     LineLegend[{Orange},{"Tan"}]
}},Alignment->{Center,Center}]]
],
{{ptctrl,Pi/6,"angle"},0,2Pi},TrackedSymbols:>{showvalue,ptctrl}]



definizionetangente[] :=
Manipulate[
Module[{anglegraph,maingraph},
anglegraph[th_]:=Show[
Graphics[{
{Lighter[Gray,0.5],Circle[{0,0},1]},
{Lighter[Gray,0.5],Line[{{0,0},{Cos[th],Sin[th]}}]},

(* punti *)
{Black,Disk[{Cos[th], 0},0.02]},
{Black,Disk[{1, Tan[th]},0.02]},
{Black,Disk[{0, 0},0.02]},
{Black,Disk[{1, 0},0.02]},


(* tangente punto *) 
{Lighter[Gray,0.5],Line[{{-6Cos[th],-6Sin[th]},{6Cos[th],6Sin[th]}}]},

(* arco *) 
{Opacity[0.2],Darker[Green,0.3],Thick,Disk[{0,0},0.3,{0,th}]},
{Darker[Green,0.3],Circle[{0,0},0.3,{0,th}]},

(* triangolo sin cos *) 
{Opacity[0.2],Cyan,Thick,EdgeForm[Directive[Thick,Cyan]], Triangle[{{0,0},{Cos[th],Sin[th]},{Cos[th],0}}]},

(* triangolo tan *) 
{Opacity[0.2],Magenta,Thick,EdgeForm[Directive[Magenta]], Triangle[{{0,0},{1,0},{1,Tan[th]}}]},

(* cos sin angolo 90\[Degree] *)  
If[th<= Pi/2 ,
	{
		{Opacity[0.2],Darker[Green,0.3],Thick,Polygon[{{Cos[th]-0.1, 0},{Cos[th]-0.1, 0.1},{Cos[th], 0.1},{Cos[th], 0}}]},
		{Darker[Green,0.3],Line[{{Cos[th]-0.1, 0},{Cos[th]-0.1, 0.1},{Cos[th], 0.1}}]}
	},
		{If[ th <= Pi,
		{
			{Opacity[0.2],Darker[Green,0.3],Thick,Polygon[{{Cos[th]+0.1, 0},{Cos[th]+0.1, 0.1},{Cos[th], 0.1},{Cos[th], 0}}]},
			{Darker[Green,0.3],Line[{{Cos[th]+0.1, 0},{Cos[th]+0.1, 0.1},{Cos[th], 0.1}}]}
		},
			{If[  th <=(3*Pi)/2,
			{
				{Opacity[0.2],Darker[Green,0.3],Thick,Polygon[{{Cos[th]+0.1, 0},{Cos[th]+0.1, -0.1},{Cos[th], -0.1},{Cos[th], 0}}]},
				{Darker[Green,0.3],Line[{{Cos[th]+0.1, 0},{Cos[th]+0.1, -0.1},{Cos[th], -0.1}}]}
			},
				{{Opacity[0.2],Darker[Green,0.3],Thick,Polygon[{{Cos[th]-0.1, 0},{Cos[th]-0.1, -0.1},{Cos[th], -0.1},{Cos[th], 0}}]},
				{Darker[Green,0.3],Line[{{Cos[th]-0.1, 0},{Cos[th]-0.1, -0.1},{Cos[th], -0.1}}]
			}
		}]
	}]
}],

(* tan angolo 90\[Degree] *)
If[th<= Pi/2  || (th > Pi  && th <= (3*Pi)/2),
{
	{Opacity[0.2],Darker[Green,0.3],Thick,Polygon[{{0.9, 0},{0.9, 0.1},{1, 0.1},{1, 0}}]},
	{Darker[Green,0.3],Line[{{0.9, 0},{0.9, 0.1},{1, 0.1}}]}
	},
	{{Opacity[0.2],Darker[Green,0.3],Thick,Polygon[{{0.9, 0},{0.9, -0.1},{1, -0.1},{1, 0}}]},
	{Darker[Green,0.3],Line[{{0.9, 0},{0.9, -0.1},{1, -0.1}}]}
}],

(* retta tratteggiata passante per P *)
{Lighter[Gray,0.5],Dashing[Medium],Line[{{Cos[th],Sin[th]-3},{Cos[th],3}}]},
(* retta tratteggiata tangente *)
{Lighter[Gray,0.5],Dashing[Medium],Line[{{1, -3},{1, 3}}]},

(* TESTO *)
Text["H",{Cos[th]+0.1,0.1}],
Text["P",{Cos[th] +0.1,Sin[th]+0.1}],
Text["O",{-0.1,0.1}],
Text["K",{1.1,0.1}],
Text["T",{1.1,Tan[th]+0.1}],
Text[Style["\[Theta]",Darker[Green,0.3]],{0.2,0.1}],
Rotate[Text[Style["Tan(\[Theta])",Orange],{1.1,Tan[th]/2}],90\[Degree]],

(* TAN *)
{Orange,Thick,Line[{{1,0},{1,Tan[th]}}]}, 

}],
PlotRange->1,ImageSize-> 400,BaseStyle->{15},Axes->True,Ticks->Automatic,PlotRangePadding->0.25];

DynamicModule[{pt={Cos[ptctrl],Sin[ptctrl]},pt2={ptctrl,0}},
Grid[{
{LocatorPane[Dynamic[pt,
	{(pt={Cos[pt2[[1]]],Sin[pt2[[1]]]})&,
	(pt=Normalize[#];pt2={If[pt2=={2Pi,0},2Pi,Mod[ArcTan[#[[1]],#[[2]]],2 Pi]],0})&,
	(pt=Normalize[#];ptctrl=pt2[[1]])&}],
     Dynamic[anglegraph[If[pt2=={2Pi,0},2Pi,Mod[ArcTan[pt[[1]],pt[[2]]],2 Pi]]]]
     ],
     
     LineLegend[{Orange,Darker[Green,0.3]},{"Tan", "\[Theta]"}]
}},Alignment->{Center,Center}]]
],
{{ptctrl,Pi/6,"angle"},0,2Pi},TrackedSymbols:>{showvalue,ptctrl}]


angolinoti30[] :=
Manipulate[
Module[{anglegraph,maingraph},
anglegraph[th_]:=Show[
Graphics[{
{Lighter[Gray,0.5],Circle[{0,0},1]},
{Lighter[Gray,0.5],Line[{{0,0},{Cos[th],Sin[th]}}]},

(* punti *)
{Black,Disk[{Cos[th], 0},0.02]},
{Black,Disk[{0, Sin[th]},0.02]},

(* tangente punto *)
{Lighter[Gray,0.5],Line[{{-6Cos[th],-6Sin[th]},{6Cos[th],6Sin[th]}}]},

(* angolo *)
{Opacity[0.2],Darker[Green,0.3],Thick,Disk[{0,0},0.3,{0,th}]},
{Darker[Green,0.3],Circle[{0,0},0.3,{0,th}]},

(* linea tratteggiata per cos *)
{Lighter[Gray,0.5],,Dashing[Medium],Line[{{Cos[th]-3,Sin[th]},{3,Sin[th]}}]},

(* linea tratteggiata per sin *)
{Lighter[Gray,0.5],Dashing[Medium],Line[{{Cos[th],Sin[th]-3},{Cos[th],3}}]},

(* TESTO *)
Text["Xp",{Cos[th]+0.1,0.1}],
Text["Yp",{0.1,Sin[th]+0.1}],
Text["P",{Cos[th] +0.1,Sin[th]+0.1}],
Text[Style["\[Theta]",Darker[Green,0.3]],{0.2,0.1}],

Text[Style["Cos(\[Theta])",Blue],{Cos[th]/2,-0.1}],
Rotate[Text[Style["Sin(\[Theta])",Red],{-0.1,Sin[th]/2}],90\[Degree]],

(* SEN COS TAN *)
(* sin *) 
{Red,Thickness[0.008],Line[{{0,0},{0,Sin[th]}}]},
{Red,,Dashing[Medium],Line[{{Cos[th],0},{Cos[th],Sin[th]}}]},

(* cos *) 
{Blue,Thickness[0.008],Line[{{0, 0},{Cos[th],0}}]},
{Blue,Dashing[Medium],Line[{{0,Sin[th]},{Cos[th],Sin[th]}}]},

{
If[th != Pi/2 && th != 3Pi/2,
	(* tan *)  {
	{Orange,Thick,Line[{{1,0},{1,Tan[th]}}]},
	{Orange,Thickness[0.008],Dashing[Medium],Line[{{0,0},{0,Tan[th]}}]}
	},
	{}
]}


}],
PlotRange->1,ImageSize-> 400,BaseStyle->{15},Axes->True,Ticks->Automatic,PlotRangePadding->0.25];

DynamicModule[{pt={Cos[ptctrl],Sin[ptctrl]}},
Labeled[Grid[{
{LocatorPane[Dynamic[pt,
	{(pt=Normalize[#]) &,
	(pt=Normalize[#])&,
	(pt=Normalize[#];ptctrl=If[#=={2Pi,0},Pi,Mod[ArcTan[#[[1]],#[[2]]],2 Pi]])&}],
     Dynamic[anglegraph[Mod[ArcTan[pt[[1]],pt[[2]]],2 Pi]]],Enabled->False],

     LineLegend[{Darker[Green,0.3],Red, Blue,Orange},{Row[{Style["\[Theta]"]}],Row@{"Sin(\[Theta]) = ",pt[[2]]},Row@{"Cos(\[Theta]) = ",pt[[1]]}, Row@{"Tan(\[Theta]) = ",Tan[ptctrl]}},LegendMarkerSize->40, LabelStyle->15]

}},Alignment->{Center,Center}],{Row[{Style["","Label",20,Gray],Text@Style["\[Theta] = ",Darker[Green,0.3],20],Style[ptctrl,Darker[Green,0.3],25]}],

Style["",10,Lighter[Gray,0.7],"Label"]},{{Top,Left},{Bottom,Right}}]]
],
{{ptctrl,Pi/6,""},0,2Pi,Pi/6},TrackedSymbols:>{showvalue,ptctrl}]


angolinoti45[]:=
Manipulate[
Module[{anglegraph,maingraph},
anglegraph[th_]:=Show[
Graphics[{
{Lighter[Gray,0.5],Circle[{0,0},1]},
(* {Darker[Green,0.2],Thick,Circle[{0,0},1,{0,th}]}, *)
{Lighter[Gray,0.5],Line[{{0,0},{Cos[th],Sin[th]}}]},
{Black,Disk[{Cos[th], 0},0.02]},
{Black,Disk[{0, Sin[th]},0.02]},
(* tangente punto *) {Lighter[Gray,0.5],Line[{{-6Cos[th],-6Sin[th]},{6Cos[th],6Sin[th]}}]},
(* angolo *) {Opacity[0.2],Darker[Green,0.3],Thick,Disk[{0,0},0.3,{0,th}]},
			  {Darker[Green,0.3],Circle[{0,0},0.3,{0,th}]},
(* linea tratteggiata per cos *){Lighter[Gray,0.5],,Dashing[Medium],Line[{{Cos[th]-3,Sin[th]},{3,Sin[th]}}]},
(* linea tratteggiata per sin *){Lighter[Gray,0.5],Dashing[Medium],Line[{{Cos[th],Sin[th]-3},{Cos[th],3}}]},

(* TESTO *)
Text["Xp",{Cos[th]+0.1,0.1}],
Text["Yp",{0.1,Sin[th]+0.1}],
Text["P",{Cos[th] +0.1,Sin[th]+0.1}],
Text[Style["\[Theta]",Darker[Green,0.3]],{0.2,0.1}],

Text[Style["Cos(\[Theta])",Blue],{Cos[th]/2,-0.1}],
Rotate[Text[Style["Sin(\[Theta])",Red],{-0.1,Sin[th]/2}],90\[Degree]],

(* SEN COS TAN *)
(* sin *) {Red,Thickness[0.008],Line[{{0,0},{0,Sin[th]}}]},
		    {Red,,Dashing[Medium],Line[{{Cos[th],0},{Cos[th],Sin[th]}}]},
(* cos *) {Blue,Thickness[0.008],Line[{{0, 0},{Cos[th],0}}]},
		     {Blue,Dashing[Medium],Line[{{0,Sin[th]},{Cos[th],Sin[th]}}]},

{
If[th != Pi/2 && th != 3Pi/2,
(* tan *)  {
{Orange,Thick,Line[{{1,0},{1,Tan[th]}}]},
		{Orange,Thickness[0.008],Dashing[Medium],Line[{{0,0},{0,Tan[th]}}]}
},{}
]}

}],
PlotRange->1,ImageSize-> 400,BaseStyle->{15},Axes->True,Ticks->Automatic,PlotRangePadding->0.25];

DynamicModule[{pt={Cos[ptctrl],Sin[ptctrl]}},
Labeled[Grid[{
{LocatorPane[Dynamic[pt,
	{(pt=Normalize[#]) &,
	(pt=Normalize[#])&,
	(pt=Normalize[#];ptctrl=If[#=={2Pi,0},Pi,Mod[ArcTan[#[[1]],#[[2]]],2 Pi]])&}],
	Dynamic[anglegraph[Mod[ArcTan[pt[[1]],pt[[2]]],2 Pi]]],Enabled->False],
	
    LineLegend[{Darker[Green,0.3],Red, Blue,Orange},{Row[{Style["\[Theta]"]}],Row@{"Sin(\[Theta]) = ",pt[[2]]},Row@{"Cos(\[Theta]) = ",pt[[1]]}, Row@{"Tan(\[Theta]) = ",Tan[ptctrl]}},LegendMarkerSize->40, LabelStyle->15]
}},Alignment->{Center,Center}],
{Row[{Style["","Label",20,Gray],Text@Style["\[Theta] = ",Darker[Green,0.3],20],Style[ptctrl,Darker[Green,0.3],25]}],

Style["",10,Lighter[Gray,0.7],"Label"]},{{Top,Left},{Bottom,Right}}]]
],
{{ptctrl,Pi/4,""},0,2Pi,Pi/4},TrackedSymbols:>{ptctrl}]



teoremacorda[] :=
Grid[{{
Graphics[{


(* CIRCONFERENZA *)
{Lighter[Gray,0.5],Circle[{0,0},1]},

{Opacity[0.2],Darker[Green,0.3],Thick,Disk[pc,0.2, angolo[pa,pb, pc]]},
{Darker[Green,0.2],Thick,Circle[pc,0.2, angolo[pa,pb, pc]]},


(* TRIANGOLO *)
{Opacity[0],Black ,EdgeForm[Black], Triangle[{pa,pb,pc}]},

(* RAGGIO *)
{Black,Dashing[Medium],Line[{{0,0},pb}]},

(* CORDA *)
{Red,Thick,Line[{pa,pb}]},

{Black,Disk[{0, 0},0.02]},
(* PUNTI *)
(* A *)
{Black,Disk[pa,0.02]},
(* B *)
{Black,Disk[{Cos[0-0.2], Sin[0-0.2]},0.02]},
(* C *)
{Black,Disk[pc,0.02]},

(* TESTO *)
Text["  A",{pa[[1]],pa[[2]]+0.1}],
Text["   B",{Cos[0], Sin[-0.3]}],
Text["   C", {pc[[1]]-0.15, pc[[2]]}],
Text["r",{0.4,-0.15}],
Text[Style["\[Theta]",Darker[Green,0.3]], {pc[[1]]+0.1, pc[[2]]+0.1}]


},PlotRange->1,ImageSize-> 400,BaseStyle->{15},Axes->False,PlotRangePadding->0.25]

, LineLegend[{Darker[Green,0.3],Red},{"\[Theta]","Corda"},LegendMarkerSize->40, LabelStyle->15]

}},Frame->Directive[Lighter[Gray,0.5]]]


teoremacorda2[]:=

Manipulate[
Module[{anglegraph},
anglegraph[a_,b_,th_]:=Module[{anga},
anga={ArcTan[(Sin[th] -a[[2]])/(Cos[th]-a[[1]])] ,ArcTan[(Sin[th] -b[[2]])/(Cos[th]-b[[1]])] };
Show[
Graphics[{

(* CIRCONFERENZA *)
{Lighter[Gray,0.5],Circle[{0,0},1]},
{Purple,Thick,Circle[{0, 0},1, {Pi/2-a[[1]], 2Pi+b[[2]]}]},

{If[ Cos[th] >b[[1]] && Cos[th] > a[[1]],
	{
		{Opacity[0.2],Darker[Green,0.3],Thick,Disk[{Cos[th], Sin[th]},0.2,{anga[[1]]+Pi,anga[[2]]+Pi } ]},
		Darker[Green,0.2],Thick,Circle[{Cos[th], Sin[th]},0.2,{anga[[1]]+Pi,anga[[2]]+Pi } ]
	},
		{If[ Cos[th] <b[[1]] &&   Cos[th] < a[[1]],
		{
			{Opacity[0.2],Darker[Green,0.3],Thick,Disk[{Cos[th], Sin[th]},0.2,{anga[[1]],anga[[2]] } ]},
			Darker[Green,0.2],Thick,Circle[{Cos[th], Sin[th]},0.2,{anga[[1]],anga[[2]]} ]
		},
			{If[ Sin[th] <b[[2]] &&   Cos[th] > a[[1]] && Cos[th]< b[[1]],
			{
				{Opacity[0.2],Darker[Green,0.3],Thick,Disk[{Cos[th], Sin[th]},0.2,{anga[[1]]+Pi,anga[[2]] } ]},
				Darker[Green,0.2],Thick,Circle[{Cos[th], Sin[th]},0.2,{anga[[1]] +Pi,anga[[2]]} ]
			},
				{If[ Sin[th] >b[[2]] &&   Cos[th] > a[[1]] && Cos[th]< b[[1]],
				{
					{Opacity[0.2],Darker[Green,0.3],Thick,Disk[{Cos[th], Sin[th]},0.2,{anga[[1]]-Pi,anga[[2]] } ]},
					Darker[Green,0.2],Thick,Circle[{Cos[th], Sin[th]},0.2,{anga[[1]]-Pi,anga[[2]]} ]
				},{}
			]}
		]}
	]}
]},


(* TRIANGOLO *)
{Opacity[0],Black ,EdgeForm[Black], Triangle[{a,b,{Cos[th], Sin[th]}}]},

(* RAGGIO *)
{Black,Dashing[Medium],Line[{{0,0},b}]},

(* CORDA *)
{Red,Thick,Line[{a,b}]},

(* PUNTO *)
{Black,Disk[{0, 0},0.02]},

(* PUNTI *)
(* A *)
{Black,Disk[a,0.02]},
(* B *)
{Black,Disk[{Cos[0-0.2], Sin[0-0.2]},0.02]},
(* C *)
{Black,Disk[{Cos[th], Sin[th]},0.02]},

(* TESTO *)
Text["  A",{a[[1]],a[[2]]+0.1}],
Text["   B",{Cos[0], Sin[-0.3]}],
Text["   C", {Cos[th]-0.15, Sin[th]}],
Text["r",{0.4,-0.15}],
Text[Style["\[Theta]",Darker[Green,0.3]], {Cos[th]+0.1, Sin[th]+0.1}]


}],PlotRange->1,ImageSize-> 400,BaseStyle->{15},Axes->False,PlotRangePadding->0.25]];



DynamicModule[{pt={Cos[ptctrl], Sin[ptctrl]},pt2={ptctrl,0}},
Grid[{
{LocatorPane[Dynamic[pt,
	{(pt={Cos[pt2[[1]]],Sin[pt2[[1]]]})&,
	(pt=Normalize[#];pt2={If[pt2=={2Pi,0},2Pi,Mod[ArcTan[#[[1]],#[[2]]],2 Pi]],0})&,
	(pt=Normalize[#];ptctrl=pt2[[1]])&}],
	Dynamic[anglegraph[pa,pb,Mod[ArcTan[pt[[1]],pt[[2]]],2 Pi]]]],
	
	pcc ={Sin[ ptctrl], Cos[ptctrl]};
LineLegend[{Darker[Green,0.3],Red,},{"\[Theta]","Corda"},LegendMarkerSize->40, LabelStyle->15]
}},Spacings->0]]

],

{{ptctrl,Pi +0.5,"angle"},0,2Pi},TrackedSymbols:>{ptctrl}]


teoremacorda3[]:=

Manipulate[
Module[{anglegraph},
anglegraph[a_,b_,c_,th_]:=Module[{anga},
anga={ArcTan[(Sin[th] -a[[2]])/(Cos[th]-a[[1]])] ,ArcTan[(Sin[th] -b[[2]])/(Cos[th]-b[[1]])] };
Show[
Graphics[{

(* CIRCONFERENZA *)
{Lighter[Gray,0.5],Circle[{0,0},1]},
{Lighter[Magenta,0.5],Thick,Circle[{0, 0},1, {Pi/2, b[[2]]}]},

{If[ Cos[th] >b[[1]] && Cos[th] > a[[1]],
	{
	{Opacity[0.2],Darker[Green,0.3],Thick,Disk[{Cos[th], Sin[th]},0.2,{anga[[1]]+Pi,anga[[2]]+Pi } ]},
	Darker[Green,0.2],Thick,Circle[{Cos[th], Sin[th]},0.2,{anga[[1]]+Pi,anga[[2]]+Pi } ]
	},
		{If[ Cos[th] <b[[1]] &&   Cos[th] < a[[1]],
			{
			{Opacity[0.2],Darker[Green,0.3],Thick,Disk[{Cos[th], Sin[th]},0.2,{anga[[1]],anga[[2]] } ]},
			Darker[Green,0.2],Thick,Circle[{Cos[th], Sin[th]},0.2,{anga[[1]],anga[[2]]} ]
		},
			{{If[ Sin[th] <b[[2]] &&   Cos[th] > a[[1]] && Cos[th]< b[[1]],
				{
				{Opacity[0.2],Darker[Green,0.3],Thick,Disk[{Cos[th], Sin[th]},0.2,{anga[[1]]+Pi,anga[[2]] } ]},
				Darker[Green,0.2],Thick,Circle[{Cos[th], Sin[th]},0.2,{anga[[1]] +Pi,anga[[2]]} ]
			},
				{If[ Sin[th] >b[[2]] &&   Cos[th] > a[[1]] && Cos[th]< b[[1]],
					{
					{Opacity[0.2],Darker[Green,0.3],Thick,Disk[{Cos[th], Sin[th]},0.2,{anga[[1]]-Pi,anga[[2]] } ]},
					Darker[Green,0.2],Thick,Circle[{Cos[th], Sin[th]},0.2,{anga[[1]]-Pi,anga[[2]]} ]
				},{}
			]}
		]}}
	]}
]},


(* ANGOLO C*)
{Opacity[0.2],Darker[Green,0.3],Thick,Disk[c,0.2,{Pi/2, ArcTan[(c[[2]] -b[[2]])/(c[[1]]-b[[1]])]} ]},
{Darker[Green,0.2],Thick,Circle[c,0.2,{Pi/2, ArcTan[(c[[2]] -b[[2]])/(c[[1]]-b[[1]])]} ]},

(* TRIANGOLO *)
{Opacity[0],Black ,EdgeForm[Black], Triangle[{a,b,c}]},
{Opacity[0],Black ,EdgeForm[Black], Triangle[{a,b,{Cos[th], Sin[th]}}]},

(* ANGOLO RETTO *)
{Rotate[
	{Opacity[0.2],Darker[Green,0.3],Thick,Polygon[{{b[[1]]-0.1,b[[2]]},{b[[1]]-0.1,b[[2]]+0.1},{b[[1]],b[[2]]+0.1},b}]},ArcTan[(b[[2]] -c[[2]])/(b[[1]]-c[[1]])],b
	]},
{Rotate[
	{Darker[Green,0.3],Line[{{b[[1]]-0.1,b[[2]]},{b[[1]]-0.1,b[[2]]+0.1},{b[[1]],b[[2]]+0.1}}]}, ArcTan[(b[[2]] -c[[2]])/(b[[1]]-c[[1]])],b
]},

(* RAGGIO *)
{Black,Dashing[Medium],Line[{{0,0},b}]},

(* CORDA *)
{Red,Thick,Line[{a,b}]},

(* PUNTO *)
{Black,Disk[{0, 0},0.02]},

(* PUNTI *)
(* A *) {Black,Disk[a,0.02]},
(* B *) {Black,Disk[b,0.02]},
(* C *) {Black,Disk[c,0.02]},

(* TESTO *)
Text["  A",{ a[[1]],a[[2]]+0.1}],
Text["   B",{ b[[1]]+0.1,b[[2]]}],
Text["   C",{ c[[1]],c[[2]]-0.1}],
Text["   D", {Cos[th]+ 0.1, Sin[th]}],
Text[Style["\[Delta]",Darker[Green,0.3]], {Cos[th]-0.1, Sin[th]-0.1}],
Text[Style["\[Theta]",Darker[Green,0.3]], c+0.1],
Text["r",{0.4,-0.15}]

}],PlotRange->1,ImageSize-> 400,BaseStyle->{15},Axes->False,PlotRangePadding->0.25]];

DynamicModule[{pt={Cos[ptctrl], Sin[ptctrl]},pt2={ptctrl,0}},
Grid[{
{LocatorPane[Dynamic[pt,
	{(pt={Cos[pt2[[1]]],Sin[pt2[[1]]]})&,
	(pt=Normalize[#];pt2={If[pt2=={2Pi,0},2Pi,Mod[ArcTan[#[[1]],#[[2]]],2 Pi]],0})&,
	(pt=Normalize[#];ptctrl=pt2[[1]])&}],
	Dynamic[anglegraph[pa2,pb2,pc2,Mod[ArcTan[pt[[1]],pt[[2]]],2 Pi]]]],
	
	pcc ={Sin[ ptctrl], Cos[ptctrl]};
  LineLegend[{Darker[Green,0.3],Red},{Row@{"\[Delta]"},"Coda"},LegendMarkerSize->40]
}},Alignment->{Center,Center}]]

],{{ptctrl,0.4,"angle"},0,2Pi},TrackedSymbols:>{ptctrl}]


teoremaseni[] :=
Grid[{{
Graphics[{

(* CIRCONFERENZA *)
{Lighter[Gray,0.5],Circle[{0,0},1]},

(* ARCO SU C *)
{Opacity[0.2],Darker[Green,0.3],Thick,Disk[pc,0.2, angolo[pa,pb, pc]]},
{Darker[Green,0.2],Thick,Circle[pc,0.2, angolo[pa,pb, pc]]},

(* ARCO SU B *)
{Opacity[0.2],Darker[Green,0.3],Thick,Disk[pb,0.2, angolo[pa,pc, pb]+Pi]},
{Darker[Green,0.2],Thick,Circle[pb,0.2, angolo[pa,pc, pb]+Pi]},

(* ARCO SU A *)
{Opacity[0.2],Darker[Green,0.3],Thick,Disk[pa,0.2, {angolo[pc,pb, pa][[1]]-Pi,angolo[pc,pb, pa][[2]]}]},
{Darker[Green,0.2],Thick,Circle[pa,0.2,{angolo[pc,pb, pa][[1]]-Pi,angolo[pc,pb, pa][[2]]}]},

(* TRIANGOLO *)
{Opacity[0.1],Cyan,EdgeForm[Directive[Thick,Cyan]], Triangle[{pa,pb,pc}]},

(* RAGGIO *)
{Black,Dashing[Medium],Line[{{0,0},pb}]},

(* PUNTI *)
(* CENTRO *){Black,Disk[{0, 0},0.02]},
(* A *) {Black,Disk[pa,0.02]},
(* B *) {Black,Disk[{Cos[0-0.2], Sin[0-0.2]},0.02]},
(* C *) {Black,Disk[pc,0.02]},

(* TESTO *)
Text["A",{pa[[1]],pa[[2]]+0.1}],
Text["B",{pb[[1]]+0.1, pb[[2]]}],
Text["C", {pc[[1]]-0.1, pc[[2]]}],
Text["r",{0.4,-0.15}],
Text[Style["\[Theta]",Darker[Green,0.3]], {pc[[1]]+0.1, pc[[2]]+0.1}],
Text[Style["\[Beta]",Darker[Green,0.3]], {pb[[1]]-0.1, pb[[2]]+0.1}],
Text[Style["\[Alpha]",Darker[Green,0.3]], {pa[[1]]+0.05, pa[[2]]-0.1}]

},
(* MISURE PLOT *)
PlotRange->1,ImageSize-> 400,BaseStyle->{15},Axes->False,PlotRangePadding->0.25]

}},Frame->Directive[Lighter[Gray,0.5]]]


teoremacoseno[]:=
Grid[{{
Graphics[{
(* INIZIALIZZAZIONE PUNTI *)
p11  = {-1,0};
p22 = {1,0};
p33 = {Cos[Pi/4],Sin[Pi/4]};

(* TRIANGOLO *)
{Opacity[0.1],Cyan,EdgeForm[Directive[Thick,Cyan]],Triangle[{p11,p22, p33}]},

(* ARCO SU A *)
{Opacity[0.2],Darker[Green,0.3],Thick,Disk[p33,0.2, angolo[p11,p22, p33]-Pi/2]},
{Darker[Green,0.2],Thick,Circle[p33,0.2, angolo[p11,p22, p33]-Pi/2]},

(* ARCO SU B *)
{Opacity[0.2],Darker[Green,0.3],Thick,Disk[p22,0.2, angolo[p11,p33, p22]+Pi]},
{Darker[Green,0.2],Thick,Circle[p22,0.2, angolo[p11,p33, p22]+Pi]},

(* ARCO SU C *)
{Opacity[0.2],Darker[Green,0.3],Thick,Disk[p11,0.2, angolo[p33,p22, p11]]},
{Darker[Green,0.2],Thick,Circle[p11,0.2, angolo[p33,p22, p11]]},

(* PUNTI *)
(* A *) {Black,Disk[p33,0.02]},
(* B *) {Black,Disk[p22,0.02]},
(* C *) {Black,Disk[p11,0.02]},

(* TESTO *)
Text["A",{p33[[1]], p33[[2]]+0.1}],
Text["B",{p22[[1]]+0.1, p22[[2]]}],
Text["C",{p11[[1]]-0.1, p11[[2]]}],
Text[Style["\[Alpha]",Darker[Green,0.3]], {p33[[1]]-0.05, p33[[2]]-0.1}],
Text[Style["\[Beta]",Darker[Green,0.3]], {p22[[1]]-0.1, p22[[2]]+0.1}],
Text[Style["\[Gamma]",Darker[Green,0.3]], {p11[[1]]+0.3, p11[[2]]+0.06}]

},
(* MISURE PLOT *)
PlotRange->1,ImageSize-> 400,BaseStyle->{15},Axes->False,PlotRangePadding->{0.20,0}]
}},Frame->Directive[Lighter[Gray,0.5]]]


teoremacoseno2[]:=
Grid[{{
Graphics[{
(* INIZIALIZZAZIONE PUNTI *)
p11  = {-1,0};
p22 = {1,0};
p33 = {Cos[Pi/4],Sin[Pi/4]};
hp33 = {p33[[1]],0};

(* TRIANGOLO *)
{Opacity[0.1],Cyan,EdgeForm[Directive[Thick,Cyan]],Triangle[{p11,p22, p33}]},

(* ARCO SU A *)
{Opacity[0.2],Darker[Green,0.3],Thick,Disk[p33,0.2, angolo[p11,p22, p33]-Pi/2]},
{Darker[Green,0.2],Thick,Circle[p33,0.2, angolo[p11,p22, p33]-Pi/2]},

(* ARCO SU B *)
{Opacity[0.2],Darker[Green,0.3],Thick,Disk[p22,0.2, angolo[p11,p33, p22]+Pi]},
{Darker[Green,0.2],Thick,Circle[p22,0.2, angolo[p11,p33, p22]+Pi]},

(* ARCO SU C *)
{Opacity[0.2],Darker[Green,0.3],Thick,Disk[p11,0.2, angolo[p33,p22, p11]]},
{Darker[Green,0.2],Thick,Circle[p11,0.2, angolo[p33,p22, p11]]},

(* ANGOLO RETTO *)
{Opacity[0.2],Darker[Green,0.3],Thick,Polygon[{{hp33[[1]]-0.1, 0},{hp33[[1]]-0.1, 0.1},{hp33[[1]], 0.1},hp33}]},
{Darker[Green,0.3],Line[{{hp33[[1]]-0.1, 0},{hp33[[1]]-0.1, 0.1},{hp33[[1]], 0.1}}]},


(* PUNTI *)
(* h *) {Black,Dashing[Medium],Line[{hp33,p33}]},
(* Ph *) {Black,,Disk[hp33,0.02]},
(* A *) {Black,Disk[p33,0.02]},
(* B *) {Black,Disk[p22,0.02]},
(* C *) {Black,Disk[p11,0.02]},

(* TESTO *)
Text["A",{p33[[1]], p33[[2]]+0.1}],
Text["B",{p22[[1]]+0.1, p22[[2]]}],
Text["C",{p11[[1]]-0.1, p11[[2]]}],
Text["D",{hp33[[1]], hp33[[2]]-0.1}],
Text["h",{hp33[[1]]-0.05, p33[[2]]/2}],
Text[Style["\[Alpha]",Darker[Green,0.3]], {p33[[1]]-0.05, p33[[2]]-0.1}],
Text[Style["\[Beta]",Darker[Green,0.3]], {p22[[1]]-0.1, p22[[2]]+0.1}],
Text[Style["\[Gamma]",Darker[Green,0.3]], {p11[[1]]+0.3, p11[[2]]+0.06}]

},
(* MISURE PLOT *)
PlotRange->1,ImageSize-> 400,BaseStyle->{15},Axes->False,PlotRangePadding->{0.20,0}]

}},Frame->Directive[Lighter[Gray,0.5]]]



pitagora[]:=
Grid[{{
Graphics[{
(* INIZIALIZZAZIONE PUNTI *)
p11  = {-1,0};
p22 = {1,0};
p33 = {Cos[Pi/4],Sin[Pi/4]};
hp33 = {p33[[1]],0};

(* TRIANGOLO *)
{Opacity[0.1],Cyan,EdgeForm[Directive[Thick,Cyan]],Triangle[{p11,hp33, p33}]},

(* ARCO SU A *)
{Opacity[0.2],Darker[Green,0.3],Thick,Disk[p33,0.2, {angolo[p11,hp33, p33][[1]]+Pi,3Pi/2}]},
{Darker[Green,0.2],Thick,Circle[p33,0.2,  {angolo[p11,hp33, p33][[1]]+Pi,3Pi/2}]},

(* ARCO SU C *)
{Opacity[0.2],Darker[Green,0.3],Thick,Disk[p11,0.2, angolo[p33,hp33, p11]]},
{Darker[Green,0.2],Thick,Circle[p11,0.2, angolo[p33,hp33, p11]]},

(* ANGOLO RETTO *)
{Opacity[0.2],Darker[Green,0.3],Thick,Polygon[{{hp33[[1]]-0.1, 0},{hp33[[1]]-0.1, 0.1},{hp33[[1]], 0.1},hp33}]},
{Darker[Green,0.3],Line[{{hp33[[1]]-0.1, 0},{hp33[[1]]-0.1, 0.1},{hp33[[1]], 0.1}}]},


(* PUNTI *)
(* h *) {Black,Disk[hp33,0.02]},
(* A *) {Black,Disk[p33,0.02]},
(* C *) {Black,Disk[p11,0.02]},

(* TESTO *)
Text["A",{p33[[1]], p33[[2]]+0.1}],
Text["C",{p11[[1]]-0.1, p11[[2]]}],
Text["B",{hp33[[1]], hp33[[2]]-0.1}],
Text[Style["\[Alpha]",Darker[Green,0.3]], {p33[[1]]-0.05, p33[[2]]-0.1}],
Text[Style["\[Beta]",Darker[Green,0.3]], {hp33[[1]]-0.15, hp33[[2]]+0.15}],
Text[Style["\[Gamma]",Darker[Green,0.3]], {p11[[1]]+0.3, p11[[2]]+0.06}]

},
(* MISURE PLOT*)
PlotRange->1,ImageSize-> 400,BaseStyle->{15},Axes->False,PlotRangePadding->{0.20,0}]
}, { Row[{"      Se l'angolo \[EGrave] retto il coseno ha valore = 1, \nquindi otteniamo il teorema di pitagora: \!\(\*SuperscriptBox[\(AB\), \(2\)]\) + \!\(\*SuperscriptBox[\(BC\), \(2\)]\) = \!\(\*SuperscriptBox[\(AC\), \(2\)]\)"}]}},Frame->Directive[Lighter[Gray,0.5]]]


(*######################### ESERCIZI ##############################*)











EsercizioEsempio[]:=

Module[{},

datiEsempio = StringJoin[Style["A = 25 \nC = 50",FontFamily-> "OpenDyslexic",Bold],Style["\nTrovare Sen(\[Alpha])",FontColor->Red,FontFamily-> "OpenDyslexic",Bold]];
risoluzioneEsempio =Panel[Style[" Sin(\[Alpha]) = \!\(\*FractionBox[\(A\), \(C\)]\) = \!\(\*FractionBox[\(\(\\\ \\\ \)\(25\)\(\\\ \)\), \(\(\\\ \)\(50\)\)]\) = \!\(\*FractionBox[\(\(\\\ \)\(1\)\(\\\ \)\), \(2\)]\)",FontFamily-> "OpenDyslexic"]];

Grid[{{Text[Style["Esempio 1:",20,FontColor-> Red,FontFamily-> "OpenDyslexic"]]}, 

(*Disegno del triangolo*)
{Magnify[Graphics[{
   (* triangle 
   Line[{{0, 0}, {0,1}, {2, 0}, {0, 0}}],
   right angle symbol 
   Line[{{0, 0.1}, {0.1, 0.1}, {0.1, 0}}],
   angle symbol 
  Circle[{2, 0}, 0.2, {145.5 Degree, 182.5 Degree}],
  Circle[{0,1},0.2,{260 Degree,340 Degree}],*)
  
   (* ANGOLO RETTANGOLO *)
 {Opacity[0.2],Darker[Green,0.3],Polygon[{{0, 0.1}, {0.1, 0.1}, {0.1, 0},{0, 0}}]},
 {Darker[Green,0.3],Line[{{0, 0.1}, {0.1, 0.1}, {0.1, 0}}]},
  
  (* ANGOLO IN (2,0) *)
  {Opacity[0.2], Darker[Green,0.3], Disk[{2, 0}, 0.2, angolo[{0, 0}, {0, 1}, {2, 0}]+Pi]},
  {Darker[Green,0.2], Circle[{2, 0}, 0.2, angolo[{0, 0}, {0,1}, {2, 0}]+Pi]},

  (* ANGOLO IN (0,1) *)
  {Opacity[0.2], Darker[Green,0.3], Disk[{0, 1}, 0.2, {angolo[{2, 0}, {0, 0}, {0, 1}][[1]]+2Pi,3Pi/2}]},
  {Darker[Green,0.2], Circle[{0, 1}, 0.2, {angolo[{2, 0}, {0, 0}, {0, 1}][[1]]+2Pi,3Pi/2}]},
  
  (* triangle *)
  {Opacity[0],EdgeForm[Directive[Black]],Triangle[{{0, 0}, {0,1}, {2, 0}}]},
  
  (* labels *)
  Rotate[
    Text[Style["A", 15,FontFamily-> "OpenDyslexic"],{-0.1, 0.5}], 0 Degree],
    Text[Style["B", 15,FontFamily-> "OpenDyslexic"],{0.7, -0.1}],
    Text[Style["C", 15,FontFamily-> "OpenDyslexic"],{1, 0.6}, {-1, 0}],
    Text[Style["\[Theta]", 10,Darker[Green,0.3],FontFamily-> "OpenDyslexic"], {1.7, 0.08}],
    Text[Style["\[Alpha]",10,Darker[Green,0.3],FontFamily-> "OpenDyslexic"],{0.085,0.75}],
    Text[Style["90\[Degree]",10,Darker[Green,0.3],FontFamily-> "OpenDyslexic"],{0.2,0.2}]
  }],2],
  
  (*Vengono stampati i dati del problema*)
  Magnify[Apply[StringJoin,ToString[#,StandardForm]&/@datiEsempio],1.4],Text[""]},
  
  (*Viene stampata la risoluzione dell'esempio*)
  {Text[Style["Procedimento:",17,FontColor -> Red,FontFamily-> "OpenDyslexic"]]},{Text[" "],Magnify[risoluzioneEsempio,2]}}

,Alignment-> {Left,Center},Spacings -> {5,5},Dividers->{{},{3 -> Red}}]

]

(*Modulo che controlla la correttezza di una risposta data dall'utente*)
CheckAnswer[answer_,correct_]:=
DynamicModule[{},
If[answer == "", 
         Text[""],  (*Se la risposta \[EGrave] vuota stampo nulla *)
         If[answer == correct,
                    (*Se la risposta \[EGrave] corretta stampa un segno di spunta verde verde*)
                    Style["\[Checkmark]",FontColor->Green], 
                    (*Se la risposta \[EGrave] sbagliata stampa una X rossa*)
                    Style["X",FontColor->Red,Bold],
                    Text[""]],
         Text[""]]
 (*If[answer == correct,Style["\[Checkmark]",FontColor->Green],Style["X",FontColor->Red,Bold],Text[""]]*)
]

(*Esercizio1 *)
Esercizio1[]:=
Module[{},
datiEsercizioGuidato = StringJoin[Style["A = 40 \nB = 110",FontFamily-> "OpenDyslexic",Bold],Style["\nTrovare Sen(\[Alpha]),Cos(\[Alpha]),Tan(\[Alpha]) e l'angolo \[Alpha]",FontColor->Red,FontFamily-> "OpenDyslexic",Bold]];

risoluzioneEsercizioGuidatoPasso1 = Row[{TPitagora[],Style[
                                    "\n",
                                    "Applico il teorema \[LongRightArrow] \!\(\*SuperscriptBox[\(A\), \(2\)]\) + ",FontFamily-> "OpenDyslexic"],
                                    Dynamic[InputField[Dynamic[catetoNome],String,FieldSize-> 1]],
                                    "\!\(\*SuperscriptBox[\(\\\ \), \(2\)]\) ",
                                    Dynamic[CheckAnswer[catetoNome,"B"]],
                                    " = ",
                                    InputField[Dynamic[ipotenusaNome],String,FieldSize-> 1] ,
                                   "\!\(\*SuperscriptBox[\(\\\ \), \(\(2\)\(\\\ \\\ \\\ \)\)]\)",
                                    Dynamic[CheckAnswer[ipotenusaNome,"C"]],
                                    Style["\n \n",FontFamily-> "OpenDyslexic"],
                                    Style["Sostituisco i valori \[LongRightArrow] \!\(\*SuperscriptBox[\(40\), \(2\)]\) + ",FontFamily-> "OpenDyslexic"],
                                    InputField[Dynamic[catetoValore],String,FieldSize-> 3],
                                    "\!\(\*SuperscriptBox[\(\\\ \), \(2\)]\) ",
                                    Dynamic[CheckAnswer[catetoValore,"110"]],
                                    " = \!\(\*SuperscriptBox[\(C\), \(2\)]\)\n",
                                    "\n",
                                    Style["Ricavo C \[LongRightArrow] C = ",FontFamily-> "OpenDyslexic"],
                                    SqrtBox[Row[{"\!\(\*SuperscriptBox[\(A\), \(2\)]\) + ",
                                    InputField[Dynamic[catetoBNome],String,FieldSize-> 1],
                                    "\!\(\*SuperscriptBox[\(\\\ \), \(\(2\)\(\\\ \\\ \\\ \\\ \)\)]\)"}]] // DisplayForm,
                                    Dynamic[CheckAnswer[catetoBNome,"B"]],"\n \n",
                                    Style["Approssima il risultato per difetto!\n\n",FontFamily-> "OpenDyslexic"],
                                    Style["Calcolo C \[LongRightArrow] C = ",FontFamily-> "OpenDyslexic"], 
                                    SqrtBox[Row[{InputField[Dynamic[catetoAValore],String,FieldSize-> 2],
                                    "\!\(\*SuperscriptBox[\(\\\ \), \(\(2\)\(\\\ \\\ \\\ \\\ \)\)]\)",
                                    Dynamic[CheckAnswer[catetoAValore,"40"]],
                                    " + ",
                                    InputField[Dynamic[catetoBValore],String,FieldSize-> 3],
                                    "\!\(\*SuperscriptBox[\(\\\ \), \(\(2\)\(\\\ \\\ \)\)]\)",
                                    Dynamic[CheckAnswer[catetoBValore,"110"]]}]] //  DisplayForm,
                                    " = ",
                                    InputField[Dynamic[ipotenusaValore],String,FieldSize->3],
                                    Dynamic[CheckAnswer[ipotenusaValore,"117"]]
}];

risoluzioneEsercizioGuidatoPasso2 = Row[{Style["sen(\[Alpha]) = ",FontFamily-> "OpenDyslexic"],
                                    (Style["A",FontFamily-> "OpenDyslexic"])/InputField[Dynamic[ipotenusaSenCNome],String,FieldSize->2],
                                    Dynamic[CheckAnswer[ipotenusaSenCNome,"C"]],
                                    " = ",
                                    (Style["40",FontFamily-> "OpenDyslexic"])/InputField[Dynamic[ipotenusaSenValore],String,FieldSize->3],
                                    Dynamic[CheckAnswer[ipotenusaSenValore,"117"]]}];

risoluzioneEsercizioGuidatoPasso3 = Row[{Style["cos(\[Alpha]) = ",FontFamily-> "OpenDyslexic"],
                                    (Style["B",FontFamily-> "OpenDyslexic"])/InputField[Dynamic[ipotenusaCosCNome],String,FieldSize->2],
                                    Dynamic[CheckAnswer[ipotenusaCosCNome,"C"]],
                                    " = ",
                                    (Style["110",FontFamily-> "OpenDyslexic"])/InputField[Dynamic[ipotenusaCosValore],String,FieldSize->3],
                                    Dynamic[CheckAnswer[ipotenusaCosValore,"117"]]}];


risoluzioneEsercizioGuidatoPasso4 = Row[{Style["tan(\[Alpha]) = ",FontFamily-> "OpenDyslexic"],
                                       Row[{InputField[Dynamic[senNome],String,FieldSize->3],Style["(\[Alpha])",FontFamily-> "OpenDyslexic"]}]/(Style["cos(\[Alpha])",FontFamily-> "OpenDyslexic"]),
                                       Dynamic[CheckAnswer[senNome,"sen"]],
                                       "\n",
                                       Style["Sostituisco i valori di seno e coseno trovati ai passi 2 e 3: ",FontFamily-> "OpenDyslexic"],
                                       Row[{InputField[Dynamic[senValoreNum],String,FieldSize->3],Dynamic[CheckAnswer[senValoreNum,"40"]]}]/Row[ {InputField[Dynamic[senValoreDen],String,FieldSize->3],Dynamic[CheckAnswer[senValoreDen,"117"]]}],
                                       "/",
                                       Row[{InputField[Dynamic[cosValoreNum],String,FieldSize->3],Dynamic[CheckAnswer[cosValoreNum,"110"]]}]/Row[{InputField[Dynamic[cosValoreDen],String,FieldSize->3],Dynamic[CheckAnswer[cosValoreDen,"117"]]}],
                                       "\n",
                                       Style["Ricorda! La divisione tra due frazioni\ndiventa la prima frazione moltiplicata per l'inversa della seconda:",FontFamily-> "OpenDyslexic"],
                                       Row[{InputField[Dynamic[senValoreNum2],String,FieldSize->3],Dynamic[CheckAnswer[senValoreNum2,"40"]]}]/Row[ {InputField[Dynamic[senValoreDen2],String,FieldSize->3],Dynamic[CheckAnswer[senValoreDen2,"117"]]}],
                                       "\[CenterDot]",
                                       Row[{InputField[Dynamic[cosValoreNum2],String,FieldSize->3],Dynamic[CheckAnswer[cosValoreNum2,"117"]]}]/Row[{InputField[Dynamic[cosValoreDen2],String,FieldSize->3],Dynamic[CheckAnswer[cosValoreDen2,"110"]]}],
                                       "\n",
                                       Style["Dopo aver eseguito la moltiplicazione tra le due frazioni ottengo:",FontFamily-> "OpenDyslexic"],
                                       Row[{InputField[Dynamic[resNum],String,FieldSize->3],Dynamic[CheckAnswer[resNum,"40"]]}]/Row[{InputField[Dynamic[resDen],String,FieldSize->3],Dynamic[CheckAnswer[resDen,"110"]]}],
                                       "\n",
                                       Style["Semplifico ai minimi termini la frazione e ottengo:",FontFamily-> "OpenDyslexic"],
                                       Row[{InputField[Dynamic[resNum2],String,FieldSize->3],Dynamic[CheckAnswer[resNum2,"4"]]}]/Row[{InputField[Dynamic[resDen2],String,FieldSize->3],Dynamic[CheckAnswer[resDen2,"11"]]}]
}];

risoluzioneEsercizioGuidatoPasso5 =Row[{Style["\[Alpha] = \!\(\*SuperscriptBox[\(sen\), \(-1\)]\)(",FontFamily-> "OpenDyslexic"],
                                       Row[{InputField[Dynamic[senValoreNum3],String,FieldSize->3],Dynamic[CheckAnswer[senValoreNum3,"40"]]}]/Row[{InputField[Dynamic[senValoreDen3],String,FieldSize->3],Dynamic[CheckAnswer[senValoreDen3,"117"]]}],")"}];



Grid[{{Text[Style["Esercizio Guidato:",20,FontColor-> Red,FontFamily-> "OpenDyslexic"]]},

(*Disegno del triangolo *)
{Magnify[Graphics[{

 
 (* ANGOLO RETTANGOLO *)
 {Opacity[0.2],Darker[Green,0.3],Polygon[{{0, 0.1}, {0.1, 0.1}, {0.1, 0},{0, 0}}]},
 {Darker[Green,0.3],Line[{{0, 0.1}, {0.1, 0.1}, {0.1, 0}}]},
  
  (* ANGOLO IN (2,0) *)
  {Opacity[0.2], Darker[Green,0.3], Disk[{2, 0}, 0.2, angolo[{0, 0}, {0, 1}, {2, 0}]+Pi]},
  {Darker[Green,0.2], Circle[{2, 0}, 0.2, angolo[{0, 0}, {0,1}, {2, 0}]+Pi]},

  (* ANGOLO IN (0,1) *)
  {Opacity[0.2], Darker[Green,0.3], Disk[{0, 1}, 0.2, {angolo[{2, 0}, {0, 0}, {0, 1}][[1]]+2Pi,3Pi/2}]},
  {Darker[Green,0.2], Circle[{0, 1}, 0.2, {angolo[{2, 0}, {0, 0}, {0, 1}][[1]]+2Pi,3Pi/2}]},
  
  (* triangle *)
  {Opacity[0],EdgeForm[Directive[Black]],Triangle[{{0, 0}, {0,1}, {2, 0}}]},
  
  (* labels *)
  Rotate[
   Text[Style["A", 15,FontFamily-> "OpenDyslexic"],
    {-0.1, 0.5}], 0 Degree],
  Text[Style["B", 15,FontFamily-> "OpenDyslexic"],
   {0.7, -0.1}],
  Text[Style["C", 15,FontFamily-> "OpenDyslexic"],
   {1, 0.6}, {-1, 0}],
 Text[Style["\[Alpha]", 10,Darker[Green,0.3],FontFamily-> "OpenDyslexic"], {1.7, 0.08}],
 Text[Style["\[Beta]",10,Darker[Green,0.3],FontFamily-> "OpenDyslexic"],{0.085,0.75}],
 Text[Style["90\[Degree]",10,Darker[Green,0.3],FontFamily-> "OpenDyslexic"],{0.2,0.2}]
  }],2]
  
  (*Vengono stampati i dati dell'esercizio*)
,Magnify[Apply[StringJoin,ToString[#,StandardForm]&/@datiEsercizioGuidato],1.4],Text[""]},
{Text[Style["Procedimento:",17,FontColor -> Red,FontFamily-> "OpenDyslexic"]]},
(*Viene stampato il passo 1 dell'esercizio*)
{Text[Style["Passo 1: Cerchiamo l'ipotenusa C.\n Applico il Teorema di Pitagora:",Bold,15,FontFamily-> "OpenDyslexic"]]},
{Text[""],Panel[Magnify[risoluzioneEsercizioGuidatoPasso1,2]]},

(*Viene stampato il passo 2 dell'esercizio*)
{Text[Style["Passo 2: Cerchiamo sen(\[Alpha]).",Bold,15,FontFamily-> "OpenDyslexic"]]},
{Text[""],Panel[Magnify[risoluzioneEsercizioGuidatoPasso2,2]]},

(*Viene stampato il passo 3 dell'esercizio*)
{Text[Style["Passo 3: Cerchiamo cos(\[Alpha]).",Bold,15,FontFamily-> "OpenDyslexic"]]},
{Text[""],Panel[Magnify[risoluzioneEsercizioGuidatoPasso3,2]]},

(*Viene stampato il passo 4 dell'esercizio*)
{Text[Style["Passo 4: Cerchiamo tan(\[Alpha]).",Bold,15,FontFamily-> "OpenDyslexic"]]},
{Text[""],Panel[Magnify[risoluzioneEsercizioGuidatoPasso4,2]]},

(*Viene stampato il passo 5 dell'esercizio*)
{Text[Style["Passo 5: Cerchiamo \[Alpha].",Bold,15,FontFamily-> "OpenDyslexic"]]},
{Text[""],Panel[Magnify[risoluzioneEsercizioGuidatoPasso5,2]]}
},Alignment-> {Left,Center},Spacings -> {1,1}] 

]


Esercizio2[]:=
Module[{catAd = "",catOp = "",catOp2= "",A= ""},

datiEsercizio2 = StringJoin[Style["B = 30 \ntan(\[Beta]) = \!\(\*FractionBox[\(3\), \(5\)]\)",FontFamily-> "OpenDyslexic",Bold],Style["\nTrovare A",FontColor->Red,FontFamily-> "OpenDyslexic",Bold]];

Grid[{{Text[Style["Esercizio 2:",20,FontColor-> Red,FontFamily-> "OpenDyslexic"]]},

 (* Disegno il triangolo  *)
 {Magnify[Graphics[{

 
  (* ANGOLO RETTANGOLO *)
  {Opacity[0.2],Darker[Green,0.3],Polygon[{{0, 0.1}, {0.1, 0.1}, {0.1, 0},{0, 0}}]},
  {Darker[Green,0.3],Line[{{0, 0.1}, {0.1, 0.1}, {0.1, 0}}]},
  
  (* ANGOLO IN (0.7,0) *)
  {Opacity[0.2], Darker[Green,0.3], Disk[{0.7, 0}, 0.2, angolo[{0, 0}, {0,1.3}, {0.7, 0}]+Pi]},
  {Darker[Green,0.2], Circle[{0.7, 0}, 0.2, angolo[{0, 0}, {0,1.3}, {0.7, 0}]+Pi]},

  (* ANGOLO IN (0,1.3) *)
  {Opacity[0.2], Darker[Green,0.3], Disk[{0, 1.3}, 0.2, {angolo[{0.7, 0}, {0, 0}, {0, 1.3}][[1]]+2Pi,3Pi/2}]},
  {Darker[Green,0.2], Circle[{0, 1.3}, 0.2, {angolo[{0.7, 0}, {0, 0}, {0, 1.3}][[1]]+2Pi,3Pi/2}]},
  
  (* triangle *)
  {Opacity[0],EdgeForm[Directive[Black]],Triangle[{{0, 0}, {0,1.3}, {0.7, 0}}]},  

  (* labels *)
   Rotate[
       Text[Style["A", 15,FontFamily-> "OpenDyslexic"],
       {-0.1, 0.5}], 0 Degree],
       Text[Style["B", 15,FontFamily-> "OpenDyslexic"],
       {0.3, -0.1}],
       Text[Style["C", 15,FontFamily-> "OpenDyslexic"],
       {0.5, 0.6}, {-1, 0}],
        Text[Style["\[Beta]",10, Darker[Green,0.3],FontFamily-> "OpenDyslexic"],{0.085,1.0}],
        Text[Style["\[Alpha]", 10,Darker[Green,0.3],FontFamily-> "OpenDyslexic"], {0.6, 0.08}],
        Text[Style["90\[Degree]",10,Darker[Green,0.3],FontFamily-> "OpenDyslexic"],{0.15,0.15}]
  }],2],
Magnify[Apply[StringJoin,ToString[#,StandardForm]&/@datiEsercizio2],1.4]},

 {Text[Style["Procedimento:",17,FontColor -> Red,FontFamily-> "OpenDyslexic"]]},

 (* Viene stampato il suggerimento *)
 {Text[Style["Ricorda! tan\[Beta] =\!\(\*FractionBox[\(\(\\\ \)\(cateto\\\ opposto\)\), \(cateto\\\ adiacente\)]\)",Bold,20,FontFamily-> "OpenDyslexic"]]},

 (* Prima domanda *)
 {Text[Style["Qual \[EGrave] il cateto adiacente?",Bold,20,FontFamily-> "OpenDyslexic"]],
              Magnify[RadioButtonBar[Dynamic[catAd],{"A","B","C"}],2],
              Magnify[Dynamic[CheckAnswer[catAd,"A"]],3]},

 (* Serconda domanda *)
 {Text[Style["Qual \[EGrave] il cateto opposto?",Bold,20,FontFamily-> "OpenDyslexic"]],
             Magnify[RadioButtonBar[Dynamic[catOp],{"A","B","C"}],2],
             Magnify[Dynamic[CheckAnswer[catOp,"B"]],3]},

 (* Terza domanda *)
 {Magnify[Row[{Text[Style["tan\[Beta] = ",FontFamily-> "OpenDyslexic"]],
          Row[{InputField[Dynamic[catOp2],String,FieldSize->1],Dynamic[CheckAnswer[catOp2,"B"]]}]/Row[{InputField[Dynamic[catAd2],String,FieldSize->1],Dynamic[CheckAnswer[catAd2,"A"]]}],
          Text[Style["\[LongRightArrow] A tan(\[Beta]) = B \[LongRightArrow] A = \!\(\*FractionBox[\(\(\\\ \)\(B\)\), \(tan \((\[Beta])\)\)]\)",FontFamily-> "OpenDyslexic"]]}],2]},

 (* Quarta domanda *)
 {Text[Style["Quanto vale A?",Bold,20,FontFamily-> "OpenDyslexic"]],
       Magnify[RadioButtonBar[Dynamic[A],{"50","18","2"}],2],
       Magnify[Dynamic[CheckAnswer[A,"50"]],3]}
 
},Alignment-> {Left,Center},Spacings -> {10,5}] 
]


Esercizio3[]:=
Module[{},

Grid[{{Text[Style["Esercizio 3:",20,FontColor-> Red,FontFamily-> "OpenDyslexic"]]},
(* Disegno del triangolo inscritto nella circonferenza*)
{Graphics[{

(* CIRCONFERENZA *)
{Lighter[Gray,0.5],Circle[{0,0},1]},

{Opacity[0.2],Darker[Green,0.3],Thick,Disk[pc,0.2, angolo[pa,pb, pc]]},
{Darker[Green,0.2],Thick,Circle[pc,0.2, angolo[pa,pb, pc]]},


(* TRIANGOLO *)
{Opacity[0],Black ,EdgeForm[Black], Triangle[{{Cos[(Pi/2)+0.3], Sin[(Pi/2)+0.3]},{Cos[0-0.2], Sin[0-0.2]},{Cos[Pi+0.5], Sin[Pi+0.5]}}]},

(* RAGGIO *)
{Black,Dashing[Medium],Line[{{0,0},pb}]},

(* CORDA *)
{Red,Thick,Line[{{Cos[(Pi/2)+0.3], Sin[(Pi/2)+0.3]},{Cos[0-0.2], Sin[0-0.2]}}]},

{Black,Disk[{0, 0},0.02]},
(* PUNTI *)
(* A *)
{Black,Disk[{Cos[(Pi/2)+0.3], Sin[(Pi/2)+0.3]},0.02]},
(* B *)
{Black,Disk[{Cos[0-0.2], Sin[0-0.2]},0.02]},
(* C *)
{Black,Disk[{Cos[Pi+0.5], Sin[Pi+0.5]},0.02]},

(* TESTO *)
Text[Style["  A",FontFamily-> "OpenDyslexic"],{Cos[(Pi/2)+0.4], Sin[(Pi/2)]}],
Text[Style["   B",FontFamily-> "OpenDyslexic"],{Cos[0], Sin[0-0.3]}],
Text[Style["   C",FontFamily-> "OpenDyslexic"],{Cos[Pi+0.2], Sin[Pi+0.6]}],
Text[Style["\[Theta]",Darker[Green,0.2],FontFamily-> "OpenDyslexic"],{Cos[Pi+0.9],Sin[11/6 Pi+0.2]}],
Text[Style["r",FontFamily-> "OpenDyslexic"],{0.4,-0.15}]

},PlotRange->1,ImageSize-> 400,BaseStyle->{15},Axes->False,PlotRangePadding->0.25]

 (* Vengono stampati i dati dell'esercizio *)
,Magnify[Row[{"r = 2\n\[Theta] = \!\(\*SuperscriptBox[\(60\), \(o\)]\)\n",Style["Trovare \!\(\*OverscriptBox[\(AB\), \(_\)]\)",FontColor->Red,FontFamily-> "OpenDyslexic",Bold]}],2]},
{Text[Style["Procedimento:",17,FontColor -> Red,FontFamily-> "OpenDyslexic"]]},

(* Risoluzione dell'esercizio *)
{Magnify[ Panel[
          Row[{Text[Style["\!\(\*OverscriptBox[\(AB\), \(_\)]\) = 2\[CenterDot] ",FontFamily-> "OpenDyslexic"]],
               Row[{InputField[Dynamic[raggio],String,FieldSize->1],Dynamic[CheckAnswer[raggio,"2"]]}],
               Style[" \[CenterDot] sen(",FontFamily-> "OpenDyslexic"],
               Row[{InputField[Dynamic[angolo],String,FieldSize->1.8],"\!\(\*SuperscriptBox[\(\\\ \), \(o\)]\)",Dynamic[CheckAnswer[angolo,"60"]]}],
               ") = ",
               Row[{InputField[Dynamic[AB],String,FieldSize->1],Dynamic[CheckAnswer[AB,"4"]]}],
               "\[CenterDot]",
               Row[{SqrtBox[InputField[Dynamic[num],String,FieldSize->1]]// DisplayForm,Dynamic[CheckAnswer[num,"3"]]}] /Row[{InputField[Dynamic[den],String,FieldSize->1],Dynamic[CheckAnswer[den,"2"]]}],
               " = ",
               Row[{InputField[Dynamic[coef],String,FieldSize->1],Dynamic[CheckAnswer[coef,"2"]]}],
               " \[CenterDot]",
               Row[{SqrtBox[InputField[Dynamic[coef2],String,FieldSize->1]]// DisplayForm,Dynamic[CheckAnswer[coef2,"3"]]}]
}]],2]}
},Alignment-> {Left,Center},Spacings -> {10,5}] 
]


Esercizio4[]:=
Module[{AB2=""},

Grid[{{Text[Style["Esercizio 4:",20,FontColor-> Red,FontFamily-> "OpenDyslexic"]]},
(*Disegno del triangolo inscritto nella circonferenza *)
{Graphics[{

(* CIRCONFERENZA *)
{Lighter[Gray,0.5],Circle[{0,0},1]},

{Opacity[0.2],Darker[Green,0.3],Thick,Disk[pc,0.2, angolo[pa,pb, pc]]},
{Darker[Green,0.2],Thick,Circle[pc,0.2, angolo[pa,pb, pc]]},

(* TRIANGOLO *)
{Opacity[0],Black ,EdgeForm[Black], Triangle[{{Cos[(Pi/2)+0.3], Sin[(Pi/2)+0.3]},{Cos[0-0.2], Sin[0-0.2]},{Cos[Pi+0.5], Sin[Pi+0.5]}}]},

(* RAGGIO *)
{Black,Dashing[Medium],Line[{{0,0},pb}]},

(* CORDA *)
{Red,Thick,Line[{{Cos[(Pi/2)+0.3], Sin[(Pi/2)+0.3]},{Cos[0-0.2], Sin[0-0.2]}}]},

{Black,Disk[{0, 0},0.02]},
(* PUNTI *)
(* A *)
{Black,Disk[{Cos[(Pi/2)+0.3], Sin[(Pi/2)+0.3]},0.02]},
(* B *)
{Black,Disk[{Cos[0-0.2], Sin[0-0.2]},0.02]},
(* C *)
{Black,Disk[{Cos[Pi+0.5], Sin[Pi+0.5]},0.02]},

(* TESTO *)
Text[Style["  A",FontFamily-> "OpenDyslexic"],{Cos[(Pi/2)+0.4], Sin[(Pi/2)]}],
Text[Style["   B",FontFamily-> "OpenDyslexic"],{Cos[0], Sin[0-0.3]}],
Text[Style["   C",FontFamily-> "OpenDyslexic"],{Cos[Pi+0.2], Sin[Pi+0.6]}],
Text[Style["\[Theta]",Darker[Green,0.3],FontFamily-> "OpenDyslexic"],{Cos[Pi+0.9],Sin[11/6 Pi+0.2]}],
Text[Style["r",FontFamily-> "OpenDyslexic"],{0.4,-0.15}]

},PlotRange->1,ImageSize-> 400,BaseStyle->{15},Axes->False,PlotRangePadding->0.25]

 (* Dati del problema*)
,Magnify[Row[{"r = 5\n\[Theta] = \!\(\*SuperscriptBox[\(60\), \(o\)]\)\n",
              Style["Trovare \!\(\*OverscriptBox[\(AB\), \(_\)]\)",FontColor->Red,FontFamily-> "OpenDyslexic",Bold]}]           
        ,2],
 Text[""]},
 
 (* Domanda a risposta multipla *)
{Text[Style["Quindi il lato \!\(\*OverscriptBox[\(AB\), \(_\)]\) misura:",17,FontColor -> Red,FontFamily-> "OpenDyslexic"]]},

{Magnify[RadioButtonBar[Dynamic[AB2],{"\!\(\*FractionBox[\(5\), \(2\)]\)","\!\(\*FractionBox[\(5\), \(2\)]\)\!\(\*SqrtBox[\(3\)]\)","5(\*SqrtBox[\(3\)])","30"}],2],
 Magnify[Dynamic[CheckAnswer[AB2,"5(\*SqrtBox[\(3\)])"]],3]}

},Alignment-> {Left,Center},Spacings -> {10,5}] 
]


Esercizio8[]:=
Module[{res5 ="",val13="",val14=""},
datiEsercizio8 = StringJoin[Style["A = 24 \nC = 12\!\(\*SqrtBox[\(3\)]\)\nB = 12",FontFamily-> "OpenDyslexic",Bold],Style["\nTrovare cos(\[Gamma])",FontColor->Red,FontFamily-> "OpenDyslexic",Bold]];


Grid[{{Text[Style["Esercizio 8:",20,FontColor-> Red,FontFamily-> "OpenDyslexic"]]},

(* Disegno del triangolo *)
{Magnify[Graphics[{


(* ARCO SU (1,0) *)
{Opacity[0.2],Darker[Green,0.3],Thick,Disk[{1, 0}, 0.15, angolo[{-0.2, 0}, {-0.5, 1}, {1, 0}]+Pi]},
{Darker[Green,0.2],Circle[{1, 0}, 0.15, angolo[{-0.2, 0}, {-0.5, 1}, {1, 0}]+Pi]},

 (* triangle *)
{Opacity[0],EdgeForm[Directive[Black]], Triangle[{{-0.2, 0}, {-0.5, 1}, {1, 0}, {-0.2, 0}}]},

 (* labels *)
  Rotate[
   Text[Style["C", 10,FontFamily-> "OpenDyslexic"],
    {-0.4, 0.4}], 0 Degree],
  Text[Style["A", 10,FontFamily-> "OpenDyslexic"],
   {0.3, -0.1}],
  Text[Style["B", 10,FontFamily-> "OpenDyslexic"],
   {0.2, 0.6}, {-0.4, 0}],
  Text[Style["\[Gamma]",10,Darker[Green,0.3],FontFamily-> "OpenDyslexic"],{0.8,0.08}]
  }],2]
  
(* Vengono stampati i dati dell'esercizio *)
,Magnify[Apply[StringJoin,ToString[#,StandardForm]&/@datiEsercizio8],1.4],Text[""]},
{Text[Style["Procedimento:",17,FontColor -> Red,FontFamily-> "OpenDyslexic"]]},

(* Suggerimento del teorema  di Carnot *)
{Text[Style["Ricorda! \!\(\*SuperscriptBox[\(C\), \(2\)]\) = \!\(\*SuperscriptBox[\(B\), \(2\)]\) + \!\(\*SuperscriptBox[\(A\), \(2\)]\) - 2A\[CenterDot]B cos(\[Gamma])",Bold,20,FontFamily-> "OpenDyslexic"]]},

(* Primo passaggio dell'esercizio *)
{Magnify[Row[{"cos(\[Gamma]) = \!\(\*FractionBox[\(\(\\\ \)\(\*SuperscriptBox[\(B\), \(2\)]\\\  + \\\ \*SuperscriptBox[\(A\), \(2\)]\\\  - \\\ \*SuperscriptBox[\(C\), \(2\)]\)\), \(2  A\[CenterDot]B\)]\) = ",
             Row[{InputField[Dynamic[val13],String,FieldSize->2],Dynamic[CheckAnswer[val13,"12"]],"\!\(\*SuperscriptBox[\(\\\ \), \(2\)]\) + 576 - 432"}]/Row[{"2 \[CenterDot]", InputField[Dynamic[val14],String,FieldSize->3],Dynamic[CheckAnswer[val14,"288"]]}] 
             }],2]},
 
(* Conclusione delle'esercizio *)            
{Style["Quindi cos(\[Gamma]) misura:",17,Bold,FontFamily-> "OpenDyslexic"],
 Magnify[Row[{RadioButtonBar[Dynamic[res5],{"\!\(\*FractionBox[\(1\), \(2\)]\)","\!\(\*FractionBox[SqrtBox[\(3\)], \(2\)]\)","-\!\(\*FractionBox[\(1\), \(2\)]\)","-\!\(\*FractionBox[SqrtBox[\(3\)], \(2\)]\)"}],"         ",Dynamic[CheckAnswer[res5,"\!\(\*FractionBox[\(1\), \(2\)]\)"]]
         }],2]}


},Alignment->{Left,Center},Spacings -> {10,5}]]


Esercizio5[] := 
Module[{esatt3 = ""},

datiEsercizio5 = StringJoin[Style["A = 6 \n\[Alpha] = \!\(\*SuperscriptBox[\(30\), \(o\)]\)\n\[Beta] = \!\(\*SuperscriptBox[\(105\), \(o\)]\)",FontFamily-> "OpenDyslexic",Bold],Style["\nTrovare C",FontColor->Red,FontFamily-> "OpenDyslexic",Bold]];

Grid[{{Text[Style["Esercizio 5:",20,FontColor-> Red,FontFamily-> "OpenDyslexic"]]},
(* Disegno il triangolo *)
{Magnify[Graphics[{

(* ARCO SU (0,0) *)
{Opacity[0.2],Darker[Green,0.3],Disk[{0, 0}, 0.12, {angolo[{0.7, 0}, {-1,1}, {0, 0}][[2]]+Pi, 0} ]},
{Darker[Green,0.2],Circle[{0, 0} ,0.12, {angolo[{0.7, 0}, {-1,1}, {0, 0}][[2]]+Pi, 0}]},

(* ARCO SU (-1,1) *)
{Opacity[0.2],Darker[Green,0.3],Disk[{-1,1}, 0.2, angolo[{0, 0}, {0.7, 0}, {-1,1}]]},
{Darker[Green,0.2],Circle[{-1,1}, 0.2, angolo[{0, 0}, {0.7, 0}, {-1,1}]]},


   (* triangle *)
 {Opacity[0],EdgeForm[Directive[Black]], Triangle[{{0, 0}, {-1,1}, {0.7, 0}}]},
 
  (* labels *)
  Rotate[
   Text[Style["C", 10,FontFamily-> "OpenDyslexic"],
    {-0.6, 0.4}], 0 Degree],
  Text[Style["A", 10,FontFamily-> "OpenDyslexic"],
   {0.3, -0.1}],
  Text[Style["B", 10,FontFamily-> "OpenDyslexic"],
   {0.1, 0.6}, {-0.5, 0}],
  Text[Style["\[Alpha]",10,Darker[Green,0.2],FontFamily-> "OpenDyslexic"],{-0.8,1.0}],
  Text[Style["\[Beta]",10,Darker[Green,0.2],FontFamily-> "OpenDyslexic"],{0.1,0.15}]
  }],2]
  
 (* Stampo i dati dell'esercizio *) 
,Magnify[Apply[StringJoin,ToString[#,StandardForm]&/@datiEsercizio5],1.4],Text[""]},
{Text[Style["Procedimento:",17,FontColor -> Red,FontFamily-> "OpenDyslexic"]]},

(* Primo passo dell'esercizio *)
{Magnify[Row[{Text[Style["Calcolare l'ampiezza dell'angolo \[Gamma]:\!\(\*SuperscriptBox[\(180\), \(o\)]\)-(",FontFamily-> "OpenDyslexic"]],
               InputField[Dynamic[ott],String,FieldSize->4],
               Style["\!\(\*SuperscriptBox[\(\\\ \), \(o\)]\)",FontFamily-> "OpenDyslexic"],
               (* Dynamic[CheckAnswer[ott,"105"]], *)
               
               Dynamic[esatt3],
               
               Style["+",FontFamily-> "OpenDyslexic"],
               InputField[Dynamic[al],String,FieldSize->2],
               "\!\(\*SuperscriptBox[\(\\\ \), \(o\)]\)",
               (* Dynamic[CheckAnswer[al,"30"]], *)
              
               (* Per la propriet\[AGrave] commutativa *)
               Dynamic[  If[(ott == "") || (al == ""),
                           esatt3 = Text[""];
                           Text[""],
                           If[(ott == "105" && al == "30") || (ott == "30" && al == "105"),
                              esatt3 = Style["\[Checkmark]",FontColor->Green];                    
                              Style["\[Checkmark]",FontColor->Green],
                              esatt3 = Style["X",FontColor->Red,Bold];
                              Style["X",FontColor->Red,Bold],
                              esatt3 = Text[""];
                              Text[""]],
                           esatt3 = Text[""];   
                           Text[""]]   
               ],
               
               Style[") = ",FontFamily-> "OpenDyslexic"],
               InputField[Dynamic[res2],String,FieldSize->2],
               Dynamic[CheckAnswer[res2,"45"]],
               Style["\!\(\*SuperscriptBox[\(\\\ \), \(o\)]\)",FontFamily-> "OpenDyslexic"]}],
 
 1.5],SpanFromLeft},
(* Suggerimento *)
{Magnify[Text[Style["Utilizzare la relazione \!\(\*FractionBox[\(\(A\)\(\\\ \)\), \(sen \((\[Alpha])\)\)]\) =\!\(\*FractionBox[\(\(\\\ \)\(C\)\), \(sen \((\[Gamma])\)\)]\)",FontFamily-> "OpenDyslexic"]],1.5]},


(* Secondo passo dell'esercizio *)
{Text[""],
 Magnify[  Panel[
           Row[{Style["6",FontFamily-> "OpenDyslexic"]/Row[{Style["sen(",FontFamily-> "OpenDyslexic"],InputField[Dynamic[al2],String,FieldSize->2],"\!\(\*SuperscriptBox[\(\\\ \), \(o\)]\))",Dynamic[CheckAnswer[al2,"30"]]}],
           Style[" = ",FontFamily-> "OpenDyslexic"],
           Row[{InputField[Dynamic[c],String,FieldSize->2],Dynamic[CheckAnswer[c,"C"]]}]/(Style["sen(\!\(\*SuperscriptBox[\(45\), \(o\)]\))",FontFamily-> "OpenDyslexic"]),Style[" \[LongRightArrow] ",FontFamily-> "OpenDyslexic"], Style[" 6 / ",FontFamily-> "OpenDyslexic"],
           Row[{InputField[Dynamic[num3],String,FieldSize->1],Dynamic[CheckAnswer[num3,"1"]]}]/Row[{InputField[Dynamic[den3],String,FieldSize->1],Dynamic[CheckAnswer[den3,"2"]]}],
           Style[" = ",FontFamily-> "OpenDyslexic"],
           Row[{InputField[Dynamic[num4],String,FieldSize->1],Dynamic[CheckAnswer[num4,"C"]]}],
           Style[" /\!\(\*FractionBox[\(\(\\\ \)\*SqrtBox[\(2\)]\), \(2\)]\)",FontFamily-> "OpenDyslexic"]}]],
           
           2]},


 (* Terzo passo dell'esercizio *)
{Magnify[Text[Style["Ricavare:",FontFamily-> "OpenDyslexic"]],1.5]},
{Text[""],
Magnify[  Panel[
          Row[{Style["C = 6",FontFamily-> "OpenDyslexic"],
          Row[{SqrtBox[InputField[Dynamic[num5],String,FieldSize->1]]//DisplayForm,Dynamic[CheckAnswer[num5,"2"]]}]/Row[{InputField[Dynamic[den5],String,FieldSize->1],Dynamic[CheckAnswer[den5,"2"]]}],
          " / ",
          Row[{InputField[Dynamic[num6],String,FieldSize->1],Dynamic[CheckAnswer[num6,"1"]]}]/Row[{InputField[Dynamic[den6],String,FieldSize->1],Dynamic[CheckAnswer[den6,"2"]]}],
          " = ",
          InputField[Dynamic[res3],String,FieldSize->1],
          Dynamic[CheckAnswer[res3,"6"]],
          SqrtBox[Row[{InputField[Dynamic[res41],String,FieldSize->1],
          Dynamic[CheckAnswer[res41,"2"]]}]] // DisplayForm }]],2
          
]}



},Alignment->{Left,Center},Spacings -> {10,5}]
]

(*Esercizio 6*)
Esercizio6[]:=
Module[{res4=""},
datiEsercizio6 = StringJoin[Style["A = 12 \nB = 9\n\[Beta] = \!\(\*SuperscriptBox[\(30\), \(o\)]\)",FontFamily-> "OpenDyslexic",Bold],Style["\nTrovare sen(\[Alpha])",FontColor->Red,FontFamily-> "OpenDyslexic",Bold]];

Grid[{{Text[Style["Esercizio 6:",20,FontColor-> Red,FontFamily-> "OpenDyslexic"]]},

(* Disegno del triangolo *)
{Magnify[Graphics[{


   (* ANGOLO IN (0,0) *)
  {Opacity[0.2], Darker[Green,0.3], Disk[{0, 0}, 0.2, angolo[{2, 0}, {1.3, 1}, {0, 0}]]},
  {Darker[Green,0.2], Circle[{0, 0}, 0.2, angolo[{2, 0}, {1.3, 1}, {0, 0}]]},
  
  (* ANGOLO IN (2,0) *)
  {Opacity[0.2], Darker[Green,0.3], Disk[{2, 0}, 0.2, angolo[{0, 0}, {1.3, 1}, {2, 0}]+Pi]},
  {Darker[Green,0.2], Circle[{2, 0}, 0.2, angolo[{0, 0}, {1.3,1}, {2, 0}]+Pi]},

  (* ANGOLO IN (1.3,1) *)
  {Opacity[0.2], Darker[Green,0.3], Disk[{1.3, 1}, 0.2, {angolo[{2, 0}, {0, 0}, {1.3, 1}][[1]], angolo[{2, 0}, {0, 0}, {1.3, 1}][[2]]-Pi}]},
  {Darker[Green,0.2], Circle[{1.3, 1}, 0.2, {angolo[{2, 0}, {0, 0}, {1.3, 1}][[1]], angolo[{2, 0}, {0, 0}, {1.3, 1}][[2]]-Pi}]},
  
   (* triangle *)
 {Opacity[0],EdgeForm[Directive[Black]], Triangle[{{0, 0}, {1.3,1}, {2, 0}}]},
 
  (* labels *)
  Rotate[
   Text[Style["B", 10,FontFamily-> "OpenDyslexic"],
    {0.6, 0.6}], 0 Degree],
  Text[Style["C", 10,FontFamily-> "OpenDyslexic"],
   {1, -0.1}],
  Text[Style["A", 10,FontFamily-> "OpenDyslexic"],
   {1.65, 0.6}, {-1, 0}],
  Text[Style["\[Beta]", 10,Darker[Green,0.3],FontFamily-> "OpenDyslexic"], {1.7, 0.1}],
  Text[Style["\[Gamma]",10,Darker[Green,0.3],FontFamily-> "OpenDyslexic"],{1.3,0.75}],
  Text[Style["\[Alpha]",10,Darker[Green,0.3],FontFamily-> "OpenDyslexic"],{0.3, 0.1}]
  }],2]

 (* Stampo i dati dell'esercizio *)
,Magnify[Apply[StringJoin,ToString[#,StandardForm]&/@datiEsercizio6],1.4],Text[""]},
{Text[Style["Procedimento:",17,FontColor -> Red,FontFamily-> "OpenDyslexic"]]},

(* Suggerimento *)
{Text[Style["Ricorda! \!\(\*FractionBox[\(A\), \(sen \((\[Alpha])\)\)]\) =\!\(\*FractionBox[\(\(B\)\(\\\ \)\), \(sen \((\[Beta])\)\)]\) =\!\(\*FractionBox[\(\(\\\ \)\(C\)\), \(sen \((\[Gamma])\)\)]\)",FontSize->17,Bold,FontFamily-> "OpenDyslexic"]]},

(* Domanda a risposta multipla*)
{Text[Style["Quanto misura sen(\[Alpha])?",Bold,20,FontFamily-> "OpenDyslexic"]],
 Magnify[RadioButtonBar[Dynamic[res4],{"\!\(\*FractionBox[\(3\), \(2\)]\)","\!\(\*FractionBox[\(2\), \(3\)]\)","\!\(\*FractionBox[\(2\), \(3\)]\)\!\(\*SqrtBox[\(3\)]\)","\!\(\*FractionBox[\(3\), \(2\)]\)\!\(\*SqrtBox[\(3\)]\)"}],2],
 Magnify[Dynamic[CheckAnswer[res4,"\!\(\*FractionBox[\(2\), \(3\)]\)"]],3]}

},Alignment->{Left,Center},Spacings -> {10,5}]
]

(*Esercizio 9*)
Esercizio9[]:=
Module[{},


Grid[{{Text[Style["Esercizio 9:",20,FontColor-> Red,FontFamily-> "OpenDyslexic"]]},
      
      
       (* Stampo foto esercizio *)
      {Magnify[Sharpen[Import["campanile.jpeg"]],5],
       Magnify[
         Row[{Style["Trovare H, dove H \[EGrave] l'altezza del campanile\n",FontFamily-> "OpenDyslexic",FontColor->Red,FontFamily-> "OpenDyslexic"],
            Style["Approssimare il risultato per difetto\n",FontFamily-> "OpenDyslexic",FontColor->Red,8],
            Style["tan(",FontFamily-> "OpenDyslexic"],InputField[Dynamic[tang],String,FieldSize->1],
            Dynamic[CheckAnswer[tang,"42"]],"\!\(\*SuperscriptBox[\(\\\ \), \(\[Degree]\)]\)) = ", 
            Row[{InputField[Dynamic[altezza],String,FieldSize->1],Dynamic[CheckAnswer[altezza,"H"]]}]/Row[{InputField[Dynamic[base],String,FieldSize->1],Dynamic[CheckAnswer[base,"80"]]}],
            Style[" \nH = ",FontFamily-> "OpenDyslexic"],Row[{InputField[Dynamic[base2],String,FieldSize->1],Dynamic[CheckAnswer[base2,"80"]]}],
            Style[" \[CenterDot] tan(",FontFamily-> "OpenDyslexic"],
            Row[{InputField[Dynamic[altezza2],String,FieldSize->1],Dynamic[CheckAnswer[altezza2,"42"]]}],
            "\!\(\*SuperscriptBox[\(\\\ \), \(\[Degree]\)]\)) = ",
            Row[{InputField[Dynamic[altezza3],String,FieldSize->1],Dynamic[CheckAnswer[altezza3,"72"]]}]}],
       2]}

},Alignment->{Left,Center},Spacings -> {10,5}]
]

(*Esercizio 7*)
Esercizio7[] := 
Module[{G7="",esatt = "",esatt2 = ""},

datiEsercizio7 = StringJoin[Style["A = 2 \nB = 3\n\[Gamma] = \!\(\*SuperscriptBox[\(60\), \(o\)]\)",FontFamily-> "OpenDyslexic",Bold],Style["\nTrovare C",FontColor->Red,FontFamily-> "OpenDyslexic",Bold]];

Grid[{{Text[Style["Esercizio 7:",20,FontColor-> Red,FontFamily-> "OpenDyslexic"]]},
{Magnify[Graphics[{

   (* ANGOLO IN (0,0) *)
  {Opacity[0.2], Darker[Green,0.3], Disk[{0, 0}, 0.2, angolo[{1.8, 0}, {0.8, 1}, {0, 0}]]},
  {Darker[Green,0.2], Circle[{0, 0}, 0.2, angolo[{1.8, 0}, {0.8, 1}, {0, 0}]]},
  
  (* ANGOLO IN (1.8,0) *)
  {Opacity[0.2], Darker[Green,0.3], Disk[{1.8, 0}, 0.2, angolo[{0, 0}, {0.8, 1}, {1.8, 0}]+Pi]},
  {Darker[Green,0.2], Circle[{1.8, 0}, 0.2, angolo[{0, 0}, {0.8,1}, {1.8, 0}]+Pi]},

  (* ANGOLO IN (0.8,1) *)
  {Opacity[0.2], Darker[Green,0.3], Disk[{0.8, 1}, 0.2, {angolo[{1.8, 0}, {0, 0}, {0.8, 1}][[1]], angolo[{1.8, 0}, {0, 0}, {0.8, 1}][[2]]-Pi}]},
  {Darker[Green,0.2], Circle[{0.8, 1}, 0.2, {angolo[{1.8, 0}, {0, 0}, {0.8, 1}][[1]], angolo[{1.8, 0}, {0, 0}, {0.8, 1}][[2]]-Pi}]},
  
   (* triangle *)
 {Opacity[0],EdgeForm[Directive[Black]], Triangle[{{0, 0}, {0.8,1}, {1.8, 0}}]},
  (* labels *)
  Rotate[
   Text[Style["B", 10,FontFamily-> "OpenDyslexic"],
    {0.3, 0.6}], 0 Degree],
  Text[Style["C", 10,FontFamily-> "OpenDyslexic"],
   {0.7, -0.1}],
  Text[Style["A", 10,FontFamily-> "OpenDyslexic"],
   {1.4, 0.6}, {-1, 0}],
 Text[Style["\[Beta]", 10, 10,Darker[Green,0.3],FontFamily-> "OpenDyslexic"], {1.5, 0.1}],
 Text[Style["\[Gamma]",10, 10,Darker[Green,0.3],FontFamily-> "OpenDyslexic"],{0.8,0.75}],
 Text[Style["\[Alpha]",10, 10,Darker[Green,0.3],FontFamily-> "OpenDyslexic"],{0.3,0.1}]
  }],2]
  
 (* Stampo i dati dell'esercizio *) 
,Magnify[Apply[StringJoin,ToString[#,StandardForm]&/@datiEsercizio7],1.4],Text[""]},
{Text[Style["Procedimento:",17,FontColor -> Red,FontFamily-> "OpenDyslexic"]]},

(* Primo passo dell'esercizio *)
{Text[Style["Applicare il teorema del coseno:",Bold,20,FontFamily-> "OpenDyslexic"]],
 Magnify[Row[{"\!\(\*SuperscriptBox[\(C\), \(2\)]\) = ",
           InputField[Dynamic[A7],String,FieldSize->1],
           "\!\(\*SuperscriptBox[\(\\\ \), \(2\)]\)",
          (* Dynamic[CheckAnswer[A7,"A"]], *)
           Dynamic[esatt],
           " + ",
           InputField[Dynamic[B7],String,FieldSize->1],
           "\!\(\*SuperscriptBox[\(\\\ \), \(2\)]\)",
           (* Dynamic[CheckAnswer[B7,"B"]], *)
           
           (* Per la propriet\[AGrave] commutativa *)
            Dynamic[  If[(A7 == "") || (B7 == ""),
                           esatt = Text[""];   
                           Text[""],
                           If[(A7 == "A" && B7 == "B") || (A7 == "B" && B7 == "A"),
                              esatt = Style["\[Checkmark]",FontColor->Green];
                              Style["\[Checkmark]",FontColor->Green],
                              esatt = Style["X",FontColor->Red,Bold];     
                              Style["X",FontColor->Red,Bold],
                              esatt = Text[""];     
                              Text[""]],
                           esatt = Text[""];      
                           Text[""]]   
               ],
           
           " - 2 \[CenterDot] A \[CenterDot] " ,
           InputField[Dynamic[B8],String,FieldSize->1],
           Dynamic[CheckAnswer[B8,"B"]],
           " \[CenterDot] cos(",
           PopupMenu[Dynamic[G7],{"\[Alpha]","\[Beta]","\[Gamma]"}],Dynamic[CheckAnswer[G7,"\[Gamma]"]],")" }]]},
           
(* Secondo passo dell'esercizio *)           
{Text[Style["Quindi, sostituendo i valori numerici:",Bold,20,FontFamily-> "OpenDyslexic"]],
 Magnify[Row[{"\!\(\*SuperscriptBox[\(C\), \(2\)]\) = ",
          InputField[Dynamic[Val7],String,FieldSize->1],
          "\!\(\*SuperscriptBox[\(\\\ \), \(2\)]\)",
          (* Dynamic[CheckAnswer[Val7,"2"]], *)
          Dynamic[esatt2],
          " + ",
          InputField[Dynamic[Val8],String,FieldSize->1],
          "\!\(\*SuperscriptBox[\(\\\ \), \(2\)]\)",
          (* Dynamic[CheckAnswer[Val8,"3"]], *)
          
          Dynamic[  If[(Val7 == "") || (Val8 == ""),
                           esatt2 = Text[""];   
                           Text[""],
                           If[(Val7 == "2" && Val8 == "3") || (Val7 == "3" && Val8 == "2"),
                              esatt2 = Style["\[Checkmark]",FontColor->Green];
                              Style["\[Checkmark]",FontColor->Green],
                              esatt2 = Style["X",FontColor->Red,Bold];     
                              Style["X",FontColor->Red,Bold],
                              esatt2 = Text[""];     
                              Text[""]],
                           esatt2 = Text[""];      
                           Text[""]]   
               ],
          
          " - 12 \[CenterDot] " ,
          Row[{InputField[Dynamic[Val9],String,FieldSize->1],Dynamic[CheckAnswer[Val9,"1"]]}]/Row[{InputField[Dynamic[Val11],String,FieldSize->1],Dynamic[CheckAnswer[Val11,"2"]]}],
          " = ",
          InputField[Dynamic[Val10],String,FieldSize->1],
          Dynamic[CheckAnswer[Val10,"7"]] }]]},

(* Terzo passo dell'esercizio *)
{Text[Style["Da cui: ",Bold,20,FontFamily-> "OpenDyslexic"]],Magnify[Row[{"C = ",SqrtBox[Row[{InputField[Dynamic[Val12],String,FieldSize->1],Dynamic[CheckAnswer[Val12,"7"]]}]]}] // DisplayForm,2]}


},Alignment->{Left,Center},Spacings -> {10,5}]
]

(*Calcolatrice*)
Calcolatrice[]:=
Module[{},
espressione = "";
CreateDialog[{Magnify[InputField[Dynamic[espressione],String,Alignment->Right,FieldSize-> 17],2],
             
                       Grid[{{
                      Column[{
                         Row[ {Button[" 1 ", espressione = StringJoin[espressione ,"1"],FrameMargins->7]," ",
                                 Button[" 2 ",espressione = StringJoin[espressione ,"2"],FrameMargins->7]," ",
                                 Button[" 3 ",espressione = StringJoin[espressione ,"3"],FrameMargins->7]}],
                         Row[ {Button[" 4 ",espressione = StringJoin[espressione ,"4"],FrameMargins->7]," ",
                                 Button[" 5 ",espressione = StringJoin[espressione ,"5"],FrameMargins->7]," ",
                                 Button[" 6 ",espressione = StringJoin[espressione ,"6"],FrameMargins->7]}],
                        Row[ {Button[" 7 ",espressione = StringJoin[espressione ,"7"],FrameMargins->7]," ",
                                 Button[" 8 ",espressione = StringJoin[espressione ,"8"],FrameMargins->7]," ",
                                 Button[" 9 ",espressione = StringJoin[espressione ,"9"],FrameMargins->7]}],
                          Row[ {Button[" C ",If[StringLength[espressione]>0,espressione =StringDrop[espressione,-1],espressione = ""],FrameMargins->7]," ",
                                 Button[" 0 ",espressione = StringJoin[espressione ,"0"],FrameMargins->7]," ",
                                 Button[" = ",If[StringLength[espressione]>0,{tmp =StandardForm[ ToExpression[espressione]],espressione = ToString[tmp]}],FrameMargins->7]}]}
                  ]," ",
                       Column[{
                                 Button[" + ", espressione = StringJoin[espressione ,"+"],FrameMargins->7],
                                 Button[" - ",espressione = StringJoin[espressione ,"-"],FrameMargins->7],
                                 Button[" * ",espressione = StringJoin[espressione ,"*"],FrameMargins->7],
                                 Button[" / ",espressione = StringJoin[espressione ,"/"],FrameMargins->7]

                  },Spacings->0.5],

                     Column[{
                           ,
                               Button[" sen ",espressione = StringJoin["Sin[",espressione ," Degree]"],FrameMargins->7],
                              Button[" cos ",espressione = StringJoin["Cos[",espressione ," Degree]"],FrameMargins->7],
                              Button[" tan ",espressione = StringJoin["Tan[",espressione ," Degree]"],FrameMargins->7],
                              Button[" \!\(\*SuperscriptBox[\(x\), \(2\)]\) ",espressione = StringJoin[espressione ,"^2"],FrameMargins->7]

                 },Spacings-> 0.5],
   Column[{
                           ,
                               Button[" \!\(\*SuperscriptBox[\(sen\), \(-1\)]\) ",espressione = StringJoin["ArcSin[",espressione ,"]"],FrameMargins->7],
                              Button[" \!\(\*SuperscriptBox[\(cos\), \(-1\)]\) ",espressione = StringJoin["ArcCos[",espressione ,"]"],FrameMargins->7],
                              Button[" \!\(\*SuperscriptBox[\(tan\), \(-1\)]\) ",espressione = StringJoin["ArcTan[",espressione ,"]"],FrameMargins->7],
                              Button["\!\(\*SqrtBox[\(\(x\)\(\\\ \)\)]\) ",espressione = StringJoin["Sqrt[",espressione ,"]"],FrameMargins->7]

                 },Spacings-> 0.5]

                     },
                      {Button["Clear",espressione = "",FrameMargins -> 7],SpanFromLeft}
                 
                 }]

}]
]

(*Teorema di pitagora*)
TPitagora[]:=Button["Teorema di Pitagora",
Module[{},

CreateDialog[{Style["Teorema di Pitagora",FontSize->14,Bold],

         Graphics[{
         (*Triangolo*)
         Line[{{0,0},{0,1},{1.5,0},{0,0}}],
   
        (*Quadrato Cateto 1*)
        {Lighter[Green,0.5],Rectangle[{0,0},{1.5,-1.5}]},
        {Darker[Green,0.5],Line[{{0,0},{1.5,0},{1.5,-1.5},{0,-1.5},{0,0}}]},
        Text[Style["\!\(\*SuperscriptBox[\(A\), \(2\)]\)",Bold,20,FontFamily-> "OpenDyslexic"],{0.8,-0.7}],
        
        (*Quadrato Cateto 2*)
        {Lighter[Red,0.5],Rectangle[{0,0},{-1,1}]},
        {Red,Line[{{0,0},{-1,0},{-1,1},{0,1},{0,0}}]},
        Text[Style["\!\(\*SuperscriptBox[\(B\), \(2\)]\)",Bold,20,FontFamily-> "OpenDyslexic"],{-0.5,0.5}],
        
        (*Quadrato ipotenusa*)
        {Lighter[Blue,0.5],Rotate[Rectangle[{0,1},{1.8,-0.8}],Pi/3 -0.06,{0,1}]},
        {Blue,Line[{{0,1},{1.5,0},{2.5,1.5},{1,2.5},{0,1}}]},
        Text[Style["\!\(\*SuperscriptBox[\(C\), \(2\)]\)",Bold,20,FontFamily-> "OpenDyslexic"],{1.3,1.3}]
}]

}]
]

]

(*Esercizio 10*)
Esercizio10[]:=
Module[{},
esercizio10testo = Text[Style["",Bold,20,FontFamily-> "OpenDyslexic"]];
Grid[{{Text[Style["Esercizio 10:",20,FontColor-> Red,FontFamily-> "OpenDyslexic"]]},

(*{esercizio10testo},*)

(* Stampo foto scivolo *)
{ Magnify[Sharpen[Import["scivolo.jpeg"]],3],

(* Svolgimento esercizio *)
Magnify[
      Row[{Style["Trovare \[Alpha]\n",FontColor->Red,FontFamily-> "OpenDyslexic"],Style["Approssima il risultato per\ndifetto alla prima cifra decimale\n\n",FontColor->Red,8,FontFamily-> "OpenDyslexic"],
      Style[""],
      Row[{InputField[Dynamic[alpha2],String,FieldSize->2],Dynamic[CheckAnswer[alpha2,"2.5"]]}]/("sen(\[Alpha])"),
      " = ",
      Row[{InputField[Dynamic[gamma3],String,FieldSize->1],Dynamic[CheckAnswer[gamma3,"3" ]]}]/Row[{"sen(",InputField[Dynamic[gamma2],String,FieldSize->1.8],
      "\[Degree])",
      Dynamic[CheckAnswer[gamma2,"60"]]}],
      " \[LongRightArrow] ",
      "sen(\[Alpha]) = ",
      Row[{InputField[Dynamic[latob],String,FieldSize->2],Dynamic[CheckAnswer[latob,"2.5"]]}]/Row[{InputField[Dynamic[latoc],String,FieldSize->1],Dynamic[CheckAnswer[latoc,"3"]]}],
      " \[CenterDot] ",
      Row[{SqrtBox[InputField[Dynamic[num10],String,FieldSize->1]]//DisplayForm,Dynamic[CheckAnswer[num10,"3"]] }] /Row[{InputField[Dynamic[den10],String,FieldSize->1],Dynamic[CheckAnswer[den10,"2"]]}],
      " = ",
      Row[{InputField[Dynamic[res10],String,FieldSize->2],Dynamic[CheckAnswer[res10,"0.7"]]}]

}],2]}

},Alignment->{Left,Center},Spacings -> {10,5}]
]


EndPackage[]
