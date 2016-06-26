#include "TF1.h"
#include "TMath.h"
#include "TGraph.h"
#include "TString.h"
#include "TCanvas.h"
#include <stdio.h>
#include <fstream>
#include <iostream>
#include <math.h>
#include "TGaxis.h"
#include "TAxis.h"
#include <string>
using namespace std;

int chargeMax = 89;
int chargeMin = 0;
int nInj = 50;
int data[256]={0};
int x   [256]={0};
int NPoints;
char fNameOut [50];
char separacion= 95;
int ELECTRONS_PER_DAC = 7; 
int fila, columna;
int NPixels=0;
int NNostart;
int NChisq;

void ResetData() {
  for (int i=0; i <= 255; i++) 
  {
    data[i] = 0;
  }
}

Double_t erf( Double_t *xx, Double_t *par){
  return (nInj / 2) *TMath::Erf((xx[0] - par[0]) / (sqrt(2) *par[1])) +(nInj / 2);
}

Float_t FindStart (int NPoints, int *xx, int *data,int col,int addr) {
  float Upper = (float)xx[NPoints-1];
  float Lower = (float)xx[0];
  int up = data[NPoints-1];
  int down = data[0];
  int reachUp = 1;
  int reachDown = 1;

  	for (int i = 2; i < NPoints+1; i ++) 
        {
        if(up==50){reachUp=0;}
        //Revisa por un flanco de bajada. Si lo encuentra se queda con el valor de la posicion.
		if (up > data[NPoints -i] ) {Upper = (float) xx[NPoints-i]; break;}
		up = data[NPoints - i];
        }

  	for (int i = 1; i < NPoints ; i++) 
        {
        if(down==0){reachDown=0;}
        //Revisa por un flanco de subida. Si lo encuentra se queda con el valor de la posicion.
		if (down < data[i] ) {Lower = (float) xx[i];break;}
		down = data[i];
	}
 
  	if ((Upper==(float)xx[NPoints-1])||(Lower==(float)xx[0])){
        //Nunca hubo flanco de subida o bajada
		if(data[0]==50){cout<<"Hot Pixel: "<< col <<" "<<addr<<endl;return -1; }
		if(data[0]==0){cout<<"Dead Pixel: "<< col <<" "<<addr<<endl;return -1; }
	}

    if (reachUp){std::cout<<"Not enough charge to activate pixel: "<<col <<" "<< addr <<std::endl;}
    if (reachDown){std::cout<<"Too much charge to start with pixel: "<<col <<" "<< addr <<std::endl;}
    //Da una idea de donde podria estar el medio.
	return (Upper + Lower)/2;

}

bool GetThreshold(double *AThreshold, double *ANoise, double *AChisq, bool *Ascurve, int col, int addr) {

   TGraph * g = new TGraph (NPoints,x,data);
   TF1 *fitfcn = new TF1("fitfcn", erf,0,500,2);
   //Valor posiblemente cercano a la media.
   Float_t Start  = FindStart(NPoints, x, data, col, addr);
   if (Start == -1) { //Dead or Hot pixel
     NNostart ++;
     std::cout << "Pixel "<<col<<" "<< addr << " has no start point. Assigning start point to the middle."<< std::endl;
     Start = (chargeMax+chargeMin)*ELECTRONS_PER_DAC/2.;
     *Ascurve = true;
   }
   else {*Ascurve = false;}

   fitfcn->SetParameters(Start,10);
   fitfcn->SetParName(0, "Threshold");
   fitfcn->SetParName(1, "Noise");
   g->Fit("fitfcn","Q");

   //Obteniendo los valores
   *ANoise     = fitfcn->GetParameter(1);
   *AThreshold = fitfcn->GetParameter(0);
   *AChisq     = fitfcn->GetChisquare()/fitfcn->GetNDF();
   g->Delete();
   fitfcn->Delete();
   if (*AThreshold>(ELECTRONS_PER_DAC*chargeMax) || *AThreshold<0){*Ascurve = true;cout<<"holi"<<endl;}
   return true;
}

Int_t ProcessPixel (int col, int addr, const char *fName) {
  double Threshold = 0;
  double Noise = 0;
  double Chisq = 0;
  bool scurve = false;
  if ( ((col%10) == 0)&&((addr%1000 == 0)) ) {cout <<"We are in col: " <<col << " of 512" <<endl;}
  if (!GetThreshold (&Threshold, &Noise, &Chisq, &scurve ,col, addr))
  {
          cout<<"Couldn't get Threshold of Pixel: "<<col<<" "<<addr<<endl;
  }
  cout<<col<<" "<<addr<<" "<<Threshold<<" "<<Noise<<" "<<Chisq<<endl;
  if (scurve) //Se ha mandado a dibujar el scurve de este pixel.
  {
	     cout<<"Drawing S-Curve: "<<col<<"-"<< addr<<" with Threshold: "<<Threshold<<endl;
	     TString colName;
	     colName.Form("%d",col);
	     TString addrName;
	     addrName.Form("%d",addr);
	     TString thresholdName;
	     thresholdName.Form("%.2f",Threshold);
	     //Cuando vuelva a pasar por aqui, se vuelve a hacer el canvas, no hay problema.
	     TCanvas *wrongPixel = new TCanvas();
         wrongPixel->cd();
	     TLine *thresholdLine;
	     TGraph* curve = new TGraph (NPoints, x, data);   
	     curve->SetMarkerStyle(21);
	     curve->GetXaxis()->SetTitle("Charge [e]");
	     curve->GetYaxis()->SetTitle("Nhits");
	     curve->SetMaximum(50);
         curve->SetMinimum(0);
	     TString Title="DoubleColumn: ";
	     Title = Title + colName+ " Address: "+addrName+" Threshold: "+ thresholdName;
	     curve->SetTitle(Title);
         //Dibujar la funcion de ajuste
   	     TF1    *fitfcn = new TF1("fitfcn", erf,0,500,2);
   	     fitfcn->SetParameter(0, Threshold);
   	     fitfcn->SetParameter(1,Noise);
	     curve->Fit("fitfcn","Q");
	     curve-> Draw("AP");
   	     thresholdLine = new TLine(Threshold,0,Threshold,50);
         thresholdLine->SetLineColor(kBlack);
         thresholdLine->SetLineStyle(2);
	     thresholdLine->SetLineWidth(3);
	     thresholdLine->Draw();

         //Grabando la imagen. Asegurese de que existe la carpeta wrongThreshold.
	     TString suffix = "./wrongThreshold/";
	     TString type=".png";
	     suffix = suffix +fName+separacion+ colName+separacion+addrName+separacion+type;
	     wrongPixel->SaveAs(suffix);
	     delete wrongPixel;
  }
  return 1;
}

Int_t Threshold(TString fName) {
  //Abriendo el archivo.
  FILE *fp = fopen (fName, "r");
  if (!fp) {
    std::cout << "Unable to open file " << fName <<std::endl;
    return -1;
  }

  Int_t col, address, ampl, hits;
  Int_t lastcol = -1, lastaddress = -1;
  NPoints  = 0;
  NNostart = 0;
  NChisq   = 0;

  //Leyendo el archivo.
  while((fscanf(fp,"%d %d %d %d",&col,&address,&ampl,&hits)==4)){ 
	    if (((lastcol!=col)||(address!=lastaddress))&&(NPoints!=0)) 
	    {
	      ProcessPixel(lastcol, lastaddress,fName);
	      NPixels ++;
	      ResetData();   
          NPoints  = 0;
        }
        lastcol = col;
	    lastaddress = address;
	    data [NPoints] = hits;
	    x    [NPoints] = ampl * ELECTRONS_PER_DAC;
	    NPoints ++;
  }
  fclose(fp);
  return 1;
}
