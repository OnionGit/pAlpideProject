#include "TF1.h"
#include "TH2F.h"
#include "TH1F.h"
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

Int_t dcol,addr;
Float_t Threshold, Noise,Chisq;
Int_t Sector;

  char vcasnTitle[15];
  int vcasnRead;
  char ithrTitle[15];
  int ithrRead;
  Int_t VCASN,ITHR;

Int_t comp(TString fName,Int_t Temp,Int_t VBB) {

  //Abriendo el archivo.
  FILE *fp = fopen (fName, "r");
  if (!fp) {
    std::cout << "Unable to open file " << fName <<std::endl;
    return -1;
  }

 FILE *fpoutResults = fopen("./Quorum.dat", "a");
 FILE *fpoutResults2 = fopen("./QuorumSector2.dat", "a");
 FILE *fpoutResults1 = fopen("./QuorumSector1.dat", "a");

  if (!fpoutResults) {
    std::cout << "Unable to open Quorum " <<std::endl;
    return -1;
  }

    fscanf (fp, "%s %d %s %d", vcasnTitle, &vcasnRead, ithrTitle, &ithrRead);
    if(strcmp(vcasnTitle, "VCASN:")==0){VCASN=vcasnRead;}
    if(strcmp(ithrTitle, "ITHR:")==0){ITHR=ithrRead;}
    cout<<"VCASN: "<<VCASN<<" ITHR: "<<ITHR<<" TEMP: "<<Temp<<" VBB: "<<VBB<<endl;


  while((fscanf(fp,"%d %d %f %f %f",&dcol,&addr,&Threshold,&Noise,&Chisq)==5)){ 

  Sector = dcol/128;
  if (Sector==1||Sector==2){fprintf(fpoutResults, "%f %f %f %d %d %d %d\n", Threshold,Noise,Chisq,VCASN,Temp,VBB,Sector);}
  if (Sector==1){fprintf(fpoutResults1, "%f %f %f %d %d %d\n", Threshold,Noise,Chisq,VCASN,Temp,VBB);}
  if (Sector==2){fprintf(fpoutResults2, "%f %f %f %d %d %d\n", Threshold,Noise,Chisq,VCASN,Temp,VBB);}


}

  fclose(fp);
  fclose(fpoutResults);
  fclose(fpoutResults1);
  fclose(fpoutResults2);


return 1;
}
