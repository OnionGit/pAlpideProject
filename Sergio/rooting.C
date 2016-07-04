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

Float_t Threshold, Noise,Chisq;
Int_t VCASN,Temp,VBB;

Int_t rooting(TString fName) {


  TFile f("out.root","RECREATE");
  TTree *tree = new TTree("tree","Example of Tree");
  tree->Branch("Threshold",&Threshold,"Threshold/F");
  tree->Branch("Noise",&Noise,"Noise/F");
  tree->Branch("Chisq",&Chisq,"Chisq/F");

  tree->Branch("VCASN",&VCASN,"VCASN/I");
  tree->Branch("Temp",&Temp,"Temp/I");
  tree->Branch("VBB",&VBB,"VBB/I");
  


  //Abriendo el archivo.
  FILE *fp = fopen (fName, "r");
  if (!fp) {
    std::cout << "Unable to open file " << fName <<std::endl;
    return -1;
  }

  while((fscanf(fp,"%f %f %f %d %d %d\n", &Threshold,&Noise,&Chisq,&VCASN,&Temp,&VBB)==6)){ 

  tree->Fill();

  }

tree->Write();
tree->Print();
delete f;

return 1;
}
