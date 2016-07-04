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

Int_t BDT() {

TMVA::Tools::Instance();
TFile* outputFile = TFile::Open("TMVAout.root","RECREATE");
TMVA::Factory *factory = new TMVA::Factory("TMVAClassification",outputFile,"V:!Silent:Color:Transformations=I:DrawProgressBar:AnalysisType=Classification");

Double_t weight1=1.0;
Double_t weight2=2.0;

TFile* toy1 = new TFile("out1.root");
TFile* toy2 = new TFile("out2.root");

TTree* sec1Tree = (TTree*)(toy1->Get("tree"));
TTree* sec2Tree = (TTree*)(toy2->Get("tree"));

factory->AddSignalTree(sec1Tree,weight1);
factory->AddBackgroundTree(sec2Tree,weight2);

factory->AddVariable("Threshold","Threshold","Charge",'F');
factory->AddVariable("Noise","Noise","Charge",'F');
factory->AddVariable("Chisq","Chisq","Unit",'F');
factory->AddVariable("VCASN","VCASN","DACS",'I');
factory->AddVariable("Temp","Temp","C",'I');
factory->AddVariable("VBB","VBB","DACS",'I');

//factory->PrepareTrainingAndTestTree("random");
factory->BookMethod(TMVA::Types::kBDT,"BDT","NTrees=400:MaxDepth=2");
factory->BookMethod(TMVA::Types::kMLP,"MLPBNN","H:V:NeuronType=tanh:VarTransform=N:NCycles=600:HiddenLayers=N+5:TestRate=5:TrainingMethod=BFGS:UseRegulator");	

factory->TrainAllMethods();
factory->TestAllMethods();

factory->EvaluateAllMethods();
outputFile->Close();
delete factory;
TMVA::TMVAGui("TMVAout.root");

return 1;

}
