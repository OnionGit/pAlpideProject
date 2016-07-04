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

Int_t dcol,addr;
Float_t Threshold, Noise,Chisq;

Int_t EvenOdd(TString fName) {


  //Abriendo el archivo.
  FILE *fp = fopen (fName, "r");
  if (!fp) {
    std::cout << "Unable to open file " << fName <<std::endl;
    return -1;
  }

    int col    = dcol * 2;    // In this case, ARegion is useless.
    if ((addr % 4) < 2) {col ++;}       // Left or right column within the double column
    int row = addr / 2;                // This is OK for the top-right and the bottom-left pixel within a group of 4
    if ((addr % 4) == 3) row -= 1;      // adjust the top-left pixel
    if ((addr % 4) == 0) row += 1;      // adjust the bottom-right pixel

}

return 1;
}
