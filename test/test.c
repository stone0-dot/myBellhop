#include <stdio.h>

#include "Cbellhop.h"

int main() {
    struct CBellhopConfigPara* cBellhopConfigParaPtr =
        cBellhopConfigParaCreate();
    cBellhopConfigParaSetTitle(cBellhopConfigParaPtr, "title");
    cBellhopConfigParaSetFreq(cBellhopConfigParaPtr, 100);
    cBellhopConfigParaSetNMedia(cBellhopConfigParaPtr, 1);
    cBellhopConfigParaSetTopopt(cBellhopConfigParaPtr, "CVW  ");
    cBellhopConfigParaSetNPts(cBellhopConfigParaPtr, 0);
    cBellhopConfigParaSetTSigma(cBellhopConfigParaPtr, 0);
    cBellhopConfigParaSetDepthB(cBellhopConfigParaPtr, 318.127);
    float zSSPV[29] = {0.494,   1.541,   2.646,   3.819,   5.078,   6.441,
                       7.930,   9.573,   11.405,  13.467,  15.810,  18.496,
                       21.599,  25.211,  29.445,  34.434,  40.344,  47.374,
                       55.764,  65.807,  77.854,  92.326,  109.729, 130.666,
                       155.851, 186.126, 222.475, 266.040, 318.127};
    float cSSPV[29] = {1533.7650, 1533.7817, 1533.7977, 1533.8129, 1533.8313,
                       1533.8496, 1533.8700, 1533.8945, 1533.9203, 1533.9516,
                       1533.9855, 1534.0268, 1534.0747, 1534.1290, 1534.1949,
                       1534.2730, 1534.3675, 1534.4781, 1534.6104, 1534.8015,
                       1535.0049, 1535.1237, 1534.1685, 1528.7311, 1521.4723,
                       1513.7857, 1509.2453, 1504.9981, 1500.6822};
    cBellhopConfigParaSetzSSPV(cBellhopConfigParaPtr, zSSPV, 29);
    cBellhopConfigParaSetcSSPV(cBellhopConfigParaPtr, cSSPV, 29);
    cBellhopConfigParaSetBotopt(cBellhopConfigParaPtr, "A  ");
    cBellhopConfigParaSetBSigma(cBellhopConfigParaPtr, 0);
    cBellhopConfigParaSetalphaR(cBellhopConfigParaPtr, 2000);
    cBellhopConfigParaSetbetaR(cBellhopConfigParaPtr, 450);
    cBellhopConfigParaSetrhoR(cBellhopConfigParaPtr, 1.9);
    cBellhopConfigParaSetalphaI(cBellhopConfigParaPtr, 0.4);
    cBellhopConfigParaSetbetaI(cBellhopConfigParaPtr, 0.225);
    float SD[1] = {1};
    cBellhopConfigParaSetSD(cBellhopConfigParaPtr, SD, 1);
    cBellhopConfigParaSetNSD(cBellhopConfigParaPtr, 1);
    float RD[2] = {0, 100};
    cBellhopConfigParaSetRD(cBellhopConfigParaPtr, RD, 2);
    cBellhopConfigParaSetNRD(cBellhopConfigParaPtr, 20);
    float R[2] = {0, 150};
    cBellhopConfigParaSetR(cBellhopConfigParaPtr, R, 2);
    cBellhopConfigParaSetNR(cBellhopConfigParaPtr, 10);
    cBellhopConfigParaSetRuntype(cBellhopConfigParaPtr, "RG R");
    cBellhopConfigParaSetNBEAMS(cBellhopConfigParaPtr, 10);
    float angle[2] = {-5, 1};
    cBellhopConfigParaSetangle(cBellhopConfigParaPtr, angle, 2);
    cBellhopConfigParaSetdeltas(cBellhopConfigParaPtr, 0);
    cBellhopConfigParaSetzBox(cBellhopConfigParaPtr, 318.127);
    cBellhopConfigParaSetrBox(cBellhopConfigParaPtr, 101);
    cBellhopConfigParaSetBeamtype(cBellhopConfigParaPtr, "MS ");
    struct CurveResult* curveResult = cBellhopRun(cBellhopConfigParaPtr);
    return 0;
}