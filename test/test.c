#include <stdio.h>

#include "Cbellhop.h"

int main() {
    struct CBellhopConfigPara* cBellhopConfigParaPtr =
        cBellhopConfigParaCreate();
    cBellhopConfigParaPtr->setTitle(cBellhopConfigParaPtr, "title");
    cBellhopConfigParaPtr->setFreq(cBellhopConfigParaPtr, 100);
    cBellhopConfigParaPtr->setNMedia(cBellhopConfigParaPtr, 1);
    cBellhopConfigParaPtr->setTopopt(cBellhopConfigParaPtr, "CVW  ");
    cBellhopConfigParaPtr->setNPts(cBellhopConfigParaPtr, 0);
    cBellhopConfigParaPtr->setTSigma(cBellhopConfigParaPtr, 0);
    cBellhopConfigParaPtr->setDepthB(cBellhopConfigParaPtr, 318.127);
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
    cBellhopConfigParaPtr->setzSSPV(cBellhopConfigParaPtr, zSSPV, 29);
    cBellhopConfigParaPtr->setcSSPV(cBellhopConfigParaPtr, cSSPV, 29);
    cBellhopConfigParaPtr->setBotopt(cBellhopConfigParaPtr, "A* ");
    cBellhopConfigParaPtr->setBSigma(cBellhopConfigParaPtr, 0);
    cBellhopConfigParaPtr->setalphaR(cBellhopConfigParaPtr, 2000);
    cBellhopConfigParaPtr->setbetaR(cBellhopConfigParaPtr, 450);
    cBellhopConfigParaPtr->setrhoR(cBellhopConfigParaPtr, 1.9);
    cBellhopConfigParaPtr->setalphaI(cBellhopConfigParaPtr, 0.4);
    cBellhopConfigParaPtr->setbetaI(cBellhopConfigParaPtr, 0.225);
    float SD[1] = {1};
    cBellhopConfigParaPtr->setSD(cBellhopConfigParaPtr, SD, 1);
    cBellhopConfigParaPtr->setNSD(cBellhopConfigParaPtr, 1);
    float RD[2] = {0, 100};
    cBellhopConfigParaPtr->setRD(cBellhopConfigParaPtr, RD, 2);
    cBellhopConfigParaPtr->setNRD(cBellhopConfigParaPtr, 20);
    float R[2] = {0, 150};
    cBellhopConfigParaPtr->setR(cBellhopConfigParaPtr, R, 2);
    cBellhopConfigParaPtr->setNR(cBellhopConfigParaPtr, 10);
    cBellhopConfigParaPtr->setRuntype(cBellhopConfigParaPtr, "CG R");
    cBellhopConfigParaPtr->setNBEAMS(cBellhopConfigParaPtr, 10);
    float angle[2] = {-5, 1};
    cBellhopConfigParaPtr->setangle(cBellhopConfigParaPtr, angle, 2);
    cBellhopConfigParaPtr->setdeltas(cBellhopConfigParaPtr, 0);
    cBellhopConfigParaPtr->setzBox(cBellhopConfigParaPtr, 368.127);
    cBellhopConfigParaPtr->setrBox(cBellhopConfigParaPtr, 101);
    cBellhopConfigParaPtr->setBeamtype(cBellhopConfigParaPtr, "MS ");
    cBellhopConfigParaPtr->setNbtyPts(cBellhopConfigParaPtr, 3);
    double btyPts[6] = {0, 3000, 5, 3000, 10, 3000};
    cBellhopConfigParaPtr->setbtyPts(cBellhopConfigParaPtr, btyPts, 6);
    struct CBellResult* cBellResultPtr = cBellhopRun(cBellhopConfigParaPtr);
    int curveNum = cBellResultPtr->curveResultPtr->getCurveNum(
        cBellResultPtr->curveResultPtr);
    printf("The curveResult has total curve num : %d\n", curveNum);
    for (int j = 0; j < curveNum; ++j) {
        struct Curve* curveObjPtr =
            curveCreate(cBellResultPtr->curveResultPtr, j);
        int pointNum = curveObjPtr->size(curveObjPtr);
        printf("The poinNum of this curve: %d\n", pointNum);
        for (int i = 0; i < pointNum; ++i) {
            struct Point point = curveObjPtr->index(curveObjPtr, i);
            printf("%lf %lf\n", point.x, point.y);
        }
        curveObjPtr->destory(curveObjPtr);
    }
    int row = cBellResultPtr->energyResultPtr->size(
        cBellResultPtr->energyResultPtr, 0);
    int col = cBellResultPtr->energyResultPtr->size(
        cBellResultPtr->energyResultPtr, 1);
    for (int i = 0; i < row; ++i) {
        for (int j = 0; j < col; ++j) {
            struct ComplexFloat ele = cBellResultPtr->energyResultPtr->index(
                cBellResultPtr->energyResultPtr, i, j);
            printf("(%f, %f) ", ele.real, ele.imag);
        }
        printf("\n");
    }
    int arrRow =
        cBellResultPtr->arrResultPtr->size(cBellResultPtr->arrResultPtr, 0);
    int arrCol =
        cBellResultPtr->arrResultPtr->size(cBellResultPtr->arrResultPtr, 1);
    for (int i = 0; i < arrRow; ++i) {
        for (int j = 0; j < arrCol; ++j) {
            struct ArrResultReceiver* receverPtr =
                cBellResultPtr->arrResultPtr->receiverCreate(
                    cBellResultPtr->arrResultPtr, i, j);
            int curveNum = receverPtr->getCurveNum(receverPtr);
            printf("%d\n", curveNum);
            for (int k = 0; k < curveNum; ++k) {
                float amp = receverPtr->ampIndex(receverPtr, k);
                float phase = receverPtr->phaseIndex(receverPtr, k);
                float delay = receverPtr->delayIndex(receverPtr, k);
                float srcAngle = receverPtr->srcAngleIndex(receverPtr, k);
                float recvAngle = receverPtr->recvAngleIndex(receverPtr, k);
                int nTopBnc = receverPtr->nTopBncIndex(receverPtr, k);
                int nBotBnc = receverPtr->nBotBncIndex(receverPtr, k);
                printf("%f %f %f %f %f %d %d\n", amp, phase, delay, srcAngle,
                       recvAngle, nTopBnc, nBotBnc);
            }
            receverPtr->destory(receverPtr);
        }
        printf("\n");
    }
    cBellhopConfigParaPtr->destory(cBellhopConfigParaPtr);
    cBellResultPtr->destory(cBellResultPtr);
    return 0;
}