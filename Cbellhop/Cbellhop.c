#include "Cbellhop.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "interface.h"

//------------------------------------------------------------------------------------------
// FortranConfigPara类的成员函数
//------------------------------------------------------------------------------------------
struct FortranConfigPara* fortranConfigParaCreate() {
    struct FortranConfigPara* fortranConfigParaPtr =
        (struct FortranConfigPara*)malloc(sizeof(struct FortranConfigPara));
    memset(&fortranConfigParaPtr->stringPara, 0, sizeof(struct StringPara));
    memset(&fortranConfigParaPtr->digitalPara, 0, sizeof(struct DigitalPara));
    return fortranConfigParaPtr;
}
void fortranConfigParaDestory(struct FortranConfigPara* fortranConfigParaPtr) {
    delete_c_chars();
    free(fortranConfigParaPtr);
}
void fortranConfigParaReadConfigIn(
    struct FortranConfigPara* fortranConfigParaPtr) {
    readconfig(&fortranConfigParaPtr->stringPara.title,
               &fortranConfigParaPtr->digitalPara.freq,
               &fortranConfigParaPtr->digitalPara.isingl,
               &fortranConfigParaPtr->digitalPara.nimage,
               &fortranConfigParaPtr->digitalPara.ibwin,
               &fortranConfigParaPtr->digitalPara.deltas,
               &fortranConfigParaPtr->digitalPara.maxn,
               &fortranConfigParaPtr->digitalPara.zbox,
               &fortranConfigParaPtr->digitalPara.rbox,
               &fortranConfigParaPtr->digitalPara.epmult,
               &fortranConfigParaPtr->digitalPara.rloop,
               &fortranConfigParaPtr->stringPara.topopt,
               &fortranConfigParaPtr->digitalPara.deptht,
               &fortranConfigParaPtr->digitalPara.cpt.real,
               &fortranConfigParaPtr->digitalPara.cpt.imag,
               &fortranConfigParaPtr->digitalPara.rhot,
               &fortranConfigParaPtr->stringPara.botopt,
               &fortranConfigParaPtr->digitalPara.depthb,
               &fortranConfigParaPtr->digitalPara.cpb.real,
               &fortranConfigParaPtr->digitalPara.cpb.imag,
               &fortranConfigParaPtr->digitalPara.rhob,
               &fortranConfigParaPtr->stringPara.runtype,
               &fortranConfigParaPtr->stringPara.beamtype);
}
//------------------------------------------------------------------------------------------
// CBellhopConfigPara类的成员函数
//------------------------------------------------------------------------------------------
struct CBellhopConfigPara* cBellhopConfigParaCreate() {
    struct CBellhopConfigPara* cBellhopConfigParaPtr =
        (struct CBellhopConfigPara*)malloc(sizeof(struct CBellhopConfigPara));
    memset(cBellhopConfigParaPtr, 0, sizeof(struct CBellhopConfigPara));
    cBellhopConfigParaPtr->destory = &cBellhopConfigParaDestory;
    cBellhopConfigParaPtr->setTitle = &cBellhopConfigParaSetTitle;
    cBellhopConfigParaPtr->setTopopt = &cBellhopConfigParaSetTopopt;
    cBellhopConfigParaPtr->setBotopt = &cBellhopConfigParaSetBotopt;
    cBellhopConfigParaPtr->setRuntype = &cBellhopConfigParaSetRuntype;
    cBellhopConfigParaPtr->setBeamtype = &cBellhopConfigParaSetBeamtype;
    cBellhopConfigParaPtr->setFreq = &cBellhopConfigParaSetFreq;
    cBellhopConfigParaPtr->setNMedia = &cBellhopConfigParaSetNMedia;
    cBellhopConfigParaPtr->setNPts = &cBellhopConfigParaSetNPts;
    cBellhopConfigParaPtr->setTSigma = &cBellhopConfigParaSetTSigma;
    cBellhopConfigParaPtr->setBSigma = &cBellhopConfigParaSetBSigma;
    cBellhopConfigParaPtr->setDepthB = &cBellhopConfigParaSetDepthB;
    cBellhopConfigParaPtr->setzSSPV = &cBellhopConfigParaSetzSSPV;
    cBellhopConfigParaPtr->setcSSPV = &cBellhopConfigParaSetcSSPV;
    cBellhopConfigParaPtr->setalphaR = &cBellhopConfigParaSetalphaR;
    cBellhopConfigParaPtr->setbetaR = &cBellhopConfigParaSetbetaR;
    cBellhopConfigParaPtr->setrhoR = &cBellhopConfigParaSetrhoR;
    cBellhopConfigParaPtr->setalphaI = &cBellhopConfigParaSetalphaI;
    cBellhopConfigParaPtr->setbetaI = &cBellhopConfigParaSetbetaI;
    cBellhopConfigParaPtr->setNSD = &cBellhopConfigParaSetNSD;
    cBellhopConfigParaPtr->setSD = &cBellhopConfigParaSetSD;
    cBellhopConfigParaPtr->setNRD = &cBellhopConfigParaSetNRD;
    cBellhopConfigParaPtr->setRD = &cBellhopConfigParaSetRD;
    cBellhopConfigParaPtr->setNR = &cBellhopConfigParaSetNR;
    cBellhopConfigParaPtr->setR = &cBellhopConfigParaSetR;
    cBellhopConfigParaPtr->setNBEAMS = &cBellhopConfigParaSetNBEAMS;
    cBellhopConfigParaPtr->setangle = &cBellhopConfigParaSetangle;
    cBellhopConfigParaPtr->setdeltas = &cBellhopConfigParaSetdeltas;
    cBellhopConfigParaPtr->setzBox = &cBellhopConfigParaSetzBox;
    cBellhopConfigParaPtr->setrBox = &cBellhopConfigParaSetrBox;
    cBellhopConfigParaPtr->setepmult = &cBellhopConfigParaSetepmult;
    cBellhopConfigParaPtr->setrloop = &cBellhopConfigParaSetrloop;
    cBellhopConfigParaPtr->setNimage = &cBellhopConfigParaSetNimage;
    cBellhopConfigParaPtr->setIbwin = &cBellhopConfigParaSetIbwin;
    cBellhopConfigParaPtr->setISIGNAL = &cBellhopConfigParaSetISIGNAL;
    cBellhopConfigParaPtr->setNbtyPts = &cBellhopConfigParaSetNbtyPts;
    cBellhopConfigParaPtr->setbtyPts = &cBellhopConfigParaSetbtyPts;
    return cBellhopConfigParaPtr;
}
void cBellhopConfigParaDestory(struct CBellhopConfigPara* configParaPtr) {
    vectorcharDestory(&configParaPtr->stringPara.title);
    vectorcharDestory(&configParaPtr->stringPara.beamtype);
    vectorcharDestory(&configParaPtr->stringPara.botopt);
    vectorcharDestory(&configParaPtr->stringPara.topopt);
    vectorcharDestory(&configParaPtr->stringPara.runtype);

    vectorfloatDestory(&configParaPtr->digitalPara.zSSPV);
    vectorfloatDestory(&configParaPtr->digitalPara.cSSPV);
    vectorfloatDestory(&configParaPtr->digitalPara.SD);
    vectorfloatDestory(&configParaPtr->digitalPara.RD);
    vectorfloatDestory(&configParaPtr->digitalPara.R);
    vectorfloatDestory(&configParaPtr->digitalPara.angle);

    vectordoubleDestory(&configParaPtr->digitalPara.btyPts);
    free(configParaPtr);
}
void cBellhopConfigParaSetTitle(struct CBellhopConfigPara* configParaPtr,
                                const char* title) {
    if (configParaPtr->stringPara.title.capacity >= strlen(title)) {
        configParaPtr->stringPara.title.size = strlen(title);
        memcpy(configParaPtr->stringPara.title.arr, title,
               sizeof(char) * strlen(title));
    } else {
        if (configParaPtr->stringPara.title.capacity != 0) {
            vectorcharDestory(&configParaPtr->stringPara.title);
        }
        configParaPtr->stringPara.title =
            vectorcharCreatebyPtr(title, strlen(title));
    }
}
void cBellhopConfigParaSetTopopt(struct CBellhopConfigPara* configParaPtr,
                                 const char* topopt) {
    if (configParaPtr->stringPara.topopt.capacity >= strlen(topopt)) {
        configParaPtr->stringPara.topopt.size = strlen(topopt);
        memcpy(configParaPtr->stringPara.topopt.arr, topopt,
               sizeof(char) * strlen(topopt));
    } else {
        if (configParaPtr->stringPara.topopt.capacity != 0) {
            vectorcharDestory(&configParaPtr->stringPara.topopt);
        }
        configParaPtr->stringPara.topopt =
            vectorcharCreatebyPtr(topopt, strlen(topopt));
    }
}
void cBellhopConfigParaSetBotopt(struct CBellhopConfigPara* configParaPtr,
                                 const char* botopt) {
    if (configParaPtr->stringPara.botopt.capacity >= strlen(botopt)) {
        configParaPtr->stringPara.botopt.size = strlen(botopt);
        memcpy(configParaPtr->stringPara.botopt.arr, botopt,
               sizeof(char) * strlen(botopt));
    } else {
        if (configParaPtr->stringPara.botopt.capacity != 0) {
            vectorcharDestory(&configParaPtr->stringPara.botopt);
        }
        configParaPtr->stringPara.botopt =
            vectorcharCreatebyPtr(botopt, strlen(botopt));
    }
}
void cBellhopConfigParaSetRuntype(struct CBellhopConfigPara* configParaPtr,
                                  const char* runtype) {
    if (configParaPtr->stringPara.runtype.capacity >= strlen(runtype)) {
        configParaPtr->stringPara.runtype.size = strlen(runtype);
        memcpy(configParaPtr->stringPara.runtype.arr, runtype,
               sizeof(char) * strlen(runtype));
    } else {
        if (configParaPtr->stringPara.runtype.capacity != 0) {
            vectorcharDestory(&configParaPtr->stringPara.runtype);
        }
        configParaPtr->stringPara.runtype =
            vectorcharCreatebyPtr(runtype, strlen(runtype));
    }
}
void cBellhopConfigParaSetBeamtype(struct CBellhopConfigPara* configParaPtr,
                                   const char* beamtype) {
    if (configParaPtr->stringPara.beamtype.capacity >= strlen(beamtype)) {
        configParaPtr->stringPara.beamtype.size = strlen(beamtype);
        memcpy(configParaPtr->stringPara.beamtype.arr, beamtype,
               sizeof(char) * strlen(beamtype));
    } else {
        if (configParaPtr->stringPara.beamtype.capacity != 0) {
            vectorcharDestory(&configParaPtr->stringPara.beamtype);
        }
        configParaPtr->stringPara.beamtype =
            vectorcharCreatebyPtr(beamtype, strlen(beamtype));
    }
}
void cBellhopConfigParaSetFreq(struct CBellhopConfigPara* configParaPtr,
                               const float freq) {
    configParaPtr->digitalPara.freq = freq;
}
void cBellhopConfigParaSetNMedia(struct CBellhopConfigPara* configParaPtr,
                                 const int NMedia) {
    configParaPtr->digitalPara.NMedia = NMedia;
}
void cBellhopConfigParaSetNPts(struct CBellhopConfigPara* configParaPtr,
                               const int NPts) {
    configParaPtr->digitalPara.NPts = NPts;
}
void cBellhopConfigParaSetTSigma(struct CBellhopConfigPara* configParaPtr,
                                 const float TSigma) {
    configParaPtr->digitalPara.TSigma = TSigma;
}
void cBellhopConfigParaSetBSigma(struct CBellhopConfigPara* configParaPtr,
                                 const float BSigma) {
    configParaPtr->digitalPara.BSigma = BSigma;
}
void cBellhopConfigParaSetDepthB(struct CBellhopConfigPara* configParaPtr,
                                 const float DepthB) {
    configParaPtr->digitalPara.DepthB = DepthB;
}
void cBellhopConfigParaSetzSSPV(struct CBellhopConfigPara* configParaPtr,
                                const float* zSSPV, const unsigned int len) {
    if (configParaPtr->digitalPara.zSSPV.capacity >= len) {
        configParaPtr->digitalPara.zSSPV.size = len;
        memcpy(configParaPtr->digitalPara.zSSPV.arr, zSSPV,
               sizeof(float) * len);
    } else {
        if (configParaPtr->digitalPara.zSSPV.capacity != 0) {
            vectorfloatDestory(&configParaPtr->digitalPara.zSSPV);
        }
        configParaPtr->digitalPara.zSSPV = vectorfloatCreatebyPtr(zSSPV, len);
    }
}
void cBellhopConfigParaSetcSSPV(struct CBellhopConfigPara* configParaPtr,
                                const float* cSSPV, const unsigned int len) {
    if (configParaPtr->digitalPara.cSSPV.capacity >= len) {
        configParaPtr->digitalPara.cSSPV.size = len;
        memcpy(configParaPtr->digitalPara.cSSPV.arr, cSSPV,
               sizeof(float) * len);
    } else {
        if (configParaPtr->digitalPara.cSSPV.capacity != 0) {
            vectorfloatDestory(&configParaPtr->digitalPara.cSSPV);
        }
        configParaPtr->digitalPara.cSSPV = vectorfloatCreatebyPtr(cSSPV, len);
    }
}
void cBellhopConfigParaSetalphaR(struct CBellhopConfigPara* configParaPtr,
                                 const double alphaR) {
    configParaPtr->digitalPara.alphaR = alphaR;
}
void cBellhopConfigParaSetbetaR(struct CBellhopConfigPara* configParaPtr,
                                const double betaR) {
    configParaPtr->digitalPara.betaR = betaR;
}
void cBellhopConfigParaSetrhoR(struct CBellhopConfigPara* configParaPtr,
                               const double rhoR) {
    configParaPtr->digitalPara.rhoR = rhoR;
}
void cBellhopConfigParaSetalphaI(struct CBellhopConfigPara* configParaPtr,
                                 const double alphaI) {
    configParaPtr->digitalPara.alphaI = alphaI;
}
void cBellhopConfigParaSetbetaI(struct CBellhopConfigPara* configParaPtr,
                                const double betaI) {
    configParaPtr->digitalPara.betaI = betaI;
}
void cBellhopConfigParaSetNSD(struct CBellhopConfigPara* configParaPtr,
                              const unsigned int NSD) {
    configParaPtr->digitalPara.NSD = NSD;
}
void cBellhopConfigParaSetSD(struct CBellhopConfigPara* configParaPtr,
                             const float* SD, const unsigned int len) {
    if (configParaPtr->digitalPara.SD.capacity >= len) {
        configParaPtr->digitalPara.SD.size = len;
        memcpy(configParaPtr->digitalPara.SD.arr, SD, sizeof(float) * len);
    } else {
        if (configParaPtr->digitalPara.SD.capacity != 0) {
            vectorfloatDestory(&configParaPtr->digitalPara.SD);
        }
        configParaPtr->digitalPara.SD = vectorfloatCreatebyPtr(SD, len);
    }
}
void cBellhopConfigParaSetNRD(struct CBellhopConfigPara* configParaPtr,
                              const unsigned int NRD) {
    configParaPtr->digitalPara.NRD = NRD;
}
void cBellhopConfigParaSetRD(struct CBellhopConfigPara* configParaPtr,
                             const float* RD, const unsigned int len) {
    if (configParaPtr->digitalPara.RD.capacity >= len) {
        configParaPtr->digitalPara.RD.size = len;
        memcpy(configParaPtr->digitalPara.RD.arr, RD, sizeof(float) * len);
    } else {
        if (configParaPtr->digitalPara.RD.capacity != 0) {
            vectorfloatDestory(&configParaPtr->digitalPara.RD);
        }
        configParaPtr->digitalPara.RD = vectorfloatCreatebyPtr(RD, len);
    }
}
void cBellhopConfigParaSetNR(struct CBellhopConfigPara* configParaPtr,
                             const unsigned int NR) {
    configParaPtr->digitalPara.NR = NR;
}
void cBellhopConfigParaSetR(struct CBellhopConfigPara* configParaPtr,
                            const float* R, const unsigned int len) {
    if (configParaPtr->digitalPara.R.capacity >= len) {
        configParaPtr->digitalPara.R.size = len;
        memcpy(configParaPtr->digitalPara.R.arr, R, sizeof(float) * len);
    } else {
        if (configParaPtr->digitalPara.R.capacity != 0) {
            vectorfloatDestory(&configParaPtr->digitalPara.R);
        }
        configParaPtr->digitalPara.R = vectorfloatCreatebyPtr(R, len);
    }
}
void cBellhopConfigParaSetNBEAMS(struct CBellhopConfigPara* configParaPtr,
                                 const unsigned int NBEAMS) {
    configParaPtr->digitalPara.NBEAMS = NBEAMS;
}
void cBellhopConfigParaSetangle(struct CBellhopConfigPara* configParaPtr,
                                const float* angle, const unsigned int len) {
    if (configParaPtr->digitalPara.angle.capacity >= len) {
        configParaPtr->digitalPara.angle.size = len;
        memcpy(configParaPtr->digitalPara.angle.arr, angle,
               sizeof(float) * len);
    } else {
        if (configParaPtr->digitalPara.angle.capacity != 0) {
            vectorfloatDestory(&configParaPtr->digitalPara.angle);
        }
        configParaPtr->digitalPara.angle = vectorfloatCreatebyPtr(angle, len);
    }
}
void cBellhopConfigParaSetdeltas(struct CBellhopConfigPara* configParaPtr,
                                 const float deltas) {
    configParaPtr->digitalPara.deltas = deltas;
}
void cBellhopConfigParaSetzBox(struct CBellhopConfigPara* configParaPtr,
                               const float zBox) {
    configParaPtr->digitalPara.zBox = zBox;
}
void cBellhopConfigParaSetrBox(struct CBellhopConfigPara* configParaPtr,
                               const float rBox) {
    configParaPtr->digitalPara.rBox = rBox;
}
void cBellhopConfigParaSetepmult(struct CBellhopConfigPara* configParaPtr,
                                 const float epmult) {
    configParaPtr->digitalPara.epmult = epmult;
}
void cBellhopConfigParaSetrloop(struct CBellhopConfigPara* configParaPtr,
                                const float rloop) {
    configParaPtr->digitalPara.rloop = rloop;
}
void cBellhopConfigParaSetNimage(struct CBellhopConfigPara* configParaPtr,
                                 const unsigned int Nimage) {
    configParaPtr->digitalPara.Nimage = Nimage;
}
void cBellhopConfigParaSetIbwin(struct CBellhopConfigPara* configParaPtr,
                                const unsigned int Ibwin) {
    configParaPtr->digitalPara.Ibwin = Ibwin;
}
void cBellhopConfigParaSetISIGNAL(struct CBellhopConfigPara* configParaPtr,
                                  const unsigned int ISIGNAL) {
    configParaPtr->digitalPara.ISIGNAL = ISIGNAL;
}
void cBellhopConfigParaSetNbtyPts(struct CBellhopConfigPara* configParaPtr,
                                  const unsigned int NbtyPts) {
    configParaPtr->digitalPara.NbtyPts = NbtyPts;
}
void cBellhopConfigParaSetbtyPts(struct CBellhopConfigPara* configParaPtr,
                                 const double* btyPts, const unsigned int len) {
    if (configParaPtr->digitalPara.btyPts.capacity >= len) {
        configParaPtr->digitalPara.btyPts.size = len;
        memcpy(configParaPtr->digitalPara.btyPts.arr, btyPts,
               sizeof(double) * len);
    } else {
        if (configParaPtr->digitalPara.btyPts.capacity != 0) {
            vectordoubleDestory(&configParaPtr->digitalPara.btyPts);
        }
        configParaPtr->digitalPara.btyPts =
            vectordoubleCreatebyPtr(btyPts, len);
    }
}
//------------------------------------------------------------------------------------------
// CurveResult类的成员函数
//------------------------------------------------------------------------------------------
struct CurveResult* curveResultCreate() {
    struct CurveResult* curveResultPtr =
        (struct CurveResult*)malloc(sizeof(struct CurveResult));
    curveResultPtr->destory = &curveResultDestory;
    curveResultPtr->getCurveNum = &curveResultGetCurveNum;
    return curveResultPtr;
}
void curveResultDestory(struct CurveResult* curveResultPtr) {
    delete_line_length();
    delete_xv_result();
    free(curveResultPtr);
}
int curveResultGetCurveNum(const struct CurveResult* curveResultPtr) {
    return curveResultPtr->curveLengthArr[0];
}
//------------------------------------------------------------------------------------------
// 运行函数，调用fortran版本bellhop的计算api
//------------------------------------------------------------------------------------------
struct CurveResult* run(struct FortranConfigPara* fortranConfigParaPtr) {
    struct CurveResult* curveResultPtr = curveResultCreate();
    caculate(&fortranConfigParaPtr->digitalPara.freq,
             &fortranConfigParaPtr->digitalPara.isingl,
             &fortranConfigParaPtr->digitalPara.nimage,
             &fortranConfigParaPtr->digitalPara.ibwin,
             &fortranConfigParaPtr->digitalPara.deltas,
             &fortranConfigParaPtr->digitalPara.maxn,
             &fortranConfigParaPtr->digitalPara.zbox,
             &fortranConfigParaPtr->digitalPara.rbox,
             &fortranConfigParaPtr->digitalPara.epmult,
             &fortranConfigParaPtr->digitalPara.rloop,
             fortranConfigParaPtr->stringPara.topopt,
             &fortranConfigParaPtr->digitalPara.deptht,
             &fortranConfigParaPtr->digitalPara.cpt.real,
             &fortranConfigParaPtr->digitalPara.cpt.imag,
             &fortranConfigParaPtr->digitalPara.rhot,
             fortranConfigParaPtr->stringPara.botopt,
             &fortranConfigParaPtr->digitalPara.depthb,
             &fortranConfigParaPtr->digitalPara.cpb.real,
             &fortranConfigParaPtr->digitalPara.cpb.imag,
             &fortranConfigParaPtr->digitalPara.rhob,
             fortranConfigParaPtr->stringPara.runtype,
             fortranConfigParaPtr->stringPara.beamtype,
             &curveResultPtr->curveLengthArr, &curveResultPtr->curveContain);
    return curveResultPtr;
}

struct CBellResult* cBellhopRun(
    struct CBellhopConfigPara* cBellhopConfigParaPtr) {
    struct CBellResult* cBellResultPtr = cBellResultCreate();
    ccaculate(&cBellhopConfigParaPtr->stringPara.title.arr,
              &cBellhopConfigParaPtr->stringPara.title.size,
              &cBellhopConfigParaPtr->digitalPara.freq,
              &cBellhopConfigParaPtr->digitalPara.ISIGNAL,
              &cBellhopConfigParaPtr->digitalPara.Nimage,
              &cBellhopConfigParaPtr->digitalPara.Ibwin,
              &cBellhopConfigParaPtr->digitalPara.deltas,
              &cBellhopConfigParaPtr->digitalPara.zBox,
              &cBellhopConfigParaPtr->digitalPara.rBox,
              &cBellhopConfigParaPtr->digitalPara.epmult,
              &cBellhopConfigParaPtr->digitalPara.rloop,
              cBellhopConfigParaPtr->stringPara.topopt.arr,
              cBellhopConfigParaPtr->stringPara.botopt.arr,
              &cBellhopConfigParaPtr->digitalPara.DepthB,
              cBellhopConfigParaPtr->stringPara.runtype.arr,
              cBellhopConfigParaPtr->stringPara.beamtype.arr,
              &cBellResultPtr->curveResultPtr->curveLengthArr,
              &cBellResultPtr->curveResultPtr->curveContain,
              &cBellhopConfigParaPtr->digitalPara.NMedia,
              &cBellhopConfigParaPtr->digitalPara.zSSPV.arr,
              &cBellhopConfigParaPtr->digitalPara.cSSPV.arr,
              &cBellhopConfigParaPtr->digitalPara.SD.arr,
              &cBellhopConfigParaPtr->digitalPara.RD.arr,
              &cBellhopConfigParaPtr->digitalPara.R.arr,
              &cBellhopConfigParaPtr->digitalPara.angle.arr,
              &cBellhopConfigParaPtr->digitalPara.zSSPV.size,
              &cBellhopConfigParaPtr->digitalPara.cSSPV.size,
              &cBellhopConfigParaPtr->digitalPara.SD.size,
              &cBellhopConfigParaPtr->digitalPara.NSD,
              &cBellhopConfigParaPtr->digitalPara.RD.size,
              &cBellhopConfigParaPtr->digitalPara.NRD,
              &cBellhopConfigParaPtr->digitalPara.R.size,
              &cBellhopConfigParaPtr->digitalPara.NR,
              &cBellhopConfigParaPtr->digitalPara.angle.size,
              &cBellhopConfigParaPtr->digitalPara.alphaR,
              &cBellhopConfigParaPtr->digitalPara.betaR,
              &cBellhopConfigParaPtr->digitalPara.rhoR,
              &cBellhopConfigParaPtr->digitalPara.alphaI,
              &cBellhopConfigParaPtr->digitalPara.betaI,
              &cBellhopConfigParaPtr->digitalPara.BSigma,
              &cBellhopConfigParaPtr->digitalPara.TSigma,
              &cBellhopConfigParaPtr->digitalPara.NPts,
              &cBellhopConfigParaPtr->digitalPara.NBEAMS,
              &cBellhopConfigParaPtr->digitalPara.NbtyPts,
              &cBellhopConfigParaPtr->digitalPara.btyPts,
              &cBellResultPtr->energyResultPtr->arr,
              &cBellResultPtr->energyResultPtr->sizeVector.arr[0],
              &cBellResultPtr->energyResultPtr->sizeVector.arr[1],
              &cBellResultPtr->arrResultPtr->nArr,
              &cBellResultPtr->arrResultPtr->ampArr,
              &cBellResultPtr->arrResultPtr->phaseArr,
              &cBellResultPtr->arrResultPtr->delayArr,
              &cBellResultPtr->arrResultPtr->srcAngleArr,
              &cBellResultPtr->arrResultPtr->recvAngleArr,
              &cBellResultPtr->arrResultPtr->nTopBncArr,
              &cBellResultPtr->arrResultPtr->nBotBncArr,
              &cBellResultPtr->arrResultPtr->sizeVector.arr[0],
              &cBellResultPtr->arrResultPtr->sizeVector.arr[1]);
    return cBellResultPtr;
}
//------------------------------------------------------------------------------------------
// Curve类的成员函数
//------------------------------------------------------------------------------------------
struct Curve* curveCreate(const struct CurveResult* curveResultPtr, int th) {
    double* start = curveResultPtr->curveContain;
    for (int i = 0; i < th; ++i) {
        start += curveResultPtr->curveLengthArr[i + 1];
    }
    double* end = start + curveResultPtr->curveLengthArr[th + 1];
    struct Curve* curvePtr = (struct Curve*)malloc(sizeof(struct Curve));
    curvePtr->start = start;
    curvePtr->end = end;
    curvePtr->destory = &curveDestory;
    curvePtr->size = &curveSize;
    curvePtr->index = &curveIndex;
    return curvePtr;
}
void curveDestory(struct Curve* curvePtr) { free(curvePtr); }
int curveSize(const struct Curve* curvePtr) {
    return (int)((curvePtr->end - curvePtr->start) / 2);
}
struct Point curveIndex(const struct Curve* curvePtr, int idx) {
    struct Point point = {curvePtr->start[idx * 2 + 0],
                          curvePtr->start[idx * 2 + 1]};
    return point;
}
//------------------------------------------------------------------------------------------
// EnergyResult类的成员函数
//------------------------------------------------------------------------------------------
struct EnergyResult* energyResultCreate() {
    struct EnergyResult* energyResultPtr =
        (struct EnergyResult*)malloc(sizeof(struct EnergyResult));
    memset(energyResultPtr, 0, sizeof(struct EnergyResult));
    energyResultPtr->sizeVector = vectorintCreatebyCapacity(2);
    vectorintPushback(&energyResultPtr->sizeVector, 0);
    vectorintPushback(&energyResultPtr->sizeVector, 0);
    energyResultPtr->destory = &energyResultDestory;
    energyResultPtr->size = &energyResultSize;
    energyResultPtr->index = &energyResultIndex;
    return energyResultPtr;
}
void energyResultDestory(struct EnergyResult* energyResultPtr) {
    delete_u_1d();
    vectorintDestory(&energyResultPtr->sizeVector);
    free(energyResultPtr);
}
int energyResultSize(struct EnergyResult* energyResultPtr, unsigned int idx) {
    return vectorintIndex(&energyResultPtr->sizeVector, idx);
}
struct ComplexFloat energyResultIndex(struct EnergyResult* energyResultPtr,
                                      unsigned int row, unsigned int col) {
    return energyResultPtr
        ->arr[col * energyResultPtr->size(energyResultPtr, 0) + row];
}
//------------------------------------------------------------------------------------------
// ArrResult类的成员函数
//------------------------------------------------------------------------------------------
struct ArrResult* arrResultCreate() {
    struct ArrResult* arrResultPtr =
        (struct ArrResult*)malloc(sizeof(struct ArrResult));
    memset(arrResultPtr, 0, sizeof(struct ArrResult));
    arrResultPtr->sizeVector = vectorintCreatebyCapacity(2);
    vectorintPushback(&arrResultPtr->sizeVector, 0);
    vectorintPushback(&arrResultPtr->sizeVector, 0);
    arrResultPtr->destory = &arrResultDestory;
    arrResultPtr->size = &arrResultSize;
    arrResultPtr->receiverCreate = &ArrResultReceiverCreate;
    return arrResultPtr;
}
void arrResultDestory(struct ArrResult* arrResultPtr) {
    vectorintDestory(&arrResultPtr->sizeVector);
    delete_arr_result();
    free(arrResultPtr);
}
int arrResultSize(const struct ArrResult* arrResultPtr, unsigned int idx) {
    return vectorintIndex(&arrResultPtr->sizeVector, idx);
}
struct ArrResultReceiver* ArrResultReceiverCreate(
    const struct ArrResult* arrResultPtr, unsigned int row, unsigned int col) {
    struct ArrResultReceiver* arrResultReceiverPtr =
        (struct ArrResultReceiver*)malloc(sizeof(struct ArrResultReceiver));
    memset(arrResultReceiverPtr, 0, sizeof(struct ArrResultReceiver));
    arrResultReceiverPtr->curveNum =
        arrResultPtr
            ->nArr[row * vectorintIndex(&arrResultPtr->sizeVector, 1) + col];
    arrResultReceiverPtr->ampArr = arrResultPtr->ampArr;
    arrResultReceiverPtr->phaseArr = arrResultPtr->phaseArr;
    arrResultReceiverPtr->delayArr = arrResultPtr->delayArr;
    arrResultReceiverPtr->srcAngleArr = arrResultPtr->srcAngleArr;
    arrResultReceiverPtr->recvAngleArr = arrResultPtr->recvAngleArr;
    arrResultReceiverPtr->nTopBncArr = arrResultPtr->nTopBncArr;
    arrResultReceiverPtr->nBotBncArr = arrResultPtr->nBotBncArr;
    for (int i = 0; i <= row; ++i) {
        for (int j = 0; j < col; ++j) {
            arrResultReceiverPtr->ampArr +=
                arrResultPtr
                    ->nArr[i * vectorintIndex(&arrResultPtr->sizeVector, 1) +
                           j];
            arrResultReceiverPtr->phaseArr +=
                arrResultPtr
                    ->nArr[i * vectorintIndex(&arrResultPtr->sizeVector, 1) +
                           j];
            arrResultReceiverPtr->delayArr +=
                arrResultPtr
                    ->nArr[i * vectorintIndex(&arrResultPtr->sizeVector, 1) +
                           j];
            arrResultReceiverPtr->srcAngleArr +=
                arrResultPtr
                    ->nArr[i * vectorintIndex(&arrResultPtr->sizeVector, 1) +
                           j];
            arrResultReceiverPtr->recvAngleArr +=
                arrResultPtr
                    ->nArr[i * vectorintIndex(&arrResultPtr->sizeVector, 1) +
                           j];
            arrResultReceiverPtr->nTopBncArr +=
                arrResultPtr
                    ->nArr[i * vectorintIndex(&arrResultPtr->sizeVector, 1) +
                           j];
            arrResultReceiverPtr->nBotBncArr +=
                arrResultPtr
                    ->nArr[i * vectorintIndex(&arrResultPtr->sizeVector, 1) +
                           j];
        }
    }
    arrResultReceiverPtr->destory = &ArrResultReceiverDestory;
    arrResultReceiverPtr->getCurveNum = &arrResultReceiverGetCurveNum;
    arrResultReceiverPtr->ampIndex = &arrResultReceiverAmpIndex;
    arrResultReceiverPtr->phaseIndex = &arrResultReceiverPhaseIndex;
    arrResultReceiverPtr->delayIndex = &arrResultReceiverDelayIndex;
    arrResultReceiverPtr->srcAngleIndex = &arrResultReceiverSrcAngleIndex;
    arrResultReceiverPtr->recvAngleIndex = &arrResultReceiverRecvAngleIndex;
    arrResultReceiverPtr->nTopBncIndex = &arrResultReceiverNTopBncIndex;
    arrResultReceiverPtr->nBotBncIndex = &arrResultReceiverNBotBncIndex;
    return arrResultReceiverPtr;
}
void ArrResultReceiverDestory(struct ArrResultReceiver* arrResultReceiverPtr) {
    free(arrResultReceiverPtr);
}
int arrResultReceiverGetCurveNum(
    const struct ArrResultReceiver* arrResultReceiverPtr) {
    return arrResultReceiverPtr->curveNum;
}
float arrResultReceiverAmpIndex(
    const struct ArrResultReceiver* arrResultReceiverPtr, unsigned int idx) {
    return arrResultReceiverPtr->ampArr[idx];
}
float arrResultReceiverPhaseIndex(
    const struct ArrResultReceiver* arrResultReceiverPtr, unsigned int idx) {
    return arrResultReceiverPtr->phaseArr[idx];
}
float arrResultReceiverDelayIndex(
    const struct ArrResultReceiver* arrResultReceiverPtr, unsigned int idx) {
    return arrResultReceiverPtr->delayArr[idx];
}
float arrResultReceiverSrcAngleIndex(
    const struct ArrResultReceiver* arrResultReceiverPtr, unsigned int idx) {
    return arrResultReceiverPtr->srcAngleArr[idx];
}
float arrResultReceiverRecvAngleIndex(
    const struct ArrResultReceiver* arrResultReceiverPtr, unsigned int idx) {
    return arrResultReceiverPtr->recvAngleArr[idx];
}
int arrResultReceiverNTopBncIndex(
    const struct ArrResultReceiver* arrResultReceiverPtr, unsigned int idx) {
    return arrResultReceiverPtr->nTopBncArr[idx];
}
int arrResultReceiverNBotBncIndex(
    const struct ArrResultReceiver* arrResultReceiverPtr, unsigned int idx) {
    return arrResultReceiverPtr->nBotBncArr[idx];
}
//------------------------------------------------------------------------------------------
// CBellResult类的成员函数
//------------------------------------------------------------------------------------------
struct CBellResult* cBellResultCreate() {
    struct CBellResult* cBellResultPtr =
        (struct CBellResult*)malloc(sizeof(struct CBellResult));
    cBellResultPtr->curveResultPtr = curveResultCreate();
    cBellResultPtr->energyResultPtr = energyResultCreate();
    cBellResultPtr->arrResultPtr = arrResultCreate();
    cBellResultPtr->destory = &cBellResultDestory;
    return cBellResultPtr;
}
void cBellResultDestory(struct CBellResult* cBellResultPtr) {
    cBellResultPtr->curveResultPtr->destory(cBellResultPtr->curveResultPtr);
    cBellResultPtr->energyResultPtr->destory(cBellResultPtr->energyResultPtr);
    cBellResultPtr->arrResultPtr->destory(cBellResultPtr->arrResultPtr);
    delete_refcomod_config_para();
    delete_sdrdrmod_config_para();
    delte_beampatternmod_config_para();
    delete_bdrymod_config_para();
    delete_arrmod_config_para();
    delete_anglemod_config_para();
    free(cBellResultPtr);
}