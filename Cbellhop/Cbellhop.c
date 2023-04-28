#include "Cbellhop.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "Vector.h"
#include "interface.h"

INIT_VECTOR_TYPE(char)
INIT_VECTOR_TYPE(float)

struct Complex {
    double real;
    double imag;
};
struct StringPara {
    char* title;
    char* topopt;
    char* botopt;
    char* runtype;
    char* beamtype;
};
struct DigitalPara {
    float freq;
    int isingl;
    int nimage;
    int ibwin;
    float deltas;
    int maxn;
    float zbox;
    float rbox;
    float epmult;
    float rloop;
    float deptht;
    struct Complex cpt;
    double rhot;
    float depthb;
    struct Complex cpb;
    double rhob;
};

struct FortranConfigPara {
    struct StringPara stringPara;
    struct DigitalPara digitalPara;
};
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
//--------------------------------------------------------------------------------
struct CBellhopStringPara {
    struct Vectorchar title;
    struct Vectorchar topopt;
    struct Vectorchar botopt;
    struct Vectorchar runtype;
    struct Vectorchar beamtype;
};
struct CBellhopDigitalPara {
    float freq;
    int NMedia;
    int NPts;
    float TSigma;
    float BSigma;
    float DepthB;
    struct Vectorfloat zSSPV;
    struct Vectorfloat cSSPV;
    double alphaR;
    double betaR;
    double rhoR;
    double alphaI;
    double betaI;
    int NSD;
    struct Vectorfloat SD;
    int NRD;
    struct Vectorfloat RD;
    int NR;
    struct Vectorfloat R;
    int NBEAMS;
    struct Vectorfloat angle;
    float deltas;
    float zBox;
    float rBox;
};
struct CBellhopConfigPara {
    struct CBellhopStringPara stringPara;
    struct CBellhopDigitalPara digitalPara;
};
struct CBellhopConfigPara* cBellhopConfigParaCreate() {
    struct CBellhopConfigPara* cBellhopConfigParaPtr =
        (struct CBellhopConfigPara*)malloc(sizeof(struct CBellhopConfigPara));
    memset(cBellhopConfigParaPtr, 0, sizeof(struct CBellhopConfigPara));
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
//----------------------------------------------------------------------------------------
struct CurveResult {
    int* curveLengthArr;
    double* curveContain;
};
struct CurveResult* curveResultCreate() {
    struct CurveResult* curveResultPtr =
        (struct CurveResult*)malloc(sizeof(struct CurveResult));
    return curveResultPtr;
}
void curveResultDestory(struct CurveResult* curveResultPtr) {
    delete_c_line_length(&curveResultPtr->curveLengthArr);
    delete_growth_double_vector(&curveResultPtr->curveContain);
    free(curveResultPtr);
}
int curveResultGetCurveNum(const struct CurveResult* curveResultPtr) {
    return curveResultPtr->curveLengthArr[0];
}

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
//------------------------------------------------------------------------------------------
struct Curve {
    int start;
    int end;
};
struct Curve* curveCreate(const struct CurveResult* curveResultPtr, int th) {
    // 本函数不做运行时检查，自行保证索引值不越界或自行在上层做检查(th<curveNum)
    int start = 0;
    for (int i = 0; i < th; ++i) {
        start += curveResultPtr->curveLengthArr[i + 1];
    }
    int end = start + curveResultPtr->curveLengthArr[th + 1] - 1;
    struct Curve* curvePtr = (struct Curve*)malloc(sizeof(struct Curve));
    curvePtr->start = start;
    curvePtr->end = end;
    return curvePtr;
}
void curveDestory(struct Curve* curvePtr) {
    // 释放Curve对象，与curveCreate配对使用
    free(curvePtr);
}
int curveSize(const struct Curve* curvePtr) {
    // 获取curve的总点数pointNum
    return (curvePtr->end - curvePtr->start + 1) / 2;
}
struct Point curveIndex(const struct CurveResult* curveResultPtr,
                        const struct Curve* curvePtr, int th) {
    // 本函数不做运行时检查，自行保证索引值不越界或自行在上层做检查(th<pointNum)
    struct Point point = {
        curveResultPtr->curveContain[curvePtr->start + th * 2 + 0],
        curveResultPtr->curveContain[curvePtr->start + th * 2 + 1]};
    return point;
}