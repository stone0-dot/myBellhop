#include "Cbellhop.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "interface.h"

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

struct ConfigPara {
    struct StringPara stringPara;
    struct DigitalPara digitalPara;
};
struct ConfigPara* configParaCreate() {
    struct ConfigPara* configParaPtr =
        (struct ConfigPara*)malloc(sizeof(struct ConfigPara));
    memset(&configParaPtr->stringPara, 0, sizeof(struct StringPara));
    memset(&configParaPtr->digitalPara, 0, sizeof(struct DigitalPara));
    return configParaPtr;
}
void configParaDestory(struct ConfigPara* configParaPtr) {
    delete_c_chars();
    free(configParaPtr);
}
void configParaReadConfigIn(struct ConfigPara* configParaPtr) {
    readconfig(
        &configParaPtr->stringPara.title, &configParaPtr->digitalPara.freq,
        &configParaPtr->digitalPara.isingl, &configParaPtr->digitalPara.nimage,
        &configParaPtr->digitalPara.ibwin, &configParaPtr->digitalPara.deltas,
        &configParaPtr->digitalPara.maxn, &configParaPtr->digitalPara.zbox,
        &configParaPtr->digitalPara.rbox, &configParaPtr->digitalPara.epmult,
        &configParaPtr->digitalPara.rloop, &configParaPtr->stringPara.topopt,
        &configParaPtr->digitalPara.deptht,
        &configParaPtr->digitalPara.cpt.real,
        &configParaPtr->digitalPara.cpt.imag, &configParaPtr->digitalPara.rhot,
        &configParaPtr->stringPara.botopt, &configParaPtr->digitalPara.depthb,
        &configParaPtr->digitalPara.cpb.real,
        &configParaPtr->digitalPara.cpb.imag, &configParaPtr->digitalPara.rhob,
        &configParaPtr->stringPara.runtype,
        &configParaPtr->stringPara.beamtype);
}
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

void run(struct ConfigPara* configParaPtr, struct CurveResult* curveResultPtr) {
    caculate(
        &configParaPtr->digitalPara.freq, &configParaPtr->digitalPara.isingl,
        &configParaPtr->digitalPara.nimage, &configParaPtr->digitalPara.ibwin,
        &configParaPtr->digitalPara.deltas, &configParaPtr->digitalPara.maxn,
        &configParaPtr->digitalPara.zbox, &configParaPtr->digitalPara.rbox,
        &configParaPtr->digitalPara.epmult, &configParaPtr->digitalPara.rloop,
        configParaPtr->stringPara.topopt, &configParaPtr->digitalPara.deptht,
        &configParaPtr->digitalPara.cpt.real,
        &configParaPtr->digitalPara.cpt.imag, &configParaPtr->digitalPara.rhot,
        configParaPtr->stringPara.botopt, &configParaPtr->digitalPara.depthb,
        &configParaPtr->digitalPara.cpb.real,
        &configParaPtr->digitalPara.cpb.imag, &configParaPtr->digitalPara.rhob,
        configParaPtr->stringPara.runtype, configParaPtr->stringPara.beamtype,
        &curveResultPtr->curveLengthArr, &curveResultPtr->curveContain);
}

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