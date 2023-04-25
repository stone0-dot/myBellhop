#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "Cbellhop.h"
#include "interface.h"

struct StringPara stringPara = {NULL, NULL, NULL, NULL, NULL};
struct DigitalPara digitalPara;

struct StringPara* stringPara_ptr = &stringPara;
struct DigitalPara* digitalPara_ptr = &digitalPara;

struct CurveResult{
    int* curveLengthArr;
    double* curveContain;
};

struct CurveResult curveResult;
struct CurveResult* curveResultPtr = &curveResult;

void readConfigIn(){
    memset(digitalPara_ptr, 0, sizeof(digitalPara));
    readconfig(&stringPara_ptr->title, &digitalPara_ptr->freq,
    &digitalPara_ptr->isingl, &digitalPara_ptr->nimage, &digitalPara_ptr->ibwin,
    &digitalPara_ptr->deltas, &digitalPara_ptr->maxn, &digitalPara_ptr->zbox,
    &digitalPara_ptr->rbox, &digitalPara_ptr->epmult, &digitalPara_ptr->rloop,
    &stringPara_ptr->topopt, &digitalPara_ptr->deptht, &digitalPara_ptr->cpt.real,
    &digitalPara_ptr->cpt.imag, &digitalPara_ptr->rhot, &stringPara_ptr->botopt,
    &digitalPara_ptr->depthb, &digitalPara_ptr->cpb.real, &digitalPara_ptr->cpb.imag,
    &digitalPara_ptr->rhob, &stringPara_ptr->runtype, &stringPara_ptr->beamtype);
}

void run(){
    caculate(&digitalPara_ptr->freq, &digitalPara_ptr->isingl, &digitalPara_ptr->nimage,
    &digitalPara_ptr->ibwin, &digitalPara_ptr->deltas, &digitalPara_ptr->maxn, &digitalPara_ptr->zbox,
    &digitalPara_ptr->rbox, &digitalPara_ptr->epmult, &digitalPara_ptr->rloop,
    stringPara_ptr->topopt, &digitalPara_ptr->deptht, &digitalPara_ptr->cpt.real,
    &digitalPara_ptr->cpt.imag, &digitalPara_ptr->rhot, stringPara_ptr->botopt,
    &digitalPara_ptr->depthb, &digitalPara_ptr->cpb.real, &digitalPara_ptr->cpb.imag,
    &digitalPara_ptr->rhob, stringPara_ptr->runtype, stringPara_ptr->beamtype, &curveResultPtr->curveLengthArr,
    &curveResultPtr->curveContain);
}

void deleteBellhopCache(){
    delete_c_chars(&stringPara_ptr->beamtype);
    delete_c_chars(&stringPara_ptr->botopt);
    delete_c_chars(&stringPara_ptr->runtype);
    delete_c_chars(&stringPara_ptr->title);
    delete_c_chars(&stringPara_ptr->topopt);
    delete_c_line_length(&curveResultPtr->curveLengthArr);
    delete_growth_double_vector(&curveResultPtr->curveContain);
}

int getCurveNum(){
    //返回计算得到的声线总条数curveNum
    return curveResultPtr->curveLengthArr[0];
}

struct Curve{
    int start;
    int end;
};

struct Curve* curveCreate(int th){
    //本函数不做运行时检查，自行保证索引值不越界或自行在上层做检查(th<curveNum)
    int start = 0;
    for(int i = 0; i < th; ++i){
        start += curveResultPtr->curveLengthArr[i + 1];
    }
    int end = start + curveResultPtr->curveLengthArr[th + 1] - 1;
    struct Curve* curvePtr = (struct Curve*)malloc(sizeof(struct Curve));
    curvePtr->start = start;
    curvePtr->end = end;
    return curvePtr;
}

void curveDestory(struct Curve* curvePtr){
    //释放Curve对象，与curveCreate配对使用
    free(curvePtr);
}

int curveSize(const struct Curve* curvePtr){
    //获取curve的总点数pointNum
    return (curvePtr->end - curvePtr->start + 1) / 2;
}

struct Point curveIndex(const struct Curve* curvePtr, int th){
    //本函数不做运行时检查，自行保证索引值不越界或自行在上层做检查(th<pointNum)
    struct Point point = {curveResultPtr->curveContain[curvePtr->start + th*2 + 0], curveResultPtr->curveContain[curvePtr->start + th*2 + 1]};
    return point;
}