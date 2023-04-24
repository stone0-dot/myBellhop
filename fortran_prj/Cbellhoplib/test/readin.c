#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "readin.h"
#include "interface.h"

struct StringPara stringPara = {NULL, NULL, NULL, NULL, NULL};
struct DigitalPara digitalPara;

struct StringPara* stringPara_ptr = &stringPara;
struct DigitalPara* digitalPara_ptr = &digitalPara;

struct LineResult{
    int* c_line_length;
    double* c_xv_result;
};

struct LineResult lineResult;
struct LineResult* lineResult_ptr = &lineResult;

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
    &digitalPara_ptr->rhob, stringPara_ptr->runtype, stringPara_ptr->beamtype, &lineResult_ptr->c_line_length,
    &lineResult_ptr->c_xv_result);
}

void deleteAll(){
    delete_c_chars(&stringPara_ptr->beamtype);
    delete_c_chars(&stringPara_ptr->botopt);
    delete_c_chars(&stringPara_ptr->runtype);
    delete_c_chars(&stringPara_ptr->title);
    delete_c_chars(&stringPara_ptr->topopt);
    delete_c_line_length(&lineResult_ptr->c_line_length);
    delete_growth_double_vector(&lineResult_ptr->c_xv_result);
}

int lineResult_getLineNum(){
    return lineResult_ptr->c_line_length[0];
}

int lineResult_getPointNum(int order){
    return lineResult_ptr->c_line_length[order + 1] / 2;
}

struct Line lineResult_getLine(int order){
    int start = 0;
    for(int i = 0; i < order; ++i){
        start += lineResult_ptr->c_line_length[i + 1];
    }
    int pointNum = lineResult_ptr->c_line_length[order + 1] / 2;
    double** contain = (double**)malloc(sizeof(double*)*pointNum);
    for(int i = 0; i < pointNum; ++i){
        contain[i] = (double*)malloc(sizeof(double)*2);
    }
    for(int i = start; i < start + lineResult_ptr->c_line_length[order + 1];){
        for(int j = 0; j < 2; ++j){
            contain[(i - start) / 2][j] = lineResult_ptr->c_xv_result[i];
            ++i;
        }
    }
    struct Line line = {pointNum, contain};
    return line;
}

void delete_line(struct Line* line){
    for(int i = 0; i < line->pointNum; ++i){
        free(line->contain[i]);
    }
    free(line->contain);
}