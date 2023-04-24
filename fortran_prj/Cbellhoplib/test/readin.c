#include <stdio.h>
#include <string.h>
#include "readin.h"
#include "interface.h"

struct StringPara stringPara = {NULL, NULL, NULL, NULL, NULL};
struct DigitalPara digitalPara;

struct StringPara* stringPara_ptr = &stringPara;
struct DigitalPara* digitalPara_ptr = &digitalPara;

int* c_line_length;
double* c_xv_result;

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
    &digitalPara_ptr->rhob, stringPara_ptr->runtype, stringPara_ptr->beamtype, &c_line_length,
    &c_xv_result);
}

void deleteAll(){
    delete_c_chars(&stringPara_ptr->beamtype);
    delete_c_chars(&stringPara_ptr->botopt);
    delete_c_chars(&stringPara_ptr->runtype);
    delete_c_chars(&stringPara_ptr->title);
    delete_c_chars(&stringPara_ptr->topopt);
    delete_c_line_length(&c_line_length);
    delete_growth_double_vector(&c_xv_result);
}