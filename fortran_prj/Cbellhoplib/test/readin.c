#include "readin.h"
#include "interface.h"

struct StringPara stringPara;
struct DigitalPara digitalPara;

struct StringPara* stringPara_ptr = &stringPara;
struct DigitalPara* digitalPara_ptr = &digitalPara;

void readConfigIn(){
    readconfig(stringPara_ptr->title, &digitalPara_ptr->freq,
    &digitalPara_ptr->isingl, &digitalPara_ptr->nimage, &digitalPara_ptr->ibwin,
    &digitalPara_ptr->deltas, &digitalPara_ptr->maxn, &digitalPara_ptr->zbox,
    &digitalPara_ptr->rbox, &digitalPara_ptr->epmult, &digitalPara_ptr->rloop,
    stringPara_ptr->topopt, &digitalPara_ptr->deptht, &digitalPara_ptr->cpt.real,
    &digitalPara_ptr->cpt.imag, &digitalPara_ptr->rhot, stringPara_ptr->botopt,
    &digitalPara_ptr->depthb, &digitalPara_ptr->cpb.real, &digitalPara_ptr->cpb.imag,
    &digitalPara_ptr->rhob, stringPara_ptr->runtype, stringPara_ptr->beamtype);
}

void deleteAll(){
    delete_c_chars(stringPara_ptr->beamtype);
    delete_c_chars(stringPara_ptr->botopt);
    delete_c_chars(stringPara_ptr->runtype);
    delete_c_chars(stringPara_ptr->title);
    delete_c_chars(stringPara_ptr->topopt);
}