#pragma once

#include "readin.h"
struct Complex
{
    double real;
    double imag;
};
struct StringPara{
    char* title;
    char* topopt;
    char* botopt;
    char* runtype;
    char* beamtype;
};
struct DigitalPara{
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

extern struct StringPara* stringPara_ptr;
extern struct DigitalPara* digitalPara_ptr;
void readConfigIn();
void run();
void deleteBellhopCache();

struct CurveResult;
int getCurveNum();

struct Curve;
struct Curve* curveCreate(int th);
void curveDestory(struct Curve* curvePtr);
int curveSize(const struct Curve* curvePtr);
struct Point{
    double x;
    double y;
};
struct Point curveIndex(const struct Curve* curve, int th);