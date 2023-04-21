#pragma once

struct Complex
{
    float real;
    float imag;
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
    float rhot;
    float depthb;
    struct Complex cpb;
    float rhob;
};

extern struct StringPara* stringPara_ptr;
extern struct DigitalPara* digitalPara_ptr;

void readConfigIn();
void deleteAll();