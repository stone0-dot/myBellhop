#pragma once
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
void readConfigIn();//fortran版本的读取参数文件api
void run();//声线计算api，在配置好参数后调用该函数进行计算
void deleteBellhopCache();//释放参数配置与返回结果占用的内存

struct CurveResult;
int getCurveNum();//返回计算得到的声线总条数curveNum

struct Curve;
struct Curve* curveCreate(int th);//创建声线对象，注:本函数不做运行时检查，自行保证索引值不越界或自行在上层做检查(th<curveNum)
void curveDestory(struct Curve* curvePtr);//释放Curve对象，与curveCreate配对使用
int curveSize(const struct Curve* curvePtr);//获取Curve对象的总点数pointNum
struct Point{
    double x;
    double y;
};
struct Point curveIndex(const struct Curve* curve, int th);//输入索引值，返回point(x,y),注:本函数不做运行时检查，自行保证索引值不越界或自行在上层做检查(th<pointNum)