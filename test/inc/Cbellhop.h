#pragma once

struct ConfigPara;
struct ConfigPara* configParaCreate();
void configParaDestory(struct ConfigPara* configParaPtr);
// fortran版本的读取参数文件api
void configParaReadConfigIn(struct ConfigPara* configParaPtr);

struct CurveResult;
struct CurveResult* curveResultCreate();
void curveResultDestory(struct CurveResult* curveResultPtr);
// 返回计算得到的声线总条数curveNum
int curveResultGetCurveNum(const struct CurveResult* curveResultPtr);

// 声线计算api，在配置好参数后调用该函数进行计算
void run(const struct ConfigPara* configParaPtr,
         struct CurveResult* curveResultPtr);

struct Curve;
// 创建声线对象，注:本函数不做运行时检查，自行保证索引值不越界或自行在上层做检查(th<curveNum)
struct Curve* curveCreate(const struct CurveResult* curveResultPtr, int th);
// 释放Curve对象，与curveCreate配对使用
void curveDestory(struct Curve* curvePtr);
// 获取Curve对象的总点数pointNum
int curveSize(const struct Curve* curvePtr);
struct Point {
    double x;
    double y;
};
// 输入索引值，返回point(x,y),注:本函数不做运行时检查，自行保证索引值不越界或自行在上层做检查(th<pointNum)
struct Point curveIndex(const struct CurveResult* curveResultPtr,
                        const struct Curve* curvePtr, int th);