#pragma once

// fortran接口参数包
struct FortranConfigPara;
// 创建输入fortran接口参数包
struct FortranConfigPara* fortranConfigParaCreate();
// fortran版本的读取参数文件api
void fortranConfigParaReadConfigIn(
    struct FortranConfigPara* fortranConfigParaPtr);
// 释放fortran接口的参数包
void fortranConfigParaDestory(struct FortranConfigPara* fortranConfigParaPtr);

// Cbellhop参数包
struct CBellhopConfigPara;
// 设置title
void configParaSetTitle(struct CBellhopConfigPara* configParaPtr, char* title);
// 设置topopt
void configParaSetTopopt(struct CBellhopConfigPara* configParaPtr,
                         char* topopt);
// 设置tbotopt
void configParaSetBotopt(struct CBellhopConfigPara* configParaPtr,
                         char* botopt);
// 设置runtype
void configParaSetRuntype(struct CBellhopConfigPara* configParaPtr,
                          char* runtype);
// 设置beamtype
void configParaSetBeamtype(struct CBellhopConfigPara* configParaPtr,
                           char* beamtype);
// 设置freq
void configParaSetFreq(struct CBellhopConfigPara* configParaPtr, float freq);
// 设置isingl
void configParaSetIsingl(struct CBellhopConfigPara* configParaPtr,
                         float isingl);

struct CurveResult;
struct CurveResult* curveResultCreate();
void curveResultDestory(struct CurveResult* curveResultPtr);
// 返回计算得到的声线总条数curveNum
int curveResultGetCurveNum(const struct CurveResult* curveResultPtr);

// 声线计算api，在配置好参数后调用该函数进行计算
struct CurveResult* run(struct FortranConfigPara* configParaPtr);

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