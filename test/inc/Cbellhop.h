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
// 创建Cbellhop参数包
struct CBellhopConfigPara* cBellhopConfigParaCreate();
// 析构Cbellhop参数包
void cBellhopConfigParaDestory(struct CBellhopConfigPara* configParaPtr);
// 设置title
void cBellhopConfigParaSetTitle(struct CBellhopConfigPara* configParaPtr,
                                const char* title);
// 设置topopt
void cBellhopConfigParaSetTopopt(struct CBellhopConfigPara* configParaPtr,
                                 const char* topopt);
// 设置tbotopt
void cBellhopConfigParaSetBotopt(struct CBellhopConfigPara* configParaPtr,
                                 const char* botopt);
// 设置runtype
void cBellhopConfigParaSetRuntype(struct CBellhopConfigPara* configParaPtr,
                                  const char* runtype);
// 设置beamtype
void cBellhopConfigParaSetBeamtype(struct CBellhopConfigPara* configParaPtr,
                                   const char* beamtype);
// 设置freq
void cBellhopConfigParaSetFreq(struct CBellhopConfigPara* configParaPtr,
                               const float freq);
// 设置NMedia
void cBellhopConfigParaSetNMedia(struct CBellhopConfigPara* configParaPtr,
                                 const int NMedia);
// 设置NPts
void cBellhopConfigParaSetNPts(struct CBellhopConfigPara* configParaPtr,
                               const int NPts);
// 设置TSigma
void cBellhopConfigParaSetTSigma(struct CBellhopConfigPara* configParaPtr,
                                 const float TSigma);
// 设置BSigma
void cBellhopConfigParaSetBSigma(struct CBellhopConfigPara* configParaPtr,
                                 const float BSigma);
// 设置DepthB
void cBellhopConfigParaSetDepthB(struct CBellhopConfigPara* configParaPtr,
                                 const float DepthB);
// 设置zSSPV
void cBellhopConfigParaSetzSSPV(struct CBellhopConfigPara* configParaPtr,
                                const float* zSSPV, const unsigned int len);
// 设置cSSPV
void cBellhopConfigParaSetcSSPV(struct CBellhopConfigPara* configParaPtr,
                                const float* cSSPV, const unsigned int len);
// 设置alphaR
void cBellhopConfigParaSetalphaR(struct CBellhopConfigPara* configParaPtr,
                                 const double alphaR);
// 设置betaR
void cBellhopConfigParaSetbetaR(struct CBellhopConfigPara* configParaPtr,
                                const double betaR);
// 设置rhoR
void cBellhopConfigParaSetrhoR(struct CBellhopConfigPara* configParaPtr,
                               const double rhoR);
// 设置alphaI
void cBellhopConfigParaSetalphaI(struct CBellhopConfigPara* configParaPtr,
                                 const double alphaI);
// 设置betaI
void cBellhopConfigParaSetbetaI(struct CBellhopConfigPara* configParaPtr,
                                const double betaI);
// 设置NSD声源数量
void cBellhopConfigParaSetNSD(struct CBellhopConfigPara* configParaPtr,
                              const unsigned int NSD);
// 设置SD
void cBellhopConfigParaSetSD(struct CBellhopConfigPara* configParaPtr,
                             const float* SD, const unsigned int len);
// 设置NRD接收源数量
void cBellhopConfigParaSetNRD(struct CBellhopConfigPara* configParaPtr,
                              const unsigned int NRD);
// 设置RD
void cBellhopConfigParaSetRD(struct CBellhopConfigPara* configParaPtr,
                             const float* RD, const unsigned int len);
// 设置NR
void cBellhopConfigParaSetNR(struct CBellhopConfigPara* configParaPtr,
                             const unsigned int NR);
// 设置R
void cBellhopConfigParaSetR(struct CBellhopConfigPara* configParaPtr,
                            const float* R, const unsigned int len);
// 设置NBEAMS
void cBellhopConfigParaSetNBEAMS(struct CBellhopConfigPara* configParaPtr,
                                 const unsigned int NBEAMS);
// 设置angle
void cBellhopConfigParaSetangle(struct CBellhopConfigPara* configParaPtr,
                                const float* angle, const unsigned int len);
// 设置deltas
void cBellhopConfigParaSetdeltas(struct CBellhopConfigPara* configParaPtr,
                                 const float deltas);
// 设置zBox
void cBellhopConfigParaSetzBox(struct CBellhopConfigPara* configParaPtr,
                               const float zBox);
// 设置rBox
void cBellhopConfigParaSetrBox(struct CBellhopConfigPara* configParaPtr,
                               const float rBox);
// 设置epmult
void cBellhopConfigParaSetepmult(struct CBellhopConfigPara* configParaPtr,
                                 const float epmult);
// 设置rloop
void cBellhopConfigParaSetrloop(struct CBellhopConfigPara* configParaPtr,
                                const float rloop);
// 设置Nimage
void cBellhopConfigParaSetNimage(struct CBellhopConfigPara* configParaPtr,
                                 const unsigned int Nimage);
// 设置Ibwin
void cBellhopConfigParaSetIbwin(struct CBellhopConfigPara* configParaPtr,
                                const unsigned int Ibwin);
// 设置ISIGNAL
void cBellhopConfigParaSetISIGNAL(struct CBellhopConfigPara* configParaPtr,
                                  const unsigned int ISIGNAL);

// 返回的声线结果
struct CurveResult;
// 创建声线结果对象
struct CurveResult* curveResultCreate();
void curveResultDestory(struct CurveResult* curveResultPtr);
// 返回计算得到的声线总条数curveNum
int curveResultGetCurveNum(const struct CurveResult* curveResultPtr);

// 声线计算api，在配置好参数后调用该函数进行计算
struct CurveResult* run(struct FortranConfigPara* configParaPtr);
// cbellhop运行
struct CurveResult* cBellhopRun(
    struct CBellhopConfigPara* cBellhopConfigParaPtr);

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