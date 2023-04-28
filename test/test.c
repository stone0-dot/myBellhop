#include <stdio.h>

#include "Cbellhop.h"

int main() {
    struct FortranConfigPara* fortranConfigParaPtr = fortranConfigParaCreate();
    fortranConfigParaReadConfigIn(fortranConfigParaPtr);
    struct CurveResult* curveResultPtr = curveResultCreate();
    curveResultPtr = run(fortranConfigParaPtr);
    int curveNum = curveResultGetCurveNum(curveResultPtr);
    printf("number of curves:%d\n", curveNum);
    struct Curve* curvePtr2 = curveCreate(curveResultPtr, 2);
    int pointNum2 = curveSize(curvePtr2);
    printf("The 2th curve has number of ponint:%d\n", pointNum2);
    for (int i = 0; i < pointNum2; ++i) {
        struct Point point = curveIndex(curveResultPtr, curvePtr2, i);
        printf("%lf %lf\n", point.x, point.y);
    }
    fortranConfigParaDestory(fortranConfigParaPtr);
    curveResultDestory(curveResultPtr);
    curveDestory(curvePtr2);
    return 0;
}