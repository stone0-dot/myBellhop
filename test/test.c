#include <stdio.h>

#include "Cbellhop.h"

int main() {
    struct ConfigPara* configParaPtr = configParaCreate();
    configParaReadConfigIn(configParaPtr);
    struct CurveResult* curveResultPtr = curveResultCreate();
    run(configParaPtr, curveResultPtr);
    int curveNum = curveResultGetCurveNum(curveResultPtr);
    printf("number of curves:%d\n", curveNum);
    struct Curve* curvePtr2 = curveCreate(curveResultPtr, 2);
    int pointNum2 = curveSize(curvePtr2);
    printf("The 2th curve has number of ponint:%d\n", pointNum2);
    for (int i = 0; i < pointNum2; ++i) {
        struct Point point = curveIndex(curveResultPtr, curvePtr2, i);
        printf("%lf %lf\n", point.x, point.y);
    }
    configParaDestory(configParaPtr);
    curveResultDestory(curveResultPtr);
    curveDestory(curvePtr2);
    return 0;
}