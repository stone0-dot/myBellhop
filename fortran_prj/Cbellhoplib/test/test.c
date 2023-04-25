#include "stdio.h"
#include "readin.h"

int main(){
    readConfigIn();
    run();
    int curveNum = getCurveNum();
    printf("number of curves:%d\n", curveNum);
    struct Curve* curve2 = curveCreate(2);
    int curve2Size = curveSize(curve2);
    printf("The curve has pointNum:%d\n", curve2Size);
    for(int i = 0; i < curve2Size; ++i){
        struct Point point = curveIndex(curve2, i);
        printf("%lf %lf\n", point.x, point.y);
    }
    curveDestory(curve2);
    deleteBellhopCache();
    return 0;
}