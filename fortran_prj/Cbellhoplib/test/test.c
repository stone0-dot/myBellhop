#include "stdio.h"
#include "readin.h"

int main(){
    readConfigIn();
    run();
    struct Line line = lineResult_getLine(1);
    for(int i = 0; i < line.pointNum; ++i){
        printf("%lf %lf\n", line.contain[i][0], line.contain[i][1]);
    }
    delete_line(&line);
    deleteAll();
    return 0;
}