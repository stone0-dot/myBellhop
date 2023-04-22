#include "stdio.h"
#include "readin.h"

int main(){
    readConfigIn();
    printf("%s\n", stringPara_ptr->title);
    printf("%s\n", stringPara_ptr->beamtype);
    printf("%s\n", stringPara_ptr->botopt);
    printf("%s\n", stringPara_ptr->runtype);
    printf("%s\n", stringPara_ptr->topopt);
    deleteAll();
    return 0;
}