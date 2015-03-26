#include "cute.h"
#include "ide_listener.h"
#include "xml_listener.h"
#include "cute_runner.h"

#include "TableTest.h"


void runSuite(int argc, char const *argv[]){
	cute::xml_file_opener xmlfile(argc,argv);
	cute::xml_listener<cute::ide_listener<>  > lis(xmlfile.out);
	cute::suite s=makeSuiteTableTest();
	cute::makeRunner(lis,argc,argv)(s, "RTester");
}

int main(int argc, char const *argv[]){
    runSuite(argc,argv);
}



