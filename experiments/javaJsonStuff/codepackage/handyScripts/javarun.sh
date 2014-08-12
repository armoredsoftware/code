#!/bin/sh
'to be ran from the path just above the codepackage folder'
java -Djava.library.path=codepackage/ -cp .:codepackage/json-simple-1.1.1.jar codepackage.FakeJavaMeasurer



