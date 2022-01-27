#
#!/bin/csh
#

set wd = `pwd`
set oslo_iris_ssw = ~viggoh/www_docs/download/iris/oslo/
set iris_dir = ~viggoh/iris/
cd $iris_dir
cvs update
tar cvf iris_idl.tar idl
/bin/cp iris_idl.tar $oslo_iris_ssw
cd $oslo_iris_ssw
tar xvf iris_idl.tar 

cd $wd
