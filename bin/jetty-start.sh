#!/bin/sh

DH_DIR=/home/javaapps/sbt-projects/DeCSHighlighter
cd $DH_DIR/jetty-base || exit

../jetty-home-11.0.14/bin/jetty.sh start
#../jetty-home-12.0.8/bin/jetty.sh start    # mudar DH_DIR

ret="$?"

sleep 120s

cd -

exit $ret
