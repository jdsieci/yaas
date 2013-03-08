#!/bin/sh
cd `dirname $0`
exec erl -pa $PWD/ebin -pa $PWD/deps/*/ebin -name yaas@127.0.0.1 -setcookie yaas -s yaas start -config ebin/sys $@
