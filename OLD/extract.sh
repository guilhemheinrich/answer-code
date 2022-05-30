
#! /bin/bash
target=$1

if [ -d "${target%%.*}" ]; then
  # Control will enter here if $DIRECTORY exists.
    rm -r ${target%%.*}
fi
unzip ${target} -d ${target%%.*}

if [ -f "./${target%%.*}/Dockerfile" ]; then
    docker build ./${target%%.*} -t ${target%%.*}
fi
