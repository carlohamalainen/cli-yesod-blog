#!/bin/bash

for x in `blog-utils --list | awk '{print $3}'`
do
    url="http://carlo-hamalainen.net/blog/$x"

    wget -O/dev/null -q $url || echo $url
done
