#!/bin/bash

scp -r dist/build/my-reflex-recipes/my-reflex-recipes.jsexe athena.dialup.mit.edu:Public/my-reflex-recipes.jsexe

scp ./index.html athena.dialup.mit.edu:Public/my-reflex-recipes.jsexe/index2.html
