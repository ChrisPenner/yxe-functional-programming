#!/bin/bash

graph(){
    dot -Tpng -Gdpi=300 > ./images/"$1".png
}

graph oo-hierarchy <<EOF
digraph {
  node [shape=record];
  class [label="{Class | {Data | Methods}}"]
  more [label="Other Functions?"]
}
EOF

graph nested-hierarchy <<EOF
digraph {
  node [shape=record];
  one [label="Data | Methods"]
  two [label="Data | Methods"]
  three [label="Data | Methods"]

  one -> two -> three
}
EOF

graph multiple-inheritance <<EOF
digraph {
  node [shape=record];
  parent1 [label="Data | Methods"]
  parent2 [label="Data | Methods"]
  child [label="Data | Methods"]
  
  parent1 -> child
  parent2 -> child
}
EOF

graph functional <<EOF
digraph {
  node [shape=record];
  Functions -> Data
}
EOF


graph typeclasses <<EOF
digraph {
  node [shape=record];
  Typeclass1 -> Data1
  Typeclass2 -> Data1
  Typeclass1 -> Data2
  Typeclass2 -> Data2
}
EOF
