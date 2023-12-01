cat input   | sed -e 's/one/o1e/g' | sed -e 's/two/t2o/g' | sed -e 's/three/t3e/g' | sed -e 's/four/f4r/g' | sed -e 's/five/f5e/g' | sed -e 's/six/s6x/g' | sed -e 's/seven/s7n/g
' | sed -e 's/eight/e8t/g' | sed -e 's/nine/n9e/g' | tr -dc "0-9\\n" | while read string; do echo "${string:0:1}${string: -1}"; done | paste -sd+ |bc
