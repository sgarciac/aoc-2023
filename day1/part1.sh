cat input  | tr -dc "0-9\\n" | while read string; do echo "${string:0:1}${string: -1}"; done | paste -sd+ |bc
