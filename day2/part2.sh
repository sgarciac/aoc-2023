function blue_max
{
    echo $1 | sed -re 's/[0-9][0-9]* red//g' |sed -re 's/[0-9][0-9]* green//g' | cut -c 8- | tr -cd "0-9\\n[:space:]" | while read line; do
        for i in $line; do
            echo $i
        done | sort -n | tail -n 1
    done
}

function red_max
{
    echo $1 | sed -re 's/[0-9][0-9]* blue//g' |sed -re 's/[0-9][0-9]* green//g' | cut -c 8- | tr -cd "0-9\\n[:space:]" | while read line; do
        for i in $line; do
            echo $i
        done | sort -n | tail -n 1
    done
}

function green_max
{
    echo $1 | sed -re 's/[0-9][0-9]* red//g' |sed -re 's/[0-9][0-9]* blue//g' | cut -c 8- | tr -cd "0-9\\n[:space:]" | while read line; do
        for i in $line; do
            echo $i
        done | sort -n | tail -n 1
    done
}


cat input-test | while read line; do
    b=$(blue_max "$line")
    g=$(green_max "$line")
    r=$(red_max "$line")
    echo "$r * $g * $b" | bc
done | paste -sd+ - | bc
