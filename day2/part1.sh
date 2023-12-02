function blue_fails
{
    echo $1 | sed -re 's/[0-9][0-9]* red//g' |sed -re 's/[0-9][0-9]* green//g' | cut -c 8- | tr -cd "0-9\\n[:space:]" | while read line; do
        for n in $(echo $line); do
            if [[ $n -gt 14 ]]; then
                echo "fail"
            fi
        done
    done
}

function green_fails
{
    echo $1 | sed -re 's/[0-9][0-9]* red//g' |sed -re 's/[0-9][0-9]* blue//g' | cut -c 8- | tr -cd "0-9\\n[:space:]" | while read line; do
        for n in $(echo $line); do
            if [[ $n -gt 13 ]]; then
                echo "fail"
            fi
        done
    done
}

function red_fails
{
    echo $1 | sed -re 's/[0-9][0-9]* blue//g' |sed -re 's/[0-9][0-9]* green//g' | cut -c 8- | tr -cd "0-9\\n[:space:]" | while read line; do
        pepe=1
        for n in $(echo $line); do
            if [[ $n -gt 12 ]]; then
                echo fail
            fi
        done
    done
}


i=0

cat input-test | while read line; do
    i=$((i+1))
    red=$(red_fails "$line")
    green=$(green_fails "$line")
    blue=$(blue_fails "$line")
    [ -z "$red" ] && [ -z "$green" ] &&  [ -z "$blue" ] && echo $i
done | paste -sd+ - | bc
