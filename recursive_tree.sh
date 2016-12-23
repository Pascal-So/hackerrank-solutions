function repeat {
    local n=$1
    local c=$2
    for ((i=0; i<$n; i++)); do
	echo -n $c
    done
}

function empty_line {
    local width=100
    repeat $width _
    echo ""
}

function get_block {
    local x=$1
    local y=$2

    # Returns: 
    # 0 - empty
    # r - fill right row
    # t - top right corner
    # b - bottom left diagonal
    # d - bottom left top right diagonal

    while [[ $y -gt 1 ]]; do
	x=$(( $x / 2 ))
	y=$(( $y / 2 ))
    done

    y=$(( $y % 4 ))

    if [[ $y -eq 3 ]]; then
	if [[ $x -eq 1 ]]; then
	    echo -n r
	else
	    echo -n 0
	fi
    else
	if [[ $x -eq 0 ]]; then
	    echo -n t
	elif [[ $x -eq 1 ]]; then
	    echo -n b
	elif [[ $x -eq 2 ]]; then
	    echo -n d
	else
	    echo -n 0
	fi
    fi
}

function get_x_y_s_in_block {
    local x=$1
    local y=$2

    # returns:
    # x y s
    # x, y are position in block
    # s is side length of block

    
}

function get_char_at_pos {
    local x=$1
    local y=$2

    # assumes that empty lines have been handled

    local block_type=$(get_block $x $y)

    if [[ $block_type -eq 0 ]]; then
	echo -n _
    else
	
    fi
}
