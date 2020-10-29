clear

# Remove hushdragon file if it exists
if [ -f ~/.hushdragon ]; then
    rm ~/.hushdragon
fi

# Counter for file names
counter=1

# Array of dragon frames
declare -a dragons

cd ~/dragon

# Number of rows in terminal window
rowcount=$(tput lines)

# Loop over each dragon file
for f in dragon*
do
    # Number of spaces to insert before each line
    x=$(($(tput cols) / 2 - 45))

    # Dragon with space inserted before each line
    dragon=$(awk -v x=$x '{printf "%" x "s%s\n", "", $0}' "dragon$counter")

    # Add dragon to dragon array
    dragons+=("$dragon")

    ((counter++))
done

# Loop over each dragon in dragon array
for dragon in $"${dragons[@]}"
do
    clear

    # If hushdragon file is present (early exit with Cmd+i), exit script
    if [ -f ~/.hushdragon ]; then exit; fi

    # First row padding
    repeat $((($rowcount-43)/2)) {
        echo
    }

    # Print dragon
    echo $dragon

    # Last row padding
    repeat $((($rowcount-43)/2)) {
        echo
    }

    # Wait before next frame
    sleep .05s
done

# Wait for Return keypress to clear
read
clear
printf '\e[3J'
