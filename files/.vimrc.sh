# Create vimrc copy
cp ~/.vimrc ~/.vimrc_copy

# Edit vimrc
nvim ~/.vimrc

# Send vimrc backup if there is a difference between the old and new versions of the vimrc
if [ "$1" != '--hard' ]
then
    if [ -z "$(diff ~/.vimrc ~/.vimrc_copy)" ]
    then
        echo "No changes made."
    else
        echo "Changes made."
        python ~/.vimrc.py
    fi
else
    python ~/.vimrc.py
fi

# Delete copy
rm ~/.vimrc_copy
