voxdir=~/Downloads/vox.txt

while true
do
    if [ -f $voxdir ]; then
        osascript -e "set Volume $(head -c 1 <$voxdir)"
        content="$(<$voxdir)"
        say "${content#?}"
        rm $voxdir
    fi
    sleep 5s
done
