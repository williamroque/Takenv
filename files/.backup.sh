cd

dirname="backup.$(date +%Y%m%d%H%M%S)"
mkdir $dirname

mkdir $dirname/config-backup
cp .vimrc $dirname/config-backup
cp .zshrc $dirname/config-backup
cp .tmux.conf $dirname/config-backup
cp .notes.md $dirname/config-backup
cp .terminit.sh $dirname/config-backup
cp -r dragon $dirname/config-backup

mkdir $dirname/scripts
cp .backup.sh $dirname/scripts
cp .vimrc.py $dirname/scripts
cp .openas.sh $dirname/scripts

cp -r Documents $dirname/documents

mkdir $dirname/lists/
ls /Applications > $dirname/lists/applications.txt

zip -r $dirname.zip $dirname
mv $dirname.zip Desktop
rm -rf $dirname
