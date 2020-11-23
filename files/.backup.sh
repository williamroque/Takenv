cd

dirname="backup.$(date +%Y%m%d%H%M%S)"
mkdir $dirname

cp -r Documents $dirname/documents

mkdir $dirname/lists/
ls /Applications > $dirname/lists/applications.txt

zip -r $dirname.zip $dirname
mv $dirname.zip Desktop
rm -rf $dirname
