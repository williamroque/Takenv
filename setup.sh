rm -rf ~/.config
sudo chown -R $(whoami) files
sudo chmod -R +rwx files
sudo cp -R files/. ~
