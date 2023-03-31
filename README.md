# config_wjd

This is my custom linux configuration file repository.  Some of the stuff here is still 
useful, especially on ubuntu... but now that I'm using nixos, my config setup procedure
relies on fewer of these custom dot files.  Below describes (a part of) my setup procedure.

TODO: write a setup.sh script that will do a headless nixos/zsh/ohmyzsh/starship/doom 
custom setup.   

## Install customized fork of oh-my-zsh

1. Clone the (custom fork of the) oh-my-zsh repository into the `~/.config` directory.

   ```sh
   git clone git@github.com:williamdemeo/ohmyzsh.git ~/.config/oh-my-zsh
   ```

2. Backup your existing `~/.zshrc` file and replace it with a link to the custom template.

   ```sh
   cp ~/.zshrc ~/.zshrc.orig
   ln -s ~/.config/oh-my-zsh/templates/zshrc.wjd-template ~/.zshrc
   ```

3. Change your default shell

   ```sh
   chsh -s $(which zsh)
   ```

   (You must log out from your user session and log back in to see this change.)
   
## Install a customized fork of doom emacs 

``` sh
git clone --depth 1 git@github.com:williamdemeo/doomemacs.git ~/.config/emacs
~/.config/emacs/bin/doom install
doom sync
doom doctor   # check that everything is okay
```




