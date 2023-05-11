# config_wjd

This is my custom linux configuration file repository.  Some of the stuff here is still 
useful, especially on ubuntu... but now that I'm using nixos, my config setup procedure
relies on fewer of these custom dot files.  Below describes (a part of) my setup procedure.

TODO: write a setup.sh script that will do a headless nixos/zsh/ohmyzsh/starship/doom 
custom setup.   

## Install customized fork of oh-my-zsh

1. Clone the (custom fork of the) oh-my-zsh repository into the `~/.config` directory.

   ```shell
   git clone git@github.com:williamdemeo/ohmyzsh.git ~/.config/oh-my-zsh
   ```

2. Run the oh-my-zsh install script.

   ```shell
   ~/.config/oh-my-zsh/tools/install.sh
   ```
   
3. Ensure your `ZSH_CUSTOM` environment variable was set correctly.

   ```shell
   echo $ZSH_CUSTOM
   ```
   It should show `~/.config/oh-my-zsh/custom`, or similar.
   
   If not, then probably something went wrong above, but for now you could try to remedy this by invoking
   
   ```shell
   export ZSH_CUSTOM=~/.config/oh-my-zsh/custom
   ```

4. Add autosuggestions and syntax highlighting plugins.

   ```sh
   git clone https://github.com/zsh-users/zsh-autosuggestions ${ZSH_CUSTOM}/plugins/zsh-autosuggestions
   git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ${ZSH_CUSTOM}/plugins/zsh-syntax-highlighting
   ```

5. Backup your existing `~/.zshrc` file and replace it with a link to the custom template.

   ```sh
   cp ~/.zshrc ~/.zshrc.orig
   ln -s ~/.config/oh-my-zsh/templates/zshrc.wjd-template ~/.zshrc
   ```

6. Change your default shell

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


## Troubleshoting on Mac

For the above to work on Mac, you likely have to install some standard programs.

For example, do some or all of the following, as necessary.

```shell
brew install niv gh rg fd shellcheck rlwrap ag
brew install --cask gcs
brew install cmake npm nodejs nvm
brew install nix
nix-env -iA nixpkgs.haskellPackages.hoogle
```




