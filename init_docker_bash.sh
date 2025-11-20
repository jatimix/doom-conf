o### COPY PASTE TO HAVE A BETTER DOCKER SHELL ##
export PS1='[\u@\h \W]\$ '
alias ls="ls --color=auto"
alias ll="ls -lh --color=auto"
alias la="ls -la --color=auto"
alias l="ls -l --color=auto"
alias grep="grep --color=auto"
alias cb="conan build ."
alias cbd="conan build . -s \"&:build_type=Debug\""
alias install_clangd="yum -y install clangd"
export CMAKE_EXPORT_COMPILE_COMMANDS=1
PS1=" λ \[\e[1;32m\][\W]> \[\e[0m\]"

## Conan setup ##
conan remote remove conancenter
conan remote add nexguard https://scm.nexguard.net/conan2
conan profile detect
sed "s/gnu17/gnu23/" -i ~/.conan2/profiles/default

## GDB style
sed "s/function black b/function cyan b/" -i /usr/share/source-highlight/esc.style

## Ripgrep
curl -LO "https://github.com/BurntSushi/ripgrep/releases/download/14.1.1/ripgrep-14.1.1-x86_64-unknown-linux-musl.tar.gz"
tar -xvf ripgrep-14.1.1-x86_64-unknown-linux-musl.tar.gz
cp ./ripgrep-14.1.1-x86_64-unknown-linux-musl/rg /bin/
rm -rf ./ripgrep-14.1.1-x86_64-unknown-linux-musl*

## LSD
curl -LO "https://github.com/lsd-rs/lsd/releases/download/v1.1.5/lsd-v1.1.5-x86_64-unknown-linux-musl.tar.gz"
tar -xvf lsd-v1.1.5-x86_64-unknown-linux-musl.tar.gz
cp lsd-v1.1.5-x86_64-unknown-linux-musl/lsd /bin
rm -rf lsd*
alias ls=lsd

echo "BEWARE - automatically try to change to /home/sc !!!!"
echo "BEWARE - automatically try to change to /home/sc !!!!"
echo "BEWARE - automatically try to change to /home/sc !!!!"
cd /home/sc
## fish
# yum -y install dnf-plugins-core
# dnf -y config-manager --add-repo https://download.opensuse.org/repositories/shells:/fish:/release:/3/CentOS_8/shells:fish:release:3.repo
# dnf -y install fish
# fish

## startship
# dnf -y copr enable atim/starship
# dnf -y install starship
# echo "starship init fish | source" >> ~/.config/fish/config.fish
# exit

# echo alias grep=\"grep --color=auto\" >> ~/.config/fish/config.fish
# echo alias cb=\"conan build .\" >> ~/.config/fish/config.fish
# echo alias cbd=\"conan build . -s \"&:build_type=Debug\"\" >> ~/.config/fish/config.fish
# echo alias install_clangd=\"yum -y install clangd\" >> ~/.config/fish/config.fish
# echo alias ls=lsd >> ~/.config/fish/config.fish

# fish
#eof
