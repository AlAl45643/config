#! /bin/bash
mkdir bin
cd bin

# setup C#
wget https://github.com/SofusA/csharp-language-server/releases/download/5.1.0-1.25501.2/csharp-language-server-x86_64-unknown-linux-gnu.tar.gz
tar -xf csharp-language-server-x86_64-unknown-linux-gnu.tar.gz

wget https://github.com/Samsung/netcoredbg/releases/download/3.1.2-1054/netcoredbg-linux-amd64.tar.gz
tar -xf netcoredbg-linux-amd64.tar.gz
mv ./netcoredbg netcored
sudo mv ./netcored/* ./

wget https://dot.net/v1/dotnet-install.sh -O dotnet-install.sh
chmod +x ./dotnet-install.sh
./dotnet-install.sh --version latest --channel STS
cat >>~/.bashrc <<EOL
export DOTNET_ROOT=\$HOME/.dotnet
export PATH=\$PATH:\$DOTNET_ROOT:\$DOTNET_ROOT/tools
EOL

# setup LaTeX
wget https://github.com/latex-lsp/texlab/releases/download/v5.23.1/texlab-x86_64-linux.tar.gz
tar -xf texlab-x86_64-linux.tar.gz

# setup python
sudo dnf install pip
pip install "python-lsp-server[all]"
pip install "debugpy"

# setup vterm
sudo dnf install cmake libtool libvterm

cat >>~/.bashrc <<EOL
vterm_printf() {
    if [ -n "\$TMUX" ] \\
        && { [ "\${TERM%%-*}" = "tmux" ] \\
            || [ "\${TERM%%-*}" = "screen" ]; }; then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\\\" "\$1"
    elif [ "\${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\\\" "\$1"
    else
        printf "\e]%s\e\\\\" "\$1"
    fi
}
vterm_prompt_end(){
    vterm_printf "51;A\$(whoami)@\$(hostname):\$(pwd)"
}
PS1=\$PS1'\[\$(vterm_prompt_end)\]'
EOL

# install jetbrains mono font
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/JetBrains/JetBrainsMono/master/install_manual.sh)"

echo "Restart your computer."

