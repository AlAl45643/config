#! /bin/bash

# setup C#
mkdir langservers/
cd langservers
mkdir csharp
cd csharp
mkdir omnisharp
cd omnisharp
wget https://github.com/OmniSharp/omnisharp-roslyn/releases/download/v1.39.13/omnisharp-linux-x64-net6.0.zip
unzip omnisharp-linux-x64-net6.0.zip
chmod +x ./OmniSharp
cd ../../../
mkdir debug-adapters/
cd debug-adapters
wget https://github.com/Samsung/netcoredbg/releases/download/3.1.2-1054/netcoredbg-linux-amd64.tar.gz
tar -xf netcoredbg-linux-amd64.tar.gz
cd ../
wget https://dot.net/v1/dotnet-install.sh -O dotnet-install.sh
chmod +x ./dotnet-install.sh
./dotnet-install.sh --version latest
echo 'export DOTNET_ROOT=$HOME/.dotnet' >> ~/.bashrc 
echo 'export PATH=$PATH:$DOTNET_ROOT:$DOTNET_ROOT/tools' >> ~/.bashrc 

# setup python
pip install "python-lsp-server[all]"
