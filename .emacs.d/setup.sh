#! /bin/bash

mkdir langservers/
cd langservers
mkdir csharp
cd csharp
mkdir omnisharp
cd omnisharp
wget https://github.com/OmniSharp/omnisharp-roslyn/releases/download/v1.39.13/omnisharp-linux-x64-net6.0.zip
unzip omnisharp-linux-x64-net6.0.zip
cd ../../../
mkdir debug-adapters/
cd debug-adapters
wget https://github.com/Samsung/netcoredbg/releases/download/3.1.2-1054/netcoredbg-linux-amd64.tar.gz
tar -xf netcoredbg-linux-amd64.tar.gz
cd ../../../
wget https://download.jetbrains.com/fonts/JetBrainsMono-2.304.zip
mv JetBrainsMono-2.304.zip ~/.local/share/fonts/
cd ~/.local/share/fonts/
unzip JetBrainsMono-2.304.zip
