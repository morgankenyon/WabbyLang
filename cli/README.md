# Waux.Lang.Cli

This holds the CLI for Waux.Lang.

Please see the [Waux.Lang](https://github.com/morgankenyon/waux-lang) project for better instructions on how to setup on your local machine.

## Commands

* Generate new package - run `./pack.ps1`
* Install as local tool from source - `dotnet tool install --local --add-source .\bin\Release\ waux.lang.cli`
* Install as global tool from source - `dotnet tool install --global --add-source .\bin\Release\ waux.lang.cli`
  * If not running from `/cli` folder, update the `.\bin\Release` to wherever you *.nupkg is located.
* Install from nuget - `dotnet tool install --global Waux.Lang.Cli --version 0.0.1`
* Uninstall tool - `dotnet tool uninstall -g waux.lang.cli`