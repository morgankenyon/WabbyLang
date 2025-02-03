# How I Perform a New Waux Release

Releasing library code is always tricky. Once a version is out in the wild, there's no real way to get it back. 

So here is what I do to try to cutdown on issues that may occur.

1. Create a release branch.
1. Make sure all unit tests pass.
1. Ensure `Waux.Lang.Cli` project is referencing the local `Wang.Lang` project and not pulling from nuget.
1. Pack and install CLI tool locally, run through all the examples programs and ensure they're working as expected.
1. Format code using `format.bat` located at the root of the project.
1. Run unit tests again, make sure everything passes.
1. Make sure versions in the [Waux.Lang.fsproj](./src/Waux.Lang.fsproj) and [Waux.Lang.Cli.fsproj](./cli/Waux.Lang.Cli.fsproj) files match.
1. Run the `pack.ps1` script in the `/src` directory.
1. This generates the nuget package for the `Waux.Lang` project.
1. Upload the *.nupkg and *.snupkg to nuget.
1. Wait for it to be indexed and available
1. Update the reference in the `Waux.Lang.Cli` project to pull in this new nuget.
1. Install CLI tool locally, ensure everything still works.
1. Run the `pack.ps1` script in the `cli` directory.
1. Upload the *.nupkg and *.snupkg to nuget.
1. Wait for it to be indexed, install locally and ensure everything is working.
1. Merge release branch back into main.
1. Tag release with the current version.
1. Create release in github.
1. Change the `Waux.Lang.Cli` project back to pulling in the local `Wang.Lang` and not pulling from nuget.