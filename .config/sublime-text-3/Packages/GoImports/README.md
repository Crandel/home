GoImports Sublime3 Plugin
=========================
Sublime3 Plugin for integrating goimports (https://github.com/bradfitz/goimports) to your favorite editor.

# Usage
  0. Clone the repository to your plugins folder.

        git clone https://github.com/ticcky/GoImports.git ~/.config/sublime-text-3/Packages/GoImports  # for Linux

  1. Specify the path to the goimports binary in `GoImports.sublime-settings`.

        {
            "goimports_bin_path" : "/usr/local/bin/goimports"
        }

  2. Add to your sublime keys the following command and whenever you 
    want to run goimports hit F4. It will format/automatically adjust imports in the current go file.

        { "keys": ["f4"], "command": "go_imports" },

  3. Roll ;)
